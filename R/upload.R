#' Upload a file or folder to a DNAnexus project
#'
#' Extends the `dx upload` command line tool to enable recursive upload of
#' folders and resumption of interrupted uploads. Unlike `dx upload`, assumes
#' the user does not want to create multiple versions of the same file when
#' uploading a file with the same name that already exists at the 'remote_path'.
#'
#' @details
#' When uploading folders, the directory tree is recreated at the destination
#' 'remote_path' location following unix conventions: if the 'local_path' is a
#' folder ending in "/" the contents of the folder are uploaded otherwise the
#' folder itself is uploaded at the destination 'remote_path'.
#'
#' @inherit file_states
#'
#' @param local_path path of the file or folder on the local machine to upload.
#' @inheritParams remote_path
#' @param exists action to take if the file being uploaded already exists on
#'    DNAnexus. One of "error", "replace" (default), or "skip".
#' @param silent logical; if TRUE does not print message on completion of each
#'    file upload.
#'
#' @returns NULL
#'
#' @export
dx_upload <- function(local_path, remote_path=".", exists="replace", silent=FALSE) {
  # Check for valid arguments
  stopifnot(length(local_path) == 1 && is.character(local_path) && !is.na(local_path))
  stopifnot(length(remote_path) == 1 && is.character(remote_path) && !is.na(remote_path))
  stopifnot(length(exists) == 1 && exists %in% c("error", "replace", "skip"))
  stopifnot(length(silent) == 1 && is.logical(silent) && !is.na(silent))

  # Check that we've provided a valid file (or folder) to upload
  if (!file.exists(local_path)) {
    stop("could not upload ", local_path, " to DNAnexus, file/folder not found")
  }

  if (!dir.exists(local_path)) {
    # We are uploading a file

    # Determine destination location - if remote path ends in a "/" or ":"
    # we treat it as a folder we want to upload the local_path file to
    if (remote_path == ".") {
      remote_path <- basename(local_path)
    } else if (grepl("/$", remote_path) || grepl("(?<!\\\\):$", remote_path, perl = TRUE)) {
      remote_path <- paste0(remote_path, basename(local_path))
    }

    # Convert remote_path to an absolute path, primarily so we can bypass any
    # wrapping job container to interface with project storage on running jobs
    normalized_remote_path <- dx_normalize_path(remote_path)

    # Find out if anything exists already at the target location
    location_metadata <- dx_get_metadata(normalized_remote_path)
    location_type <- dx_type(location_metadata)

    # Get information about the project at that location
    project_metadata <- dx_get_project_metadata(location_metadata$project[1]) # [1] in case duplicate files

    # Make sure we have at necessary permissions
    assert_dx_project_permissions(project_metadata, "CONTRIBUTE")

    # Defer to 'exists' argument if file already exists on the target location
    if (length(location_type) > 1 || !(dx_type(location_metadata) %in% c("none", "folder"))) {
      if (exists == "error") {
        stop("Data already exists on DNAnexus at ", remote_path)
      } else if (exists == "skip") {
        return(invisible(NULL))
      } else if (exists == "replace") {
        if (dx_user_can_rm(project_metadata)) {
          msg <- suppressWarnings(system(sprintf("dx rm -rfa '%s' 2>&1", remote_path), intern=TRUE))
          if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx rm'
          if (!silent) {
            for (fid in location_metadata$id) {
              cat(sep="", remote_path, " on DNA nexus deleted (file ID: ", fid, ")\n")
            }
          }
        } else {
          for (ii in seq_along(location_metadata$id)) {
            if (length(location_metadata$id) == 1) {
              msg <- suppressWarnings(system(sprintf(
                "dx mv '%s' %s:trash/ 2>&1",
                remote_path, location_metadata$project
              ), intern=TRUE))
            } else {
              msg <- suppressWarnings(system(sprintf(
                "dx mv '%s:%s' %s:trash/ 2>&1",
                location_metadata$project[ii], location_metadata$id[ii],
                location_metadata$project[ii]
              ), intern=TRUE))
            }
            if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx mv'
            if (!silent) {
              if (grepl(":", remote_path)) {
                cat(sep="", remote_path, " (file ID: ", location_metadata$id[ii],
                    ") moved to ", location_metadata$project[ii], ":/trash/\n")
              } else {
                cat(sep="", remote_path, " (file ID: ", location_metadata$id[ii],
                    ") moved to trash/ on DNAnexus project\n")
              }
            }
          }
        }
      }
    }

    # Now upload the file - make sure we attach current the DNA nexus job ID as
    # the uploaded_by property
    if (dx_is_job()) {
      file_id <- suppressWarnings(system(sprintf(
        "dx upload '%s' --destination '%s' --parents --brief --property uploaded_by=$DX_JOB_ID",
        local_path, normalized_remote_path
      ), intern=TRUE))
    } else {
      file_id <- suppressWarnings(system(sprintf(
        "dx upload '%s' --destination '%s' --parents --brief",
        local_path, normalized_remote_path
      ), intern=TRUE))
    }
    if (!is.null(attr(file_id, "status"))) stop(paste(file_id, collapse="\n")) # Something has gone wrong, e.g. connection to server
    if (!silent) {
      cat(sep="", "'", local_path, "' uploaded to DNAnexus at '", remote_path, "' with file ID ", file_id, "\n")
    }

    return(invisible(NULL))
  } else {
    # We are uploading a folder

    # Get a listing of all the files in that folder
    file_list <- data.frame(
      local_path=list.files(
        path=gsub("/$", "", local_path), recursive=TRUE, full.names=TRUE
      )
    )

    # Determine the relative location on DNAnexus we plan to upload to
    if (grepl("/$", local_path)) {
      file_list$remote_path <- paste0(
        gsub("/$", "", remote_path), "/",
        gsub(paste0("^", local_path), "", file_list$local_path)
      )
    } else {
      file_list$remote_path <- paste0(
        gsub("/$", "", remote_path), "/", file_list$local_path
      )
    }
    file_list$remote_path <- gsub("^\\./", "", file_list$remote_path)

    # Also get the absolute location, in case we're working in a DNAnexus job
    # container
    file_list$abs_remote_path <- sapply(file_list$remote_path, dx_normalize_path)

    # Iterate through files, checking whether they exist already on DNA nexus
    # and following the 'exists' for what to do if encountered. Uploads as we
    # go so that if exists="replace" the upload happens directly after the
    # 'dx rm'
    for (ii in seq_len(nrow(file_list))) {
      # Look up the file using dx_get_metadata - and also delete any incomplete
      # uploads initated by the current DNAnexus job
      remote_metadata <- dx_get_metadata(file_list$abs_remote_path[ii])

      # Get project metadata and check we have CONTRIBUTE permissions, this
      # only needs to be done once not for every file
      if (ii == 1) {
        project_metadata <- dx_get_project_metadata(remote_metadata$project[1])
        assert_dx_project_permissions(project_metadata, "CONTRIBUTE")
      }

      # Handle existing files based on the "exists" argument
      remote_type <- dx_type(remote_metadata)
      if (length(remote_type) > 1 || !(remote_type %in% c("none", "folder"))) {
        if (exists == "error") {
          # Exit at first error, no need to check every file
          stop("Data already exists on DNAnexus at '", file_list$remote_path[ii], "'")
        } else if (exists == "skip") {
          if (!silent) {
            cat(sep="", "Skipping upload of '", file_list$local_path[ii], "', data already exists on DNAnexus at '",
                file_list$remote_path[ii], "'\n")
          }
          next
        } else if (exists == "replace") {

          if (dx_user_can_rm(project_metadata)) {
            msg <- suppressWarnings(system(sprintf("dx rm -rfa '%s' 2>&1", file_list$abs_remote_path[ii]), intern=TRUE))
            if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx rm'
            if (!silent) {
              for (fid in remote_metadata$id) {
                cat(sep="", "'", file_list$remote_path[ii], "' on DNA nexus deleted (file ID: '", fid, "')\n")
              }
            }
          } else {
            # Set up a trash/ folder in the project if needed
            msg <- suppressWarnings(system(sprintf("dx mkdir -p %s:trash 2>&1", project_metadata$id), intern=TRUE))
            if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx mv'

            for (jj in seq_along(remote_metadata$id)) {
              msg <- suppressWarnings(system(sprintf(
                "dx mv '%s' %s:trash/ 2>&1",
                remote_metadata$id[jj], project_metadata$id
              ), intern=TRUE))

              if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx mv'
              if (!silent) {
                if (dx_path_contains_project(file_list$remote_path[ii])) {
                  cat(sep="", "'", file_list$remote_path[ii], "' (file ID: '", remote_metadata$id[jj],
                      "') moved to '", project_metadata$id, ":/trash/'\n")
                } else {
                  cat(sep="", "'", file_list$remote_path[ii], "' (file ID: '", remote_metadata$id[jj],
                      "') moved to 'trash/' on DNAnexus project\n")
                }
              }
            }
          }
        }
      }

      # If we have reached this point, we can now upload the files
      if (dx_is_job()) {
        file_id <- suppressWarnings(system(sprintf(
          "dx upload '%s' --destination '%s' --parents --brief --property uploaded_by=$DX_JOB_ID",
          file_list$local_path[ii], file_list$abs_remote_path[ii]
        ), intern=TRUE))
      } else {
        file_id <- suppressWarnings(system(sprintf(
          "dx upload '%s' --destination '%s' --parents --brief",
          file_list$local_path[ii], file_list$abs_remote_path[ii]
        ), intern=TRUE))
      }
      if (!is.null(attr(file_id, "status"))) stop(paste(file_id, collapse="\n")) # Something has gone wrong, we should be able to 'dx mv'
      if (!silent) {
        cat(sep="", "'", file_list$local_path[ii], "' uploaded to DNAnexus at '", file_list$remote_path[ii], "' with file ID '", file_id, "'\n")
      }
    }

    # All done! :)
    return(invisible(NULL))
  }
}
