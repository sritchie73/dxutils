#' Upload a file or folder to a DNAnexus project
#'
#' Extends the `dx upload` command line tool to enable recursive upload of
#' folders and resumption of interrupted uploads. Unlike `dx upload`, assumes
#' the user does not want to create multiple versions of the same file when
#' uploading a file with the same name that already exists at the 'remote_path'.
#'
#' @details
#' When uploading folders, the directory tree is recreated at the destination
#' 'remote_path' location following unix conventions: if the path is a folder
#' ending in "/" the contents of the folder are uploaded, otherwise the folder
#' itself is uploaded at the destination 'remote_path'.
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

    # Determine destination location - if and only if remote path ends in a "/"
    # do we treat it as a folder we want to upload the local_path file to
    if (remote_path == ".") {
      remote_path <- basename(local_path)
    } else if (grepl("/$", remote_path)) {
      remote_path <- paste0(remote_path, basename(local_path))
    }

    # Find out if anything exists already at the target location
    location_metadata <- dx_get_metadata(remote_path)

    # Get information about the project at that location
    project_metadata <- dx_get_project_metadata(location_metadata$project)

    # Make sure we have at necessary permissions
    assert_dx_project_permissions(project_metadata, "CONTRIBUTE")

    # Defer to 'exists' argument if file already exists on the target location
    if (!(dx_type(location_metadata) %in% c("none", "folder"))) {
      if (exists == "error") {
        stop("Data already exists on DNAnexus at ", remote_path)
      } else if (exists == "skip") {
        return(NULL)
      } else if (exists == "replace") {
        if (dx_user_can_rm(project_metadata)) {
          msg <- suppressWarnings(system(sprintf("dx rm -rfa '%s' 2>&1", remote_path), intern=TRUE))
          if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx rm'
          cat(remote_path, "on DNAnexus deleted\n")
        } else {
          msg <- suppressWarnings(system(sprintf("dx mv '%s' %s:trash/ 2>&1", remote_path, location_metadata$project), intern=TRUE))
          if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx mv'
          if (grepl(":", remote_path)) {
            cat(sep="", remote_path, " on DNAnexus moved to ", location_metadata$project, ":/trash/\n")
          } else {
            cat(remote_path, "on DNAnexus moved to trash/\n")
          }
        }
      }
    }

    # Now upload the file - make sure we attach current the DNA nexus job ID as
    # the uploaded_by property
    if (Sys.getenv("DX_JOB_ID") != "") {
      file_id <- suppressWarnings(system(sprintf(
        "dx upload '%s' --destination '%s' --parents --brief --property uploaded_by=$DX_JOB_ID",
        local_path, remote_path
      ), intern=TRUE))
    } else {
      file_id <- suppressWarnings(system(sprintf(
        "dx upload '%s' --destination '%s' --parents --brief",
        local_path, remote_path
      ), intern=TRUE))
    }
    if (!is.null(attr(file_id, "status"))) stop(paste(file_id, collapse="\n")) # Something has gone wrong, we should be able to 'dx mv'
    cat(local_path, "uploaded to DNAnexus at", remote_path, "with file ID", file_id)

    return(NULL)
  } else {
    # We are uploading a folder
  }
}
