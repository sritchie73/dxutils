#' Download a file or folder from a DNA nexus project
#'
#' Extends the `dx download` command line tool with additional functionality
#' that enables it to optionally wait for incomplete or missing files to be
#' uploaded by other processes before initiating the download. For programmatic
#' convenience, the location of the download on the local machine is returned.
#'
#' @details
#' When downloading folders, the directory tree is recreated at the destination
#' 'local_path' following unix conventions: if the 'remote_path' is a folder
#' ending in "/" the contents of the folder are downloaded, otherwise the folder
#' itself is downloaded at the destination 'local_path'.
#'
#' @inheritParams remote_path
#' @param local_path location on the local machine to download the file or folder.
#' @param exists action to take if the file(s) being downloaded already exists
#'    on the local machine. One of "error", "overwrite", or "skip" (default).
#' @param missing action to take if the 'remote_path' is not found on DNAnexus.
#'    One of "error" (default), "skip", or "wait".
#' @param incomplete action to take if the 'remote_path' points to an incomplete
#'    file on DNAnexus not created by the current DNAnexus job. One of
#'    "error", "skip", or "wait" (default).
#' @param silent logical; if TRUE does not print message on completion of each
#'    file download.
#'
#' @returns The path on the local machine where the file or files were downloaded
#'    to, replacing the equivalent path given in the 'remote_path' argument.
#'
#' @export
dx_download <- function(remote_path, local_path=".", exists="skip", missing="error", incomplete="wait", silent=FALSE) {
  # Check for valid arguments
  stopifnot(length(remote_path) == 1 && is.character(remote_path) && !is.na(remote_path))
  stopifnot(length(local_path) == 1 && is.character(local_path) && !is.na(local_path))
  stopifnot(length(exists) == 1 && exists %in% c("error", "overwrite", "skip"))
  stopifnot(length(missing) == 1 && missing %in% c("error", "skip", "wait"))
  stopifnot(length(incomplete) == 1 && incomplete %in% c("error", "skip", "wait"))
  stopifnot(length(silent) == 1 && is.logical(silent) && !is.na(silent))

  # Get the absolute format of the remote_path - particularly important for
  # bypassing container projects on cloud workstations
  normalized_remote_path <- dx_normalize_path(remote_path)

  # Enter while TRUE loop to handle conditions where we may have to wait
  while (TRUE) {
    # Get information about the project (i.e. whether we can even connect to
    # it from this job)
    project_metadata <- dx_get_project_metadata(normalized_remote_path)

    # Get information about whatever is stored at the target location on
    # DNAnexus (if anything)
    metadata <- dx_get_metadata(normalized_remote_path)

    # Determine if its a file, folder, or points to something that doesn't exist
    type <- dx_type(metadata)

    if (length(type) == 1 && type == "none") {
      # If the nothing exists (yet) at 'remote_path', either throw an error
      # if 'missing' is set to "error", return 'NULL' if 'missing' is set to
      # "skip", or wait for 10s before checking again if 'missing' is set to
      # "wait".
      if (missing == "error") {
        throw_file_not_exists_error(remote_path)
      } else if (missing == "skip") {
        return(invisible(NULL))
      } else if (missing == "wait") {
        assert_dx_project_permissions(project_metadata, "VIEW")
        cat(sep="", "'", remote_path, "' on DNAnexus does not exist yet, waiting 10s...\n")
        Sys.sleep(10)
        next
      }
    } else if (length(type) > 1 || type != "folder") {
      # 'remote_path' points to a single file/object

      # Extract the state of the file or object
      state <- dx_state(metadata)

      # Check that we don't have multiple files with the same name
      if (length(state) > 1) {
        stop("Multiple files with same file path found at '", remote_path, "' on DNAnexus:\n",
             paste(paste0("    '", remote_path, "' : '", metadata$id, "'"), collapse="\n"))
      }

      # If the file is in the 'open' state, defer to the 'incomplete' setting
      if (state == "open") {
        if (incomplete == "error") {
          stop("'", remote_path, "' is an incomplete file on DNAnexus still in the process of being uploaded by another process")
        } else if (incomplete == "skip") {
          return(invisible(NULL))
        } else if (incomplete == "wait") {
          cat(sep="", "'", remote_path, "' is an incomplete file on DNAnexus, waiting 10s for upload to finish before trying again...\n")
          Sys.sleep(10)
          next
        }
      }

      # If the file is in the 'closing' state, always wait for DNAnexus to
      # finish finalizing the file before downloading
      if (state == "closing") {
        cat(sep="'", remote_path, "' on DNAnexus is in the process of closing, waiting 10s for DNAnexus to finalize the file before trying again...\n")
        Sys.sleep(10)
        next
      }

      # File is ok to download, now check the target local_path to make sure
      # we're not overwriting an existing file unless intended
      if (dir.exists(local_path) || grepl("/$", local_path))  {
        if (local_path == ".") {
          local_path <- metadata$name
        } else if (grepl("/$", local_path)) {
          local_path <- sprintf("%s%s", local_path, metadata$name)
        } else {
          local_path <- sprintf("%s/%s", local_path, metadata$name)
        }
      }
      local_path_exists <- file.exists(local_path)

      if (local_path_exists) {
        if (exists == "error") {
          if (file.exists(local_path) && !dir.exists(local_path) && basename(local_path) == metadata$name) {
            # 'local_path' points to a file with the same name as the remote file
            stop("Error downloading '", remote_path, "' from DNAnexus: a file already exists on local machine at '", local_path, "'")
          } else if (file.exists(local_path) && dir.exists(local_path)) {
            # 'local_path' points to a folder that contains a file with the same name as the remote file
            if (!grepl("/$", local_path)) local_path <- paste0(local_path, "/")
            stop("Error downloading '", remote_path, basename(local_path), "' from DNAnexus: a file already exists on local machine at '", local_path, "'")
          } else {
            # 'local_path' points to a file with a different name to the remote file
            stop("Error downloading '", remote_path, "' to '", local_path, "' from DNAnexus: file already exists on local machine")
          }
        } else if (exists == "skip") {
          if (!silent) {
            if (file.exists(local_path) && !dir.exists(local_path) && basename(local_path) == metadata$name) {
              # 'local_path' points to a file with the same name as the remote file
              cat(sep="", "'", local_path, "' already exists on local machine, skipping download of '", remote_path, "' from DNAnexus\n")
            } else if (file.exists(local_path) && dir.exists(local_path)) {
              # 'local_path' points to a folder that contains a file with the same name as the remote file
              if (!grepl("/$", local_path)) local_path <- paste0(local_path, "/")
              cat(sep="", "'", local_path, "' already exists on local machine, skipping download of '", remote_path, basename(local_path), "' from DNAnexus\n")
            } else {
              # 'local_path' points to a file with a different name to the remote file
              cat(sep="", "'", local_path, "' already exists on local machine, skipping download of '", remote_path, "' from DNAnexus to this file path\n")
            }
          }
        }
      }

      # At this point, we are now ok to download the file if we need to (we
      # might not need to, i.e. if local_path_exists && exists == "skip")
      if (!local_path_exists || exists == "overwrite") {

        # Before doing so, we need to make sure any directories on the local
        # machine that need to exist are created
        if (grepl("/$", local_path)) {
          msg <- suppressWarnings(system(sprintf("mkdir -p '%s' 2>&1", local_path), intern=TRUE))
          if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # e.g. permission denied on local machine
        } else {
          msg <- suppressWarnings(system(sprintf("mkdir -p '%s' 2>&1", dirname(local_path)), intern=TRUE))
          if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # e.g. permission denied on local machine
        }

        # Download the file
        msg <- suppressWarnings(system(sprintf("dx download -f '%s' -o '%s' 2>&1", normalized_remote_path, local_path), intern=TRUE))
        if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))

        # Print out success message
        if (!silent) {
          cat(sep="", "'", remote_path, "' on DNAnexus downloaded to local machine at '", local_path, "'\n")
        }
      }

      # Determine path of the downloaded file to return as a string
      if (dir.exists(local_path)) {
        if (grepl("/$", local_path)) {
          return(sprintf("%s%s", local_path, metadata$name))
        } else {
          return(sprintf("%s/%s", local_path, metadata$name))
        }
      } else {
        return(local_path)
      }

    } else {
      # 'remote_path' is a folder to download

      # Get information about all the files at the remote_path
      file_list_metadata <- suppressWarnings(system(sprintf("dx find data --path '%s' --json 2>&1", normalized_remote_path), intern=TRUE))
      if (!is.null(attr(file_list_metadata, "status"))) stop(paste(file_list_metadata, collapse="\n"))
      file_list_metadata <- fromJSON(file_list_metadata)

      # If there are now files, download the directory structure regardless
      if (length(file_list_metadata) > 0) {

        # Get the absolute remote path of each file
        file_list_metadata$normalized_remote_path <- paste0(
          file_list_metadata$project, ":",
          gsub("^/$", "", file_list_metadata$describe$folder), "/",
          file_list_metadata$describe$name
        )

        # Get the relative remote paths
        file_list_metadata$remote_path <- paste0(
          remote_path,
          gsub(normalized_remote_path, "", file_list_metadata$normalized_remote_path, fixed=TRUE)
        )

        # Determine location to download each file to
        if (grepl("/$", remote_path)) {
          file_list_metadata$local_path <- paste0(
            gsub("/$", "", local_path), "/",
            gsub(paste0("^", remote_path), "", file_list_metadata$remote_path)
          )
        } else {
          file_list_metadata$local_path <- paste0(
            gsub("/$", "", local_path), "/",
            basename(remote_path),
            gsub(paste0("^", remote_path), "", file_list_metadata$remote_path)
          )
        }

        # Check for duplicate files that mean we can't proceed
        dups <- unique(file_list_metadata$local_path[duplicated(file_list_metadata$local_path)])
        if (length(dups) > 0) {
          dups <- file_list_metadata[file_list_metadata$local_path %in% dups,]
          stop("Multiple files with the same file path found at '", remote_path, "' on DNAnexus:\n",
               paste(paste0("    '", dups$remote_path, "' : '", dups$id, "'"), collapse="\n"))
        }

        # Check if any are in the open state, and if they are, check whether any
        # have a "uploaded_by" property matching the current DNAnexus job ID.
        # If there are matches, delete those files, remove them from the download
        # list, and continue
        if (
          any(file_list_metadata$describe$state == "open") &&
          Sys.getenv("DX_JOB_ID") != ""
        ) {
          for (fid in which(file_list_metadata$describe$state == "open")) {
            file_metadata <- dx_get_metadata(file_list_metadata$id[fid])
            new_state <- dx_state(file_metadata)
            file_list_metadata$describe$state[fid] <- new_state
          }
          file_list_metadata <- file_list_metadata[file_list_metadata$describe$state != "none",]
        }

        # If any files remain in the open state, defer to the 'incomplete' setting
        if (
          any(file_list_metadata$describe$state == "open")
        ) {
          incomplete_files <- file_list_metadata[file_list_metadata$describe$state == "open", "normalized_remote_path"]

          if (incomplete == "error") {
            stop("'", remote_path, "' on DNAnexus contains ", length(incomplete_files),
                 " incomplete files:\n", paste(paste0("    '", incomplete_files, "'"), collapse="\n"))
          } else if (incomplete == "skip") {
            if (!silent) {
              cat(sep="", "Skipping download of ", length(incomplete), " incomplete files on DNAnexus at '",
                  remote_path, "':", paste(paste0("    '", incomplete_files, "'"), collapse="\n"))
            }
            file_list_metadata <- file_list_metadata[file_list_metadata$describe$state != "open",]
          } else if (incomplete == "wait") {
            cat(sep="", "'", remote_path, "' on DNAnexus contains ", length(incomplete_files),
                " incomplete files:\n", paste(paste0("    '", incomplete_files, "'"), collapse="\n"),
                "Waiting 10s for files to finish uploading before trying again...\n")
            Sys.sleep(10)
            next
          }
        }

        # If any files are closing, wait for them to finish
        if (
          any(file_list_metadata$describe$state == "closing")
        ) {
          closing_files <- file_list_metadata[file_list_metadata$describe$state == "closing", "normalized_remote_path"]

          cat(sep="", "'", remote_path, "' on DNAnexus contains ", length(closing_files),
              " files in the process of closing:\n", paste(paste0("    '", closing_files, "'"), collapse="\n"),
              "Waiting 10s for DNAnexus to finalize the files before trying again...\n")
          Sys.sleep(10)
          next
        }

        # Check whether any of these files already exist, in accordance with the
        # 'exists' argument
        file_list_metadata$exists <- file.exists(file_list_metadata$local_path)

        if (any(file_list_metadata$exists)) {
          files_exist <- file_list_metadata[file_list_metadata$exists, "normalized_remote_path"]

          if (exists == "error") {
            stop(length(files_exist), " files from '", remote_path,
              "' on DNAnexus already exist on the local machine at '", gsub("/$", "", local_path),
              "'/:", paste(paste("    '", files_exist, file_list_metadata$local_path), collapse="\n"))

          } else if (exists == "skip") {
            cat(paste(paste(files_exist,
              "on DNAnexus skipped as a file on the local machine already exists at",
              file_list_metadata$local_path), collapse="\n"), "\n")

            file_list_metadata <- file_list_metadata[!file_list_metadata$exists,]
          }
        }
      }

      # If we have reached this point without error we can recreate the remote
      # directory tree in the designated local_path
      dx_clone_tree(normalized_remote_path, local_path, silent=silent)

      # Download each of the files
      for (ii in seq_len(nrow(file_list_metadata))) {
        msg <- suppressWarnings(system(sprintf(
            "dx download -f '%s' -o '%s' 2>&1",
            file_list_metadata$normalized_remote_path[ii],
            file_list_metadata$local_path[ii]
          ), intern=TRUE))
        if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))

        # Print out success message
        if (!silent) {
          cat(sep="", "'", file_list_metadata$remote_path[ii],
            "' on DNAnexus downloaded to local machine at '",
            file_list_metadata$local_path[ii], "'\n")
        }
      }

      # Return the local_path
      if (grepl("/$", remote_path)) {
        return(local_path)
      } else {
        if (grepl("/$", local_path)) {
          return(sprintf("%s%s", local_path, basename(remote_path)))
        } else {
          return(sprintf("%s/%s", local_path, basename(remote_path)))
        }
      }
    }
  }
}

#' Clone the directory structure from a DNAnexus folder
#'
#' @details
#' Recursively traverses the remote folder location given by `remote_path` to
#' recreate the directory tree at the given `local_path`. The `remote_subdir`
#' argument is used by the recursive calls to the `dx_clone_tree` function and
#' should be omitted when called by other functions.
#'
#' The directory tree is recreated at the destination `local_path` following
#' unix conventions: if the `remote_path` is a folder ending in "/" the contents
#' of the folder are downloaded, otherwise the folder itself is downloaded at
#' the destination `local_path`.
#'
#' @inheritParams remote_path
#' @param local_path location on the local machine to download the file or folder.
#' @param remote_subdir subdirectory relative to the remote_path (see Details)
#' @param silent logical; if TRUE does not print message when creating each
#'   folder
#'
#' @returns NULL
dx_clone_tree <- function(remote_path, local_path, remote_subdir, silent) {
  local_path <- gsub("/$", "", local_path) # means we don't get // in local tree message print outs

  # Create the remote directory at the current depth of the remote tree in the
  # respective location on the local path
  if (missing(remote_subdir)) {
    if (grepl("/$", remote_path)) {
      newdir <- local_path
    } else {
      newdir <- sprintf("%s/%s", local_path, basename(remote_path))
    }
  } else {
    if (grepl("/$", remote_path)) {
      newdir <- sprintf("%s/%s", local_path, remote_subdir)
    } else {
      newdir <- sprintf("%s/%s/%s", local_path, basename(remote_path), remote_subdir)
    }
  }
  if (!dir.exists(newdir)) {
    msg <- suppressWarnings(system(sprintf("mkdir -p '%s'", newdir), intern=TRUE))
    if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    if (!silent) {
      cat(sep="", "Created missing local directory '", newdir, "'\n")
    }
  }

  # Get list of subdirectories at current depth of the remote tree
  if (missing(remote_subdir)) {
    subdirs <- suppressWarnings(system(sprintf("dx ls '%s' --folders", remote_path), intern=TRUE))
    if (!is.null(attr(subdirs, "status"))) stop(paste(msg, collapse="\n"))
  } else {
    subdirs <- suppressWarnings(system(sprintf("dx ls '%s/%s' --folders", remote_path, remote_subdir), intern=TRUE))
    if (!is.null(attr(subdirs, "status"))) stop(paste(msg, collapse="\n"))
  }

  # Recurse into them with this function
  for (sd in subdirs) {
    if (missing(remote_subdir)) {
      dx_clone_tree(remote_path, local_path, sd, silent)
    } else {
      dx_clone_tree(remote_path, local_path, sprintf("%s/%s", remote_subdir, sd), silent)
    }
  }
}
