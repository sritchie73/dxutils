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

  # Enter while TRUE loop to handle conditions where we may have to wait
  while (TRUE) {
    # Get information about whatever is stored at the target location on
    # DNAnexus (if anything)
    metadata <- dx_get_metadata(remote_path)

    # Determine if its a file, folder, or points to something that doesn't exist
    type <- dx_type(metadata)

    if (type == "none") {
      # If the nothing exists (yet) at 'remote_path', either throw an error
      # if 'missing' is set to "error", return 'NULL' if 'missing' is set to
      # "skip", or wait for 10s before checking again if 'missing' is set to
      # "wait".
      if (missing == "error") {
        if (grepl(":", remote_path) || dx_is_id(remote_path)) {
          stop("'", remote_path, "' not found on DNAnexus")
        } else {
          stop("'", remote_path, "' not found in current working directory on DNAnexus ('",
               system("dx pwd", intern=TRUE), "')")
        }
      } else if (missing == "skip") {
        return(NULL)
      } else if (missing == "wait") {
        cat(remote_path, "does not exist yet, waiting 10s...\n")
        Sys.sleep(10)
        next
      }
    } else if (type != "folder") {
      # 'remote_path' points to a single file/object

      # Extract the state of the file or object
      state <- dx_state(metadata)

      # If the file is in the 'open' state, defer to the 'incomplete' setting
      if (state == "open") {
        if (incomplete == "error") {
          stop(remote_path, " is an incomplete file still in the process of uploading")
        } else if (incomplete == "skip") {
          return(NULL)
        } else if (incomplete == "wait") {
          cat(remote_path, " is an incomplete file, waiting 10s for upload to finish before trying again...\n")
          Sys.sleep(10)
          next
        }
      }

      # If the file is in the 'closing' state, always wait for DNAnexus to
      # finish finalizing the file before downloading
      if (state == "closing") {
        cat(remote_path, "is in the process of closing, waiting 10s for DNAnexus to finalize the file before trying again...\n")
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
            stop("Error downloading ", remote_path, ": ", metadata$name, " already exists on local machine at ", dirname(local_path), "/")
          } else if (file.exists(local_path) && dir.exists(local_path)) {
            # 'local_path' points to a folder that contains a file with the same name as the remote file
            if (!grepl("/$", local_path)) local_path <- paste0(local_path, "/")
            stop("Error downloading ", remote_path, ": ", metadata$name, " already exists on local machine at ", local_path)
          } else {
            # 'local_path' points to a file with a different name to the remote file
            stop("Error downloading ", remote_path, ": file already exists on local machine at ", local_path)
          }
        } else if (exists == "skip") {
          if (!silent) {
            cat(local_path, "already exists on local machine, skipping download of", dx_path_from_metadata(metadata), "\n")
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
        msg <- suppressWarnings(system(sprintf("dx download -f '%s' -o '%s' 2>&1", remote_path, local_path), intern=TRUE))
        if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))

        # Print out success message
        if (!silent) {
          cat(dx_path_from_metadata(metadata), "on DNAnexus downloaded to local machine at", local_path, "\n")
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
      file_list_metadata <- suppressWarnings(system(sprintf("dx find data --path '%s' --json 2>&1", remote_path), intern=TRUE))
      if (!is.null(attr(file_list_metadata, "status"))) stop(paste(file_list_metadata, collapse="\n"))
      file_list_metadata <- fromJSON(file_list_metadata)

      # Check if any are in the open state, and if they are, check whether any
      # have a "uploaded_by" property matching the current DNAnexus job ID.
      # If there are matches, delete those files, remove them from the download
      # list, and continue
      if (
        length(file_list_metadata) > 0 &&
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
        length(file_list_metadata) > 0 &&
        any(file_list_metadata$describe$state == "open")
      ) {
        incomplete_files <- file_list_metadata[file_list_metadata$describe$state == "open", ]
        incomplete_files <- sprintf("%s:%s/%s", incomplete_files$describe$project,
          incomplete_files$describe$folder, incomplete_files$describe$name)

        if (incomplete == "error") {
          stop(remote_path, " contains ", length(incomplete_files),
               " incomplete files:\n", paste(incomplete_files, collapse="\n"))
        } else if (incomplete == "skip") {
          file_list_metadata <- file_list_metadata[file_list_metadata$describe$state != "open",]
        } else if (incomplete == "wait") {
          cat(remote_path, "contains", length(incomplete_files),
              "incomplete files:\n", paste(incomplete_files, collapse="\n"),
              "waiting 10s for files to finish uploading before trying again...\n")
          Sys.sleep(10)
          next
        }
      }

      # If any files are closing, wait for them to finish
      if (
        length(file_list_metadata) > 0 &&
        any(file_list_metadata$describe$state == "closing")
      ) {
        closing_files <- file_list_metadata[file_list_metadata$describe$state == "closing", ]
        closing_files <- sprintf("%s:%s/%s", closing_files$describe$project,
          closing_files$describe$folder, closing_files$describe$name)

        cat(remote_path, "contains", length(incomplete_files),
            "files in the process of closing:\n", paste(incomplete_files, collapse="\n"),
            "waiting 10s for DNAnexus to finalize files before trying again...\n")
        Sys.sleep(10)
        next
      }

      # Determine location to download each file to
      if (grepl("/$", remote_path)) {
        remote_root <- sprintf("^%s%s/", metadata$folder, metadata$name)
      } else if (metadata$folder != "/") {
        remote_root <- sprintf("^%s/", metadata$folder)
      } else {
        remote_root <- "^/"
      }

      # Get the absolute remote path of each file
      file_list_metadata$remote_path <- paste0(
        file_list_metadata$project, ":",
        gsub("^/$", "", file_list_metadata$describe$folder), "/",
        file_list_metadata$describe$name
      )

      file_list_metadata$local_path <- paste0(
        gsub("/$", "", local_path), "/",
        gsub(remote_root, "", file_list_metadata$describe$folder),
        "/", file_list_metadata$describe$name
      )

      # Check whether any of these files already exist, in accordance with the
      # 'exists' argument
      file_list_metadata$exists <- file.exists(file_list_metadata$local_path)

      if (any(file_list_metadata$exists)) {
        files_exist <- file_list_metadata[file_list_metadata$exists, ]
        files_exist_remote <- sprintf("%s:%s/%s", files_exist$describe$project,
                                      files_exist$describe$folder, files_exist$describe$name)
        if (exists == "error") {
          stop(length(files_exist), " files from ", remote_path,
            " already exist on the local machine at ", gsub("/$", "", local_path),
            "/:", paste(paste(files_exist_remote, files_exist$local_path), collapse="\n"))

        } else if (exists == "skip") {
          cat(paste(paste(files_exist_remote,
            "on DNAnexus skipped as a file on the local machine already exists at",
            files_exist$local_path), collapse="\n"), "\n")

          file_list_metadata <- file_list_metadata[!file_list_metadata$exists,]
        }
      }

      # If we have reached this point without error we can recreate the remote
      # directory tree in the designated local_path
      dx_clone_tree(remote_path, local_path)

      # Download each of the files
      for (ii in seq_len(nrow(file_list_metadata))) {
        msg <- suppressWarnings(system(sprintf(
            "dx download -f '%s' -o '%s' 2>&1",
            file_list_metadata$remote_path[ii], file_list_metadata$local_path[ii]
          ), intern=TRUE))
        if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))

        # Print out success message
        if (!silent) {
          cat(file_list_metadata$remote_path[ii],
            "on DNAnexus downloaded to local machine at",
            file_list_metadata$local_path[ii], "\n")
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
#'
#' @returns NULL
dx_clone_tree <- function(remote_path, local_path, remote_subdir) {
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
      dx_clone_tree(remote_path, local_path, sd)
    } else {
      dx_clone_tree(remote_path, local_path, sprintf("%s/%s", remote_subdir, sd))
    }
  }

  # Create the remote directory at the current depth of the remote tree in the
  # respective location on the local path
  if (missing(remote_subdir)) {
    if (grepl("/$", remote_path)) {
      msg <- suppressWarnings(system(sprintf("mkdir -p '%s'", local_path), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    } else {
      msg <- suppressWarnings(system(sprintf("mkdir -p '%s/%s'", local_path, basename(remote_path)), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    }
  } else {
    if (grepl("/$", remote_path)) {
      msg <- suppressWarnings(system(sprintf("mkdir -p '%s/%s'", local_path, remote_subdir), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    } else {
      msg <- suppressWarnings(system(sprintf("mkdir -p '%s/%s/%s'", local_path, basename(remote_path), remote_subdir), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    }
  }
}
