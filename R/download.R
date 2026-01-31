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
#' @param missing action to take if the 'remote_path' is not found on DNA nexus.
#'    One of "error" (default), "skip", or "wait".
#' @param incomplete action to take if the 'remote_path' points to an incomplete
#'    file on DNA nexus not created by the current DNA nexus job. One of
#'    "error", "skip", or "wait" (default).
#'
#' @returns The path on the local machine where the file or files were downloaded
#'    to, replacing the equivalent path given in the 'remote_path' argument.
#'
#' @export
dx_download <- function(remote_path, local_path, exists="skip", missing="error", incomplete="wait") {
  # Check for valid arguments
  stopifnot(length(remote_path) == 1 && is.character(remote_path) && !is.na(remote_path))
  stopifnot(length(local_path) == 1 && is.character(local_path) && !is.na(local_path))
  stopifnot(length(exists) == 1 && exists %in% c("error", "overwrite", "skip"))
  stopifnot(length(missing) == 1 && missing %in% c("error", "skip", "wait"))
  stopifnot(length(incomplete) == 1 && incomplete %in% c("error", "skip", "wait"))

  # Poll server for information about the project associated with the user
  # provided 'remote_path'
  project_metadata <- dx_get_project_metadata(remote_path)

  # Check we have necessary permission to download
  assert_dx_project_permissions(project_metadata, "VIEW")

  # Enter while TRUE loop to handle conditions where we may have to wait
  while (TRUE) {
    # Get the metadata associated with the entity at the remote path (if any)
    entity_metadata <- dx_get_metadata(remote_path)

    # Determine if its a file, folder, or points to something that doesn't exist
    entity_type <- dx_type(entity_metadata)

    # How we handle things depends on whether the remote path exists or not,
    # and if it does whether or not its a folder
    if (entity_type == "none") {
      # If the nothing exists (yet) at 'remote_path', either throw an error
      # if 'missing' is set to "error", return 'NULL' if 'missing' is set to
      # "skip", or wait for 10s before checking again if 'missing' is set to
      # "wait".
      if (missing == "error") {
        if (grepl(":", remote_path) || dx_is_id(remote_path)) {
          stop("'", remote_path, "' not found on DNA nexus")
        } else {
          stop("'", remote_path, "' not found in current working directory on DNA nexus ('",
               system("dx pwd", intern=TRUE), "')")
        }
      } else if (missing == "skip") {
        return(NULL)
      } else if (missing == "wait") {
        cat(remote_path, "does not exist yet, waiting 10s...\n")
        sleep(10)
        next
      }
    } else if (entity_type != "folder") {
      # 'remote_path' points to a single file/object

      # Extract the state of the file or object
      file_state <- dx_state(entity_metadata)

      # If the file is in the 'open' state, defer to the 'incomplete' setting
      if (file_state == "open") {
        if (incomplete == "error") {
          stop(remote_path, " is an incomplete file")
        } else if (incomplete == "skip") {
          return(NULL)
        } else if (incomplete == "wait") {
          cat(remote_path, " is an incomplete file, waiting 10s for upload to finish and trying again...\n")
          sleep(10)
          next
        }
      }

      # If the file is in the 'closing' state, always wait for DNA nexus to
      # finish finalizing the file before downloading
      if (file_state == "closing") {
        cat(remote_path, "is in the process of finalizing, waiting 10s and trying again...\n")
        sleep(10)
        next
      }

      # File is ok to download, now check the target local_path to make sure
      # we're not overwriting an existing file unless intended
      local_path_exists <- file.exists(local_path)
      if (local_path_exists && dir.exists(local_path)) {
        local_path_exists <- file.exists(file.path(local_path, basename(remote_path)))
      }

      if (local_path_exists) {
        if (exists == "error") {
          if (file.exists(local_path) && !dir.exists(local_path) && basename(local_path) == entity_metadata$name) {
            # 'local_path' points to a file with the same name as the remote file
            stop("Error downloading ", remote_path, ": ", entity_metadata$name, " already exists on local machine at ", dirname(local_path), "/")
          } else if (file.exists(local_path) && dir.exists(local_path)) {
            # 'local_path' points to a folder that contains a file with the same name as the remote file
            if (!grepl("/$", local_path)) local_path <- paste0(local_path, "/")
            stop("Error downloading ", remote_path, ": ", entity_metadata$name, " already exists on local machine at ", local_path)
          } else {
            # 'local_path' points to a file with a different name to the remote file
            stop("Error downloading ", remote_path, ": file already exists at target location ", local_path)
          }
        }
      }

      # At this point, we are now ok to download the file if we need to (we
      # might not need to, i.e. if local_path_exists && exists == "skip")
      if (!local_path_exists || exists == "overwrite") {

        # Before doing so, we need to make sure any directories on the local
        # machine that need to exist are created
        if (grepl("/$", local_path)) {
          system(sprintf("mkdir -p '%s'", local_path))
        } else {
          system(sprintf("mkdir -p '%s'", dirname(local_path)))
        }

        # Download the file
        system(sprintf("dx download -f '%s' -o '%s'", remote_path, local_path))
      }

      # Determine path of the downloaded file to return as a string
      if (dir.exists(local_path)) {
        return(file.path(local_path, entity_metadata$name))
      } else {
        return(local_path)
      }

    } else {
      # 'remote_path' is a folder to download

    }
  }
}
