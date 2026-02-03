#' Delete an object or folder on a DNAnexus project
#'
#' @description
#' Deletes a file or recursively deletes a folder at the given path on DNAnexus.
#' For projects where the user does not have file deletion ermissions, moves the
#' file or folder to the trash/ folder at the root folder of the project,
#' creating that folder if necessary.
#'
#' @details
#' Where the user has the necessary permissions to delete files, runs `dx rm -rfa`
#' on the provided 'remote_path'.
#'
#' If the user does not have delete permissions and the remote path is a
#' DNAnexus object (e.g. a file), runs `dx mv` to relocate the object to the
#' trash/ folder at the project root.
#'
#' If the user does not have delete permissions and the remote path is a folder,
#' renames the folder to include the current date and time, then moves that
#' folder to trash/ with `dx mv`. The renaming step ensures that the folder name
#' is unique, which necessary to prevent errors when moving to the trash/ folder
#' as there cannot be multiple folders with the same name in one location on a
#' DNAnexus project, and `dx mv` does not support merging two folders with the
#' same name.
#'
#' @inheritParams remote_path
#' @param not_exists action to take if the location being removed does not
#'   exist, either "ignore" or "error".
#'
#' @returns NULL
#'
#' @export
dx_rm <- function(remote_path, not_exists="ignore") {
  stopifnot(length(remote_path) == 1)
  stopifnot(length(not_exists) == 1 && not_exists %in% c("ignore", "error"))

  # Poll server to get information about the project associated with the
  # user-provided remote_path
  if (dx_is_data_id(remote_path)) {
    entity_metadata <- dx_get_metadata(remote_path)
    normalized_remote_path <- dx_path_from_metadata(entity_metadata)
  } else {
    normalized_remote_path <- dx_normalize_path(remote_path)
  }
  project_id <- dx_extract_project_id(normalized_remote_path)
  project_metadata <- dx_get_project_metadata(normalized_remote_path)

  # Check we have permissions to delete (or at least move) files in this
  # project
  assert_dx_project_permissions(project_metadata, "CONTRIBUTE")

  # Handling of file removal depends on exact permissions, see Details section
  # of function documentation
  if (dx_user_can_rm(project_metadata)) {
    # Remove file, do not error if the file doesn't exist (matching 'rm -f')
    msg <- suppressWarnings(system(sprintf("dx rm -rfa '%s' 2>&1", remote_path), intern=TRUE))
    if (!is.null(attr(msg, "status"))) {
      if (grepl("Could not resolve", msg[1])) {
        if (not_exists == "error") stop(remote_path, " not found on DNAnexus")
      } else {
        stop(paste(msg, collapse="\n"))
      }
    }
  } else {
    # Set up a trash/ folder in the project if needed
    msg <- suppressWarnings(system(sprintf("dx mkdir -p %s:trash 2>&1", project_id), intern=TRUE))
    if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n")) # Something has gone wrong, we should be able to 'dx mv'

    # Extract information about the remote location
    if (!exists('entity_metadata')) {
      entity_metadata <- dx_get_metadata(remote_path)
    }
    entity_type <- dx_type(entity_metadata)

    # Are we dealing with a file or a folder?
    if (length(entity_type) == 1 && entity_type == "folder") {
      # Unlike files, we can't have multiple folders with the same name in one
      # location, so to prevent conflicts in trash/ we attached the date and
      # time to the folder name before moving to trash/
      uid <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S")
      msg <- suppressWarnings(system(sprintf("dx mv '%s' '%s-%s' 2>&1", remote_path, remote_path, uid), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
      msg <- suppressWarnings(system(sprintf("dx mv '%s-%s' %s:trash/ 2>&1", remote_path, uid, project_id), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    } else {
      if (length(entity_metadata$name) == 1) {
        # Multiple files with the same name can exist in one location on DNAnexus
        # by design - as they are distinguished by file ID not name. So we can
        # just move any file to trash/ without needing to rename
        msg <- suppressWarnings(system(sprintf("dx mv '%s' %s:trash/ 2>&1", remote_path, entity_metadata$project), intern=TRUE))
        if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
      } else {
        # If we're deleting multiple files of the same name, move by ID to trash/
        for (fid in entity_metadata$id) {
          msg <- suppressWarnings(system(sprintf("dx mv '%s:%s' %s:trash/ 2>&1", entity_metadata$project, fid, entity_metadata$project), intern=TRUE))
          if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
        }
      }
    }
  }
}
