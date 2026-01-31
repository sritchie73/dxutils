#' Delete an object or folder on a DNA nexus project
#'
#' @description
#' Deletes a file or recursively deletes a folder at the given path on a DNA
#' nexus project. For projects where the user does not have file deletion
#' permissions, moves the file or folder to the trash/ folder at the root folder
#' of the project, creating that folder if necessary.
#'
#' @details
#' Where the user has the necessary permissions to delete files, runs `dx rm -rfa`
#' on the provided 'remote_path'.
#'
#' If the user does not have delete permissions and the remote path is a DNA
#' nexus object (e.g. a file), runs `dx mv` to relocate the object to the trash/
#' folder at the project root.
#'
#' If the user does not have delete permissions and the remote path is a folder,
#' renames the folder to include the current date and time, then moves that
#' folder to trash/ with `dx mv`. The renaming step ensures that the folder name
#' is unique, which necessary to prevent errors when moving to the trash/ folder
#' as there cannot be multiple folders with the same name in one location on a
#' DNA nexus project, and `dx mv` does not support merging two folders with the
#' same name.
#'
#' @inheritParams remote_path
#'
#' @returns NULL
#'
#' @export
dx_rm <- function(remote_path) {
  assert_dx_project_permissions(remote_path, "CONTRIBUTE")
  if (dx_user_can_rm(remote_path)) {
    system(sprintf("dx rm -rfa '%s'", remote_path))
  } else {
    project_id <- dx_get_project(remote_path)
    system(sprintf("dx mkdir -p %s:trash", project_id))
    if (dx_type(remote_path) == "folder") {
      uid <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S") # can't have multiple folders of the same name
      system(sprintf("dx mv '%s' '%s-%s'", remote_path, remote_path, uid))
      system(sprintf("dx mv '%s-%s' %s:trash/", remote_path, uid, project_id))
    } else {
      system(sprintf("dx mv '%s' %s:trash/", remote_path, project_id))
    }
  }
}
