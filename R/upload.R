#' Upload a file or folder to a DNA nexus project
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
#' @param exists action to take if the file being uploaded already exists on DNA
#'    nexus project. One of "error", "replace" (default), or "skip".
#'
#' @returns NULL
#'
#' @export
dx_upload <- function(local_path, remote_path, exists="replace") {

}
