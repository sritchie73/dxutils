#' @param remote_path DNA nexus file identifier or DNA nexus file/folder path
#'    (Syntax: "projectID:/folder/path", "/folder/path", or "folder/path").
#'    Paths are relative to the current working directory in the current project
#'    unless otherwise specified.
#'
#' @name remote_path
NULL

#' Normalize a DNA nexus path or file ID to an absolute path
#'
#' @inheritParams remote_path
#'
#' @returns an absolute remote path with syntax "projectID:/folder/file.txt"
#'
#' @importFrom jsonlite fromJSON
dx_normalize_path <- function(remote_path) {
  metadata <- fromJSON(system(sprintf("dx describe '%s' --json", remote_path), intern=TRUE))
  remote_path <- sprintf("%s:%s/%s", metadata$project, metadata$folder, metadata$name)
  gsub("//", "/", remote_path)
}


