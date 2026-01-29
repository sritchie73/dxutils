#' Get the unique identifier of a DNA nexus project
#'
#' @inheritParams remote_path
#'
#' @details
#' There are three ways the project identifier is determined:
#' 1. If no `remote_path` is provided, then the ID of the current project is
#'    returned.
#' 2. If `remote_path` is provided, and includes a project identifier as part
#'    of the path, returns the project identifier embedded in path.
#' 3. If `remote_path` is provided without a project identifier, the ID of the
#'    current project is returned
#'
#' @returns A string corresponding to a DNA nexus project identifier.
dx_get_project <- function(remote_path) {
  if (missing(remote_path) || !grepl(":", remote_path)) {
    system("dx env | grep 'Current workspace\t' | cut -f 2", intern=TRUE)
  } else {
    gsub(":.*", "", remote_path)
  }
}
