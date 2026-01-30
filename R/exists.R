#' Check whether an object or folder exists on a DNA nexus project
#'
#' @inheritParams remote_path
#'
#' @returns Logical; TRUE or FALSE.
#'
#' @export
dx_exists <- function(remote_path) {
  assert_dx_project_permissions(remote_path, "VIEW")
  !system(sprintf("dx ls '%s' &> /dev/null", remote_path))
}

#' Throw an error if the requested object or folder does not exist on DNA nexus
#'
#' @inheritParams remote_path
#
#' @returns NULL
#'
#' @export
assert_dx_exists <- function(remote_path) {
  if (!dx_exists(remote_path)) {
    if (grepl(":", remote_path)) {
      stop(remote_path, " not found on DNA nexus project storage")
    } else {
      stop(system("dx pwd", intern=TRUE), remote_path, " not found on DNA nexus project storage")
    }
  }
}
