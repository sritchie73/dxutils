#' Determine the type of entity at a location on a DNA nexus project
#'
#' @inheritParams remote_path
#' @importFrom jsonlite fromJSON
#'
#' @returns "none" if nothing exists at the 'remote_path' provided, "folder" if
#'  the 'remote_path' points to a folder, or the Class attribute (e.g. "file")
#'  if the 'remote_path' points to a DNA nexus object.
dx_type <- function(remote_path) {
  if (dx_exists(remote_path)) {
    can_be_described <- !system(sprintf("dx describe '%s' &> /dev/null", remote_path))
    if (!can_be_described) {
      return("folder")
    } else {
      return(fromJSON(system(sprintf("dx describe '%s' --json", remote_path), intern=TRUE))$class)
    }
  }
  return("none")
}
