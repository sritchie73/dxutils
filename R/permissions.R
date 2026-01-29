#' Determine access permissions of a DNA nexus project
#'
#' @inheritParams remote_path
#'
#' @returns "NONE", "VIEW", "CONTRIBUTE", or "ADMINISTER", as an ordered factor.
dx_get_project_permissions <- function(remote_path) {
  project_id <- dx_get_project(remote_path)
  line <- system(sprintf("dx describe %s | grep 'Access level'", project_id), intern=TRUE)
  ordered(gsub("Access level +", "", line), levels=c("NONE", "VIEW", "CONTRIBUTE", "ADMINISTER"))
}

#' Determine whether a DNA nexus project has its Protected attribute set to true
#'
#' @inheritParams remote_path
#'
#' @details
#' DNA nexus projects may be configured so that object deletion is restricted to
#' administrators of the project.
#'
#' @returns Logical; either TRUE or FALSE.
dx_project_protected <- function(remote_path) {
  project_id <- dx_get_project(remote_path)
  line <- system(sprintf("dx describe %s | grep 'Protected'", project_id), intern=TRUE)
  as.logical(toupper(gsub("Protected +", "", line)))
}

#' Checks whether the user can delete files on a DNA nexus project
#'
#' @inheritParams remote_path
#'
#' @details
#' Returns TRUE if the user has at least "CONTRIBUTE" permissions, or if the
#' project has the Protected attribute set to true, if the user has "ADMINISTER"
#' permissions.
#'
#' @returns Logical; either TRUE or FALSE.
dx_user_can_rm <- function(remote_path) {
  dx_get_project_permissions(remote_path) == "ADMINISTER" ||
    (dx_get_project_permissions(remote_path) == "CONTRIBUTE" && !dx_project_protected(remote_path))
}

#' Error if the user does not has sufficient permissions for a given operation
#'
#' @inheritParams remote_path
#' @param minimum_permissions minimum permissions user must have on the DNA nexus
#'   project. Must be one of "VIEW", "CONTRIBUTE" or "ADMINISITER".
#'
#' @returns NULL
assert_dx_project_permissions <- function(remote_path, minimum_permissions) {
  valid_permissions <- c("VIEW", "CONTRIBUTE", "ADMINISTER")
  if (!(minimum_permissions %in% valid_permissions)) {
    stop("'minimum_permissions' must be one of \"VIEW\", \"CONTRIBUTE\" or \"ADMINISITER\"")
  }
  user_permissions <- dx_get_project_permissions(remote_path)
  if (user_permissions < minimum_permissions) {
    project_id <- dx_get_project(remote_path)
    stop("Unable to modify files in project ", project_id, ": user permissions are ",
         user_permissions, ", at least ",  minimum_permissions, "required")
  }
}
