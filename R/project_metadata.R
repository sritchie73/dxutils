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

#' Determine access permissions of a DNA nexus project
#'
#' @inheritParams remote_path
#' @importFrom jsonlite fromJSON
#'
#' @returns "NONE", "VIEW", "CONTRIBUTE", or "ADMINISTER", as an ordered factor.
dx_get_project_permissions <- function(remote_path) {
  project_id <- dx_get_project(remote_path)
  ordered(
    fromJSON(system(sprintf("dx describe '%s' --json", project_id), intern=TRUE))$level,
    levels=c("NONE", "VIEW", "CONTRIBUTE", "ADMINISTER")
  )
}

#' Determine whether a DNA nexus project has its Protected attribute set to true
#'
#' @inheritParams remote_path
#'
#' @details
#' DNA nexus projects may be configured so that object deletion is restricted to
#' administrators of the project.
#'
#' @importFrom jsonlite fromJSON
#'
#' @returns Logical; either TRUE or FALSE.
dx_project_protected <- function(remote_path) {
  project_id <- dx_get_project(remote_path)
  fromJSON(system(sprintf("dx describe '%s' --json", project_id), intern=TRUE))$protected
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
