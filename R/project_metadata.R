#' Get the ID of the currently selected DNA nexus project
#'
#' @returns A string corresponding to a DNA nexus project identifier.
dx_get_current_project <- function() {
  id <- suppressWarnings(system("dx env | grep 'Current workspace\t' | cut -f 2 2>&1", intern=TRUE))
  if (!is.null(attr(id, "status"))) stop(paste(id, collapse="\n"))
  return(id)
}

#' Get the ID of the DNA nexus project associated with a remote path
#'
#' @inheritParams remote_path
#'
#' @returns A string corresponding to a DNA nexus project identifier.
dx_get_project <- function(remote_path) {
  if (dx_is_project_id(remote_path)) {
    return(remote_path)
  } else if (!grepl(":", remote_path)) {
    return(dx_get_current_project())
  } else {
    remote_path <- gsub(":.*", "", remote_path)
    if (dx_is_project_id(remote_path)) {
      return(remote_path)
    } else {
      metadata <- suppressWarnings(system(sprintf("dx describe '%s:' --json 2>&1", remote_path), intern=TRUE))
      if (!is.null(attr(metadata, "status"))) {
        if (grepl("dxpy.exceptions.DXCLIError", metadata)) {
          stop("No project named '", remote_path, "' found on DNA nexus")
        } else {
          stop(paste(metadata, collapse="\n")) # Some other error, e.g. contacting servers
        }
      }
      return(fromJSON(metadata)$id)
    }
  }
}

#' Get the metadata associated with a DNA nexus project
#'
#' @inheritParams remote_path
#'
#' @returns a named list containing the project metadata obtained from the
#'   output of `dx describe --json`
#'
#' @importFrom jsonlite fromJSON
dx_get_project_metadata <- function(remote_path) {
  # Get the project ID
  project_id <- dx_get_project(remote_path)

  # Get the metadata associated with the object at the remote_path:
  metadata <- suppressWarnings(system(sprintf("dx describe '%s:' --json 2>&1", project_id), intern=TRUE))
  if (!is.null(attr(metadata, "status"))) {
    if (grepl("dxpy.exceptions.DXCLIError", metadata)) {
      stop("No project named '", remote_path, "' found on DNA nexus")
    } else {
      stop(paste(metadata, collapse="\n")) # Some other error, e.g. contacting servers
    }
  }
  return(fromJSON(metadata))
}


#' Determine access permissions of a DNA nexus project
#'
#' @inheritParams remote_path
#'
#' @returns "NONE", "VIEW", "CONTRIBUTE", or "ADMINISTER", as an ordered factor.
#'
#' @importFrom jsonlite fromJSON
dx_get_project_permissions <- function(remote_path) {
  metadata <- dx_get_project_metadata(remote_path)
  ordered(
    fromJSON(metadata)$level,
    levels=c("NONE", "VIEW", "CONTRIBUTE", "ADMINISTER")
  )
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
  metadata <- dx_get_project_metadata(remote_path)
  metadata$level == "ADMINISTER" || (metadata$level == "CONTRIBUTE" && !metadata$protected)
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
