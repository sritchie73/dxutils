#' Check whether a string matches the format of a DNAnexus project ID
#'
#' See <https://documentation.dnanexus.com/developer/api/entity-ids>
#'
#' @param string a character vector of length 1
#'
#' @returns TRUE or FALSE
dx_is_project_id <- function(string) {
  stopifnot(length(string) == 1 && is.character(string))
  grepl("^project-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}$", string)
}

#' Get the metadata associated with a DNAnexus project
#'
#' @inheritParams remote_path
#'
#' @returns a named list containing the project metadata obtained from the
#'   output of `dx describe --json`
#'
#' @importFrom jsonlite fromJSON
dx_get_project_metadata <- function(remote_path) {
  # Determine the ID or name of the project associated with the remote path
  if (!dx_path_contains_project(remote_path)) {
    remote_path <- dx_normalize_path(remote_path)
  }
  project <- gsub("^(.*?):.+", "\\1", remote_path)

  # Get the metadata associated with the project
  metadata <- suppressWarnings(system(sprintf("dx describe '%s:' --json 2>&1", project), intern=TRUE))
  if (!is.null(attr(metadata, "status"))) {
    if (grepl("dxpy.exceptions.DXCLIError: No match found for", metadata[1])) {
      if (dx_is_container_job()) {
        parent_project_id <- Sys.getenv("DX_PROJECT_CONTEXT_ID")
        parent_metadata <- dx_get_project_metadata(parent_project_id)
        stop("Unable to connect to project '", project,
             "', this job only has access to project '", parent_metadata$name,
             "' (", parent_project_id, ")")
      } else {
        stop("could not find project '", project, "' on DNAnexus")
      }
    } else {
      stop(paste(metadata, collapse="\n")) # Some other error, e.g. contacting servers
    }
  }
  return(fromJSON(metadata))
}

#' Determine access permissions of a DNAnexus project
#'
#' @param metadata project metadata extracted by the `dx_get_project_metadata`
#'    function
#'
#' @returns "NONE", "VIEW", "CONTRIBUTE", or "ADMINISTER", as an ordered factor.
#'
#' @importFrom jsonlite fromJSON
dx_get_project_permissions <- function(metadata) {
  ordered(metadata$level, levels=c("NONE", "VIEW", "CONTRIBUTE", "ADMINISTER"))
}

#' Checks whether the user can delete files on a DNAnexus project
#'
#' @param metadata project metadata extracted by the `dx_get_project_metadata`
#'   function
#'
#' @details
#' Returns TRUE if the user has at least "CONTRIBUTE" permissions, or if the
#' project has the Protected attribute set to true, if the user has "ADMINISTER"
#' permissions.
#'
#' @returns Logical; either TRUE or FALSE.
dx_user_can_rm <- function(metadata) {
  metadata$level == "ADMINISTER" || (metadata$level == "CONTRIBUTE" && !metadata$protected)
}

#' Error if the user does not has sufficient permissions for a given operation
#'
#' @param metadata project metadata extracted by the `dx_get_project_metadata`
#'   function
#' @param minimum_permissions minimum permissions user must have on the DNAnexus
#'   project. Must be one of "VIEW", "CONTRIBUTE" or "ADMINISITER"
#'
#' @returns NULL
assert_dx_project_permissions <- function(metadata, minimum_permissions) {
  valid_permissions <- c("VIEW", "CONTRIBUTE", "ADMINISTER")
  if (!(minimum_permissions %in% valid_permissions)) {
    stop("'minimum_permissions' must be one of \"VIEW\", \"CONTRIBUTE\" or \"ADMINISITER\"")
  }
  user_permissions <- dx_get_project_permissions(metadata)
  if (user_permissions < minimum_permissions) {
    stop("Insufficient privileges for operation in project ", metadata$id,
         ": user permissions are ", user_permissions, ", at least ",
         minimum_permissions, "required")
  }
}
