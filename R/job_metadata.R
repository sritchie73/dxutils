#' Check whether a string matches the format of a DNAnexus job ID
#'
#' See <https://documentation.dnanexus.com/developer/api/entity-ids>
#'
#' @param string a character vector of length 1
#'
#' @returns TRUE or FALSE
dx_is_job_id <- function(string) {
  stopifnot(length(string) == 1 && is.character(string))
  grepl("^job-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}$", string)
}

#' Is the current process running as part of a DNAnexus job?
#'
#' @returns TRUE or FALSE
dx_is_job <- function() {
  Sys.getenv("DX_JOB_ID") != ""
}

#' Is the current process running within a containerized DNAnexus job?
#'
#' @details
#' Some DNAnexus jobs, e.g. cloud workstations, are run within a DNAnexus
#' container. This has two consequences:
#'
#'  (1) When given a path without the project name or project ID, DNAnexus
#'      command line utilities look for the path in the container, instead of
#'      the project storage associated with the job
#'  (2) The current process only has access to the project storage associated
#'      with the job, not the full set of projects the user has permissions to.
#'
#' @return TRUE or FALSE
dx_is_container_job <- function() {
  dx_is_job() && Sys.getenv("DX_PROJECT_CONTEXT_ID") != Sys.getenv("DX_WORKSPACE_ID")
}
