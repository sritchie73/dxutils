#' Check whether a string matches the format of a DNAnexus unique ID
#'
#' See <https://documentation.dnanexus.com/developer/api/entity-ids>
#'
#' @param string a character vector of length 1
#'
#' @returns TRUE or FALSE
dx_is_id <- function(string) {
  stopifnot(length(string) == 1 && is.character(string))
  grepl("^(project)|(container)|(file)|(applet)|(record)|(app)|(database)|(job)-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}$", string)
}

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

#' Check whether a string matches the format of a DNAnexus data object
#'
#' See <https://documentation.dnanexus.com/developer/api/entity-ids>
#'
#' @param string a character vector of length 1
#'
#' @returns TRUE or FALSE
dx_is_data_id <- function(string) {
  stopifnot(length(string) == 1 && is.character(string))
  grepl("^(file)|(applet)|(record)|(app)|(database)-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}$", string)
}
