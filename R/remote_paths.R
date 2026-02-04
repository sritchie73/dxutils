#' @param remote_path DNAnexus file identifier or DNAnexus file/folder path
#'    (Syntax: "projectID:/folder/path", "/folder/path", or "folder/path").
#'    Paths are relative to the current working directory in the current project
#'    unless otherwise specified.
#'
#' @name remote_path
NULL

#' Get the standardized absolute format of a remote path on DNAnexus
#'
#' @inheritParams remote_path
#' @param return_as_parts logical; if TRUE, returns the result as a named list
#'    containing the project ID, folder, and name.
#'
#' @returns a DNAnexus path with syntax 'project-ID:/path/to/file.ext'
#'
#' @importFrom utils read.table
dx_normalize_path <- function(remote_path, return_as_parts=FALSE) {

  if (dx_is_data_id(remote_path)) {
    # If the remote_path is a DNAnexus data object ID, query the object directly
    # to get its metadata
    metadata <- dx_get_metadata(remote_path)
    if (dx_type(metadata) == "none") {
      throw_file_not_exists_error(remote_path)
    }
    abs_path <- dx_path_from_metadata(metadata)

  } else if (dx_path_contains_project(remote_path)) {
    # Remote path already has the project name or ID as part of the path
    project <- gsub("^(.*?):.*", "\\1", remote_path)

    # Standardise the rest of the path
    rel_path <- gsub("^.*?:", "", remote_path)
    if (!grepl("^/", rel_path)) rel_path <- paste0("/", rel_path)

    # If the project is given as a name instead of a project ID we need to
    # look this up
    if (!dx_is_project_id(project)) {
      metadata <- dx_get_project_metadata(paste0(project, ":/"))
      project <- metadata$id

    }

    abs_path <- sprintf("%s:%s", project, rel_path)

  } else if (dx_is_container_job()) {
    # We are running on a DNAnexus job that is running within a container, e.g.
    # a cloud workstation. In this case, particularly for 'dx_upload()' we want
    # to bypass the container to access project storage directly. The cloud
    # workstation also does not have the ability to 'dx cd' to the project
    # storage or a different folder within project storage.
    project <- Sys.getenv("DX_PROJECT_CONTEXT_ID")
    rel_path <- remote_path
    if (!grepl("^/", remote_path)) rel_path <- paste0("/", rel_path)

    abs_path <- sprintf("%s:%s", project, rel_path)
  } else {
    # Otherwise we need to get the current working directory of the user
    env <- suppressWarnings(system("dx env | sed 's/\t\t/\t/'" , intern=TRUE))
    if (!is.null(attr(env, "status"))) stop(paste(env, collapse="\n"))
    env <- read.table(text=env, header=FALSE, col.names=c("key", "value"), sep="\t")

    project <- env[env$key == "Current workspace", "value"]

    # How we handle the path depends on whether the user has given an absolute
    # or relative path
    if (grepl("^/", remote_path)) {
      rel_path <- remote_path
    } else {
      wd <- env[env$key == "Current folder", "value"]
      if (wd == "/") {
        rel_path <- paste0(wd, remote_path)
      } else {
        rel_path <- sprintf("%s/%s", wd, remote_path)
      }
    }

    abs_path <- sprintf("%s:%s", project, rel_path)
  }

  # Return the path now, unless we need to split up and return as parts
  if (!return_as_parts) {
    return(abs_path)
  } else {
    project_id <- gsub("^(project-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}).*", "\\1", abs_path)
    rel_path <- gsub("project-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}:", "", abs_path)
    if (rel_path == "/") {
      dirname <- "/"
      basename <- ""
    } else {
      dirname <- dirname(rel_path)
      basename <- basename(rel_path)
      if (grepl("/$", rel_path)) {
        basename <- paste0(basename, "/")
      }
    }
    return(list("project"=project_id, "folder"=dirname, "name"=basename))
  }
}

#' Does the user-provided remote path contain the project name or ID?
#'
#' Returns TRUE if the remote path contains a colon (":") and that colon is
#' not escaped ("\:") (which is the case for files or folders on DNAnexus that
#' contain a colon).
#'
#' @inheritParams remote_path
#'
#' @returns TRUE or FALSE
dx_path_contains_project <- function(remote_path) {
  dx_is_project_id(remote_path) || grepl("(?<!\\\\):", remote_path, perl = TRUE)
}
