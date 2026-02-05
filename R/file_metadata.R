#' @details # File states on DNAnexus
#' Files on DNAnexus can be in three possible states: "closed", "closing", or
#' "open". Files in the "closed" state are those that are complete and not being
#' actively written to by `dx upload`. Files in the "closing" state are those
#' where `dx upload` has finished, and DNAnexus is now finalizing the file.
#' Files in the "open" state are those that are being actively written to by a
#' `dx upload` process, or where a `dx upload` process was killed part way
#' through an upload, e.g. when a low-priority job is interrupted and restarted
#' by AWS.
#'
#' To help distinguish between these scenarios and to facilitate checkpointing,
#' the [dx_upload()] function attaches the job ID to the each file as a property
#' named 'uploaded_by' if run from a DNAnexus job (e.g. from a remote cloud
#' workstation) so that restarted jobs can identify files which were interrupted
#' mid-upload and remove them when next queried.
#'
#' @name file_states
NULL

#' Check whether a string matches the format of a DNAnexus data object ID
#'
#' See <https://documentation.dnanexus.com/developer/api/entity-ids>
#'
#' @param string a character vector of length 1
#'
#' @returns TRUE or FALSE
dx_is_data_id <- function(string) {
  stopifnot(length(string) == 1 && is.character(string))
  grepl("^(file)|(applet)|(record)|(app)|(database)|(workflow)-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}$", string)
}

#' Get the metadata associated with a location on DNAnexus
#'
#' If the location resolves to a file that is in the "open" state, removes that
#' file if it has an "uploaded_by" property matching the current DNAnexus job
#' ID (see Details).
#'
#' @inherit file_states details
#'
#' @inheritParams remote_path
#'
#' @returns a named list containing information about the target location on
#'   DNAnexus. If it resolves to a DNAnexus object, then the output of
#'   `dx describe --json`. Otherwise a named list containing basic information:
#'   the project ID, parent folder, target file/folder name, and either "folder"
#'   or "none" as the class depending on whether or the remote_path resolves
#'   to a folder on a DNAnexus project or the remote_path does not exist (yet).
#'
#' @importFrom jsonlite fromJSON
dx_get_metadata <- function(remote_path) {

  # If the remote_path is a relative path, we need to use 'dx_normalize_path' to
  # convert it to an absolute path, primarily so that if we're running inside
  # containerised job we make sure we're querying the file on project storage,
  # not looking for it on the container itself where it almost certainly doesn't
  # exist (unless it is one of the job input files already copied to the local
  # machine).
  if (!dx_is_data_id(remote_path) && !dx_path_contains_project(remote_path)) {
    remote_path <- dx_normalize_path(remote_path)
  }

  # Check if we can resolve the given remote_path to a location on DNAnexus
  exists <- suppressWarnings(system(sprintf("dx ls '%s' 2>&1", remote_path), intern=TRUE))
  if (!is.null(attr(exists, "status"))) {
    if (grepl("dxpy.utils.resolver.ResolutionError", exists[1])) {
      # Object does not exist
      if (dx_is_data_id(remote_path)) {
        return(c("id"=remote_path, dx_normalize_path("", return_as_parts=TRUE), class="none"))
      } else {
        return(c(id=NA, dx_normalize_path(remote_path, return_as_parts=TRUE), class="none"))
      }
    } else {
      stop(paste(exists, collapse="\n")) # Some other error (e.g. permissions, connecting to server, etc.)
    }
  }

  # Get the metadata associated with the object at the remote_path:
  metadata <- suppressWarnings(system(sprintf("dx describe '%s' --json --mult 2>&1", remote_path), intern=TRUE))
  if (!is.null(attr(metadata, "status"))) stop(paste(metadata, collapse="\n"))

  # If folder metadata is "[]"
  if (length(metadata) == 1 && metadata == "[]") {
    return(c(id=NA, dx_normalize_path(remote_path, return_as_parts=TRUE), class="folder"))
  }

  # parse JSON to list
  metadata <- fromJSON(metadata)

  # If the file is in the open state check whether it has an uploaded_by
  # property matching the current DNAnexus job ID, in which case we remove
  # the file
  for (ii in seq_along(metadata$state)) { # in case multiple files with same name
    if (
      metadata$state[ii] == "open" && Sys.getenv("DX_JOB_ID") == "" &&
      "uploaded_by" %in% names(metadata$properties) &&
      dx_is_job_id(metadata$properties$uploaded_by[ii]) &&
      metadata$properties$uploaded_by[ii] == Sys.getenv("DX_JOB_ID")
    ) {
      if (dx_user_can_rm()) {
        # Remove file, do not error if the file doesn't exist (matching 'rm -f')
        msg <- suppressWarnings(system(sprintf("dx rm -rfa '%s' 2>&1", metadata$id[ii]), intern=TRUE))
        if (!is.null(attr(msg, "status"))) {
          if (!grepl("Could not resolve", msg[1])) {
            stop(paste(msg, collapse="\n"))
          }
        }
      } else {
        # If these commands fail, something has gone wrong - we should have 'dx mv'
        # permissions if the file was created by this job id
        msg <- suppressWarnings(system(sprintf("dx mkdir -p %s:trash 2>&1", metadata$project), intern=TRUE))
        if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
        msg <- suppressWarnings(system(sprintf("dx mv '%s' %s:trash/ 2>&1", metadata$id[ii], metadata$project), intern=TRUE))
        if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
      }
      metadata$class[ii] <- "none"
    }
  }

  # Remove deleted items, if any
  metadata <- metadata[metadata$class != "none",]

  # Return the object metadata, or equivalent metadata for type 'none'
  if (nrow(metadata) == 0) {
    return(c(dx_normalize_path(remote_path, return_as_parts=TRUE), class="none"))
  } else {
    return(metadata)
  }
}

#' Determine the type of entity at a location on a DNAnexus project from metadata
#'
#' @param metadata file metadata extracted by the internal `dx_get_metadata`
#'    function
#'
#' @returns "none" if nothing exists at the 'remote_path' provided, "folder" if
#'  the 'remote_path' points to a folder, or the Class attribute (e.g. "file")
#'  if the 'remote_path' points to a DNAnexus object.
dx_type <- function(metadata) {
  return(metadata$class)
}

#' Get the state of a file (or other data object) on DNAnexus from metadata
#'
#' @inherit file_states details
#'
#' @param metadata file metadata extracted by the `dx_get_metadata` function
#'
#' @returns "none" if the file does not exist (or has been deleted; see Details),
#'   "folder" if the location points to a folder, or one of "closed",
#'   "closing", or "open" (see Details).
#'
#' @importFrom jsonlite fromJSON
dx_state <- function(metadata) {
  # Extract relevant information
  type <- dx_type(metadata)
  if (length(type) == 1 && type %in% c("none", "folder")) {
    return(type)
  } else {
    return(metadata$state) # "closed", "closing", or "open"
  }
}

#' Get the path of a file (or other data object) on DNAnexus from metadata
#'
#' @param metadata file metadata extracted by the internal `dx_get_metadata`
#'    function
#'
#' @returns a DNAnexus path with syntax 'project-ID:/path/to/file.ext'
dx_path_from_metadata <- function(metadata) {
  if (length(metadata$folder) > 1) {
    sprintf("%s:%s", metadata$project, metadata$id)
  } else if (metadata$folder == "/") {
    sprintf("%s:%s%s", metadata$project, metadata$folder, metadata$name)
  } else {
    sprintf("%s:%s/%s", metadata$project, metadata$folder, metadata$name)
  }
}

#' Check whether a file or folder exists on a DNAnexus project
#'
#' @inherit file_states details
#'
#' @inheritParams remote_path
#' @param incomplete logical; if the 'remote_path' points to a file that is
#'   still being written to by another DNAnexus job (see Details), should we
#'   consider this file to exist on DNAnexus?
#'
#' @returns Logical; TRUE or FALSE.
#'
#' @export
dx_exists <- function(remote_path, incomplete=TRUE) {
  stopifnot(length(remote_path) == 1)
  stopifnot(length(incomplete) == 1 && incomplete %in% c(TRUE, FALSE))

  # If not the current project, check we can access
  if (dx_path_contains_project(remote_path)) {
    # Don't need the metadata, just want to error if we can't access the user-provided project
    dx_get_project_metadata(remote_path)
  }

  # Determine state of object, if one exists, at the remote path
  metadata <- dx_get_metadata(remote_path)
  remote_state <- dx_state(metadata)

  if (any(remote_state == "none")) {
    return(FALSE)
  } else if (all(remote_state == "open")) {
    return(incomplete)
  } else {
    return(TRUE)
  }
}

#' Throws an error about the requested object not being found
#'
#' Error message dependent on how the user provided the remote_path and whether
#' the process is running inside a DNAnexus container job.
#'
#' @inheritParams remote_path
#'
#' @returns always throws an error
throw_file_not_exists_error <- function(remote_path) {
  if (dx_path_contains_project(remote_path) || dx_is_data_id(remote_path)) {
    stop("'", remote_path, "' not found on DNAnexus")
  } else if (dx_is_container_job()) {
    parent_project <- Sys.getenv("DX_PROJECT_CONTEXT_ID")
    parent_project_metadata <- dx_get_project_metadata(paste0(parent_project, ":"))
    stop("'", remote_path, "' not found in project storage attached to the",
         "current DNAnexus job ('", parent_project_metadata$name, "' : ",
         parent_project, ")")
  } else {
    stop("'", remote_path, "' not found in current working directory on DNAnexus ('",
         system("dx pwd", intern=TRUE), "')")
  }
}

#' Throw an error if the requested object or folder does not exist on DNAnexus
#'
#' @inherit file_states details
#'
#' @inherit dx_exists params
#'
#' @returns NULL
#'
#' @export
assert_dx_exists <- function(remote_path, incomplete=TRUE) {
  stopifnot(length(remote_path) == 1)
  stopifnot(length(incomplete) == 1 && incomplete %in% c(TRUE, FALSE))

  if (!dx_exists(remote_path, incomplete)) throw_file_not_exists_error(remote_path)
}
