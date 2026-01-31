#' Get the metadata associated with a location on DNA nexus
#'
#' If the location resolves to a file that is in the "open" state, removes that
#' file if it has an "uploaded_by" property matching the current DNA nexus job
#' ID (see Details).
#'
#' @inherit file_states details
#'
#' @inheritParams remote_path
#'
#' @returns "none" if nothing exists at that location, "folder" if the location
#'   resolves to a folder on a DNA nexus project, or a named list containing the
#'   object metadata obtained from the output of `dx describe --json`
#'
#' @importFrom jsonlite fromJSON
dx_get_metadata <- function(remote_path) {
  # Check if we can resolve the given remote_path to a location on DNA nexus
  exists <- suppressWarnings(system(sprintf("dx ls '%s' 2>&1", remote_path), intern=TRUE))
  if (!is.null(attr(exists, "status"))) {
    if (grepl("dxpy.utils.resolver.ResolutionError", exists[1])) {
      return("none") # File/folder not found
    } else {
      stop(paste(exists, collapse="\n")) # Some other error (e.g. permissions, connecting to server, etc.)
    }
  }

  # Get the metadata associated with the object at the remote_path:
  metadata <- suppressWarnings(system(sprintf("dx describe '%s' --json 2>&1", remote_path), intern=TRUE))
  if (!is.null(attr(metadata, "status"))) {
    if (grepl("dxpy.exceptions.DXCLIError", metadata[1])) {
      return("folder") # no object to describe at path - i.e. must be a folder
    } else {
      stop(paste(metadata, collapse="\n")) # Some other error, e.g. contacting servers
    }
  }

  # parse JSON to list
  metadata <- fromJSON(metadata)

  # If the file is in the open state check whether it has an uploaded_by
  # property matching the current DNA nexus job ID, in which case we remove
  # the file
  if (
    metadata$state == "open" && Sys.getenv("DX_JOB_ID") == "" &&
    "uploaded_by" %in% names(metadata$properties) &&
    dx_is_job_id(metadata$properties$uploaded_by) &&
    metadata$properties$uploaded_by == Sys.getenv("DX_JOB_ID")
  ) {
    if (dx_user_can_rm()) {
      # Remove file, do not error if the file doesn't exist (matching 'rm -f')
      msg <- suppressWarnings(system(sprintf("dx rm -rfa '%s' 2>&1", remote_path), intern=TRUE))
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
      msg <- suppressWarnings(system(sprintf("dx mv '%s' %s:trash/ 2>&1", remote_path, metadata$project), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    }
    return("none")
  }

  # Return the object metadata
  return(metadata)
}

#' Determine the type of entity at a location on a DNA nexus project
#'
#' @param metadata file metadata extracted by the internal `dx_get_metadata`
#'    function
#'
#' @returns "none" if nothing exists at the 'remote_path' provided, "folder" if
#'  the 'remote_path' points to a folder, or the Class attribute (e.g. "file")
#'  if the 'remote_path' points to a DNA nexus object.
dx_type <- function(metadata) {
  # Extract relevant information
  if (is.null(names(metadata))) {
    return(metadata) # "none" or "folder"
  } else {
    return(metadata$class) # "file", "record", "applet", "database" etc.
  }
}

#' Get the state of a file (or other data object) on DNA nexus
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
  if (is.null(names(metadata))) {
    return(metadata) # "none" or "folder"
  } else {
    return(metadata$state) # "closed", "closing", or "open"
  }
}

#' Check whether a file or folder exists on a DNA nexus project
#'
#' @inherit file_states details
#'
#' @inheritParams remote_path
#' @param incomplete logical; if the 'remote_path' points to a file that is
#'   still being written to by another DNA nexus job (see Details), should we
#'   consider this file to exist on DNA nexus?
#'
#' @returns Logical; TRUE or FALSE.
#'
#' @export
dx_exists <- function(remote_path, incomplete=TRUE) {
  stopifnot(length(remote_path) == 1)
  stopifnot(length(incomplete) == 1 && incomplete %in% c(TRUE, FALSE))

  # Determine state of object, if one exists, at the remote path
  metadata <- dx_get_metadata(remote_path)
  remote_state <- dx_state(metadata)

  if (remote_state == "none") {
    return(FALSE)
  } else if (remote_state == "open") {
    return(incomplete)
  } else {
    return(TRUE)
  }
}

#' Throw an error if the requested object or folder does not exist on DNA nexus
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

  # Just a wrapper over 'dx_exists()' that gives a more informative error
  # message depending on how the user has provided the remote_path.
  if (!dx_exists(remote_path, incomplete)) {
    if (grepl(":", remote_path) || dx_is_id(remote_path)) {
      stop("'", remote_path, "' not found on DNA nexus")
    } else {
      stop("'", remote_path, "' not found in current working directory on DNA nexus ('",
           system("dx pwd", intern=TRUE), "')")
    }
  }
}
