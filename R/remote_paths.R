#' @param remote_path DNAnexus file identifier or DNAnexus file/folder path
#'    (Syntax: "projectID:/folder/path", "/folder/path", or "folder/path").
#'    Paths are relative to the current working directory in the current project
#'    unless otherwise specified.
#'
#' @name remote_path
NULL

#' @param normalized_remote_path DNAnexus file or folder path that has been
#'    processed by the `dx_normalize_path` function and now has the syntax
#'    "projectID:/folder/path"
#'
#' @name normalized_remote_path
NULL

#' Get the current project and working directory on DNAnexus
#'
#' As opposed to 'dx pwd' returns the path with the project-ID instead of the
#' project name
#'
#' @return a DNAnexus path with syntax 'project-ID:/path/to/folder'
#'
#' @importFrom utils read.table
dx_getwd <- function() {
  env <- suppressWarnings(system("dx env | sed 's/\t\t/\t/'" , intern=TRUE))
  if (!is.null(attr(env, "status"))) stop(paste(env, collapse="\n"))
  env <- read.table(text=env, header=FALSE, col.names=c("key", "value"), sep="\t")
  sprintf("%s:%s", env[env$key == "Current workspace", "value"], env[env$key == "Current folder", "value"])
}

#' Get the standardized absolute format of a remote path on DNAnexus
#'
#' @details
#' This is designed as a low-level helper function - i.e. it avoids making
#' expensive DNAnexus API calls to 'dx describe' to work out what is at the
#' remote_path.
#'
#' If the remote_path is a DNAnexus object identifier rather than a path,
#' you need to use the `dx_path_from_metadata` function after extracting the
#' object metadata using the `dx_get_metadata` function.
#'
#' @param remote_path a DNAnexus file/folder path with syntax
#'    "projectID:/folder/path", "/folder/path", or "folder/path".
#' @param return_as_parts logical; if TRUE, returns the result as a named list
#'    containing the project ID, folder, and name.
#'
#' @returns a DNAnexus path with syntax 'project-ID:/path/to/file.ext'
dx_normalize_path <- function(remote_path, return_as_parts=FALSE) {
  if (dx_is_data_id(remote_path)) {
    stop("dx_normalize_path() must be used on a path not a DNAnexus data ID")
  }

  # The easiest way to do this that also handles folders appears to
  # be using 'dx cd' to change to the target remote path, get the absolute
  # location using 'dx_getwd()'.

  # 'remote_path' may be a file or a folder - obviously we can only 'dx cd' if
  # the path is a folder. To prevent expensive API calls to determine what the
  # path is pointing at (whose functions may indeed rely on this one), we simply
  # take the dirname() of the remote path to ensure that we're changing to a
  # directory (i.e. either the folder containing the remote file, or parent
  # folder containing the remote folder). The only time we can't take this
  # approach is when the remote_path is "project-ID:/" or "project-ID:" as then
  # dirname() will return "." instead of "/".
  if (grepl(":", remote_path) && !grepl("(:/)|(:$)|", remote_path)) {
    remote_path <- gsub(":", ":/", remote_path)
  }
  if (grepl("(:$)|(/$)", remote_path)) {
    dirname <- remote_path
    basename <- ""
  } else {
    dirname <- dirname(remote_path)
    basename <- basename(remote_path)
  }

  # We only need to 'dx cd' if the dirname doesn't resolve to "." in which
  # case the remote path points to a file or folder in the current working
  # directory.
  if (dirname != ".") {
    # What is the current working directory?
    working_dir <- dx_getwd()

    # Make sure that on function exit, regardless of reason, we always change back
    # to the current working directory
    on.exit({
      msg <- suppressWarnings(system(sprintf("dx cd '%s' 2>&1", working_dir), intern=TRUE))
      if (!is.null(attr(msg, "status"))) stop(paste(msg, collapse="\n"))
    })

    # Enter a while loop, in case we're in a nested folder path that doesn't yet
    # exist, so that we can keep trying higher and higher in the tree
    while (TRUE) {
      # Try to change the current working directory to the parent folder of the
      # remote_path on DNAnexus
      msg <- suppressWarnings(system(sprintf("dx cd '%s' 2>&1", dirname), intern=TRUE))
      if (!is.null(attr(msg, "status"))) {
        if (grepl("Folder .* does not exist in project", msg[1])) {
          # If the parent folder does not exist, try the parent of the parent
          # folder and so on
          basename <- sprintf("%s/%s", basename(dirname), basename)
          dirname <- dirname(dirname)
          next
        } else {
          # Some other error, e.g. permissions, can't contact DNAnexus servers
          stop(paste(msg, collapse="\n"))
        }
      }

      # We have successfully changed working directory to the target location, so
      # we can exit the while loop
      break
    }
  }

  # Get the absolute path format of the remote_path
  dirname <- dx_getwd()
  if (basename == "") {
    abs_path <- dirname
  } else if (!grepl("/$", dirname)) {
    abs_path <- sprintf("%s/%s", dirname, basename)
  } else {
    abs_path <- sprintf("%s%s", dirname, basename)
  }

  # Return the path now, unless we need to split up and return as parts
  if (!return_as_parts) {
    return(abs_path)
  } else {
    if (grepl("(/$)|(:$)", abs_path)) {
      dirname <- abs_path
      basename <- ""
    } else {
      dirname <- dirname(abs_path)
      basename <- basename(abs_path)
    }
    if (grepl(":$", dirname)) {
      dirname <- paste0(dirname, "/")
    }
    return(list(
      project_id=gsub("^(project-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}).*", "\\1", dirname),
      folder=gsub("project-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}:", "", dirname),
      name=basename
    ))
  }
}

#' Extract the project ID from an absolute path on DNAnexus
#'
#' @inheritParams normalized_remote_path
#'
#' @returns a DNAnexus project ID
dx_extract_project_id <- function(normalized_remote_path) {
  gsub("^(project-[0123456789BFGJKPQVXYZbfgjkpqvxyz]{24}).*", "\\1", normalized_remote_path)
}
