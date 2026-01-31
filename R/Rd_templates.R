#' @details # File states on DNA nexus
#' Files on DNA nexus can be in three possible states: "closed", "closing", or
#' "open". Files in the "closed" state are those that are complete and not being
#' actively written to by `dx upload`. Files in the "closing" state are those
#' where `dx upload` has finished, and DNA nexus is now finalizing the file.
#' Files in the "open" state are those that are being actively written to by a
#' `dx upload` process, or where a `dx upload` process was killed part way
#' through an upload, e.g. when a low-priority job is interrupted and restarted
#' by AWS.
#'
#' To help distinguish between these scenarios and to facilitate checkpointing,
#' the [dx_upload()] function attaches the job ID to the each file as a property
#' named 'uploaded_by' if run from a DNA nexus job (e.g. from a remote cloud
#' workstation) so that restarted jobs can identify files which were interrupted
#' mid-upload and remove them when next queried.
#'
#' @name file_states
NULL

#' @param remote_path DNA nexus file identifier or DNA nexus file/folder path
#'    (Syntax: "projectID:/folder/path", "/folder/path", or "folder/path").
#'    Paths are relative to the current working directory in the current project
#'    unless otherwise specified.
#'
#' @name remote_path
NULL

#' @param from_metadata logical; if TRUE, assumes the `remote_path` actually
#'    contains the metadata associated with that location extracted by
#'    [dx_get_metadata()]
#'
#' @name from_metadata
NULL
