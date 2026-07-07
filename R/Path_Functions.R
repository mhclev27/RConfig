
# Path Functions ----------------------------------------------------------

#' Translate a path between local and cluster environments
#'
#' Rewrites `Z:/` or `X:/` drive paths to `/project/...` when running on the
#' lab compute cluster, and rewrites `/project/...` paths back to `Z:/` when
#' running locally on Windows. Lets the same script's file paths work
#' unmodified in either environment.
#'
#' @param path Path to translate.
#' @return The translated path (unchanged if neither environment is detected).
#' @export


spath = function(path) {
  original_path = path

  if (dir.exists("/project/nchevrier/software")) {
    path = sub("^([Z]:/)", "/project/", path)
  }

  if (dir.exists("C:/Users/")) {
    path = sub("^/project/", "Z:/", path)
  }

  if (original_path != path) {
    message("Transformed: ", original_path, " -> ", path)
  }

  return(path)
}
