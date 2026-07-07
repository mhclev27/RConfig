
# ----- STYLER FUNCTIONS  -----------------------------------------------

#' Lint a script with project conventions applied
#'
#' Runs `lintr::lint()` with `assignment_linter`, `line_length_linter`,
#' `object_name_linter`, and `pipe_consistency_linter` disabled, since
#' this project intentionally uses `=` assignment, `%>%` pipes, long
#' section-header comments, and non-snake_case toptable object names.
#'
#' @param path Path to the R script to lint.
#' @return A `lints` object (see `lintr::lint()`).
#' @export

lint_clean = function(path) {
  lintr::lint(
    path,
    linters = lintr::linters_with_defaults(
      assignment_linter       = NULL,
      line_length_linter      = NULL,
      object_name_linter      = NULL,
      pipe_consistency_linter = NULL
    )
  )
}


#' Style a script without changing assignment operators
#'
#' Runs `styler::style_file()` restricted to `scope = "line_breaks"`, so
#' spacing/indentation get fixed but `=` is never rewritten to `<-`.
#'
#' @param path Path to the R script to style.
#' @return Invisible; called for its side effect of rewriting `path`.
#' @export

style_clean = function(path) {
  styler::style_file(
    path,
    scope = "line_breaks"
  )
}