#' glue transformer that collapses atomic vectors for interpolation
#'
#' Used internally by [printg()] as the `.transformer` for `glue::glue()`.
#' Evaluates `text` and, if the result is an atomic vector of length > 1,
#' collapses it into a comma-separated string (capped at 50 elements).
#'
#' @param text String expression to evaluate.
#' @param envir Environment to evaluate `text` in.
#' @return The evaluated value, or a collapsed string if it was a long atomic vector.
#' @keywords internal
# (glue is loaded centrally via R/config/Libraries.R)
collapse_transformer = function(text, envir) {
  val = eval(parse(text = text), envir)
  if (is.atomic(val) && length(val) > 1) {
    max_n = 50L
    if (length(val) > max_n) {
      val = c(val[seq_len(max_n)], sprintf("... and %d more", length(val) - max_n))
    }
    glue::glue_collapse(val, sep = ", ")
  } else {
    val
  }
}

#' glue() + print(), with vectors auto-collapsed
#'
#' Like `glue::glue()` followed by `print()`, but any `{...}` expression
#' that evaluates to an atomic vector of length > 1 is collapsed into a
#' comma-separated string (see [collapse_transformer()]).
#'
#' @param string Glue string template.
#' @param .envir Environment to evaluate expressions in. Defaults to the caller's frame.
#' @return Invisible; called for the side effect of printing.
#' @export
printg <- function(string, .envir = parent.frame()) {
  tryCatch(
    glue(string, .transformer = collapse_transformer, .envir = .envir) |> print(),
    error = function(e) stop("glue error: ", conditionMessage(e), call. = FALSE)
  )
  cat("\n")
}