# Argument Check Functions --------------------------------------------------------------------


#' Describe a value for an argument-check error message
#'
#' Summarises `x` as its class and length, plus its value when it is a short
#' atomic vector, for use in the "received" part of an error message.
#'
#' @param x Value to describe.
#' @return A single string such as `character of length 1 ("a")`.
#' @keywords internal

describe_received = function(x) {
  if (is.null(x)) {
    return("NULL")
  }

  description = paste0(
    class(x)[1],
    " of length ",
    length(x)
  )

  if (is.atomic(x) && length(x) >= 1 && length(x) <= 3) {
    shown_values = if (is.character(x)) paste0("\"", x, "\"") else as.character(x)
    description = paste0(
      description,
      " (",
      paste(shown_values, collapse = ", "),
      ")"
    )
  }

  description
}


#' Stop with a standard argument-check error
#'
#' @param arg Name of the argument being checked.
#' @param expected What the argument was expected to be.
#' @param received What was received instead.
#' @return Never returns; always errors.
#' @keywords internal

stop_invalid_arg = function(
  arg,
  expected,
  received
) {
  stop(
    paste0(
      "Invalid argument ", arg, ": expected ", expected,
      ", received ", received
    ),
    call. = FALSE
  )
}


#' Check that an argument is a single TRUE or FALSE
#'
#' Stops with an informative error unless `x` is a logical scalar that is not
#' `NA`.
#'
#' @param x Value to check.
#' @param arg Label used in the error message. Defaults to the name of `x`.
#' @return `invisible(x)` if valid; otherwise errors.
#' @export
#' @examples
#' check_flag(TRUE)
#' try(check_flag(NA))
check_flag = function(
  x,
  arg = deparse(substitute(x))
) {
  if (!is.logical(x) || length(x) != 1 || is.na(x)) {
    received = describe_received(x)
    stop_invalid_arg(
      arg = arg,
      expected = "TRUE or FALSE",
      received = received
    )
  }

  invisible(x)
}


#' Check that an argument is a single finite number within bounds
#'
#' Stops with an informative error unless `x` is a numeric scalar that is not
#' `NA`, is finite, lies within `[lower, upper]`, and (optionally) is a whole
#' number.
#'
#' @param x Value to check.
#' @param arg Label used in the error message. Defaults to the name of `x`.
#' @param lower Smallest allowed value (inclusive).
#' @param upper Largest allowed value (inclusive).
#' @param integerish If TRUE, `x` must also be a whole number (any numeric
#'   type, e.g. `3` or `3L`).
#' @param allow_null If TRUE, `NULL` is accepted.
#' @return `invisible(x)` if valid; otherwise errors.
#' @export
#' @examples
#' check_number(
#'   x = 0.5,
#'   lower = 0,
#'   upper = 1
#' )
#' check_number(
#'   x = 3,
#'   integerish = TRUE
#' )
#' try(
#'   check_number(
#'     x = 2,
#'     upper = 1
#'   )
#' )
check_number = function(
  x,
  arg = deparse(substitute(x)),
  lower = -Inf,
  upper = Inf,
  integerish = FALSE,
  allow_null = FALSE
) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }

  expected = if (integerish) "a single finite whole number" else "a single finite number"
  received = describe_received(x)

  if (!is.numeric(x) || length(x) != 1 || !is.finite(x)) {
    stop_invalid_arg(
      arg = arg,
      expected = expected,
      received = received
    )
  }

  if (integerish && x != round(x)) {
    stop_invalid_arg(
      arg = arg,
      expected = expected,
      received = received
    )
  }

  if (x < lower || x > upper) {
    bounds_expected = paste0(
      "a number in [", lower, ", ", upper, "]"
    )
    stop_invalid_arg(
      arg = arg,
      expected = bounds_expected,
      received = received
    )
  }

  invisible(x)
}


#' Check that an argument is a list with unique, non-empty names
#'
#' Stops with an informative error unless `x` is a list (not a data frame)
#' whose elements all have non-empty names, with no name repeated.
#'
#' @param x Value to check.
#' @param arg Label used in the error message. Defaults to the name of `x`.
#' @param allow_null If TRUE, `NULL` is accepted.
#' @param allow_empty If TRUE (default), a zero-length list is accepted.
#' @return `invisible(x)` if valid; otherwise errors.
#' @export
#' @examples
#' settings = list(
#'   a = 1,
#'   b = 2
#' )
#' check_named_list(settings)
#' try(check_named_list(list(1, 2)))
check_named_list = function(
  x,
  arg = deparse(substitute(x)),
  allow_null = FALSE,
  allow_empty = TRUE
) {
  if (is.null(x) && allow_null) {
    return(invisible(x))
  }

  if (!is.list(x) || is.data.frame(x)) {
    received = describe_received(x)
    stop_invalid_arg(
      arg = arg,
      expected = "a named list",
      received = received
    )
  }

  if (length(x) == 0) {
    if (!allow_empty) {
      stop_invalid_arg(
        arg = arg,
        expected = "a non-empty named list",
        received = "an empty list"
      )
    }
    return(invisible(x))
  }

  element_names = names(x)
  if (is.null(element_names)) {
    element_names = rep(
      x = "",
      times = length(x)
    )
  }

  unnamed_positions = which(is.na(element_names) | element_names == "")
  if (length(unnamed_positions) > 0) {
    received = paste0(
      "unnamed elements at positions: ",
      paste(unnamed_positions, collapse = ", ")
    )
    stop_invalid_arg(
      arg = arg,
      expected = "every element to be named",
      received = received
    )
  }

  duplicated_names = unique(element_names[duplicated(element_names)])
  if (length(duplicated_names) > 0) {
    received = paste0(
      "duplicated names: ",
      paste(duplicated_names, collapse = ", ")
    )
    stop_invalid_arg(
      arg = arg,
      expected = "unique element names",
      received = received
    )
  }

  invisible(x)
}
