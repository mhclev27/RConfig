# Data QC Functions ----------------------------------------------------------


#' Check that a data frame contains required columns
#'
#' Stops with an informative error if any of `cols` are missing from `df`.
#' Optionally returns the data frame subset to just those columns.
#'
#' @param df Data frame to check.
#' @param cols Character vector of column names that must be present.
#' @param df_name Label used in the error message. Defaults to the name of `df`.
#' @param message Optional custom prefix for the error message.
#' @param return_subset If TRUE, returns `df[, cols]` instead of the full `df`.
#' @return `invisible(TRUE)` if all `cols` are present, or a returns `df[, cols]` if `return_subset`; otherwise errors.
#' @export


check_cols = function(
    df,
    cols,
    df_name = deparse(substitute(df)),
    message = NULL,
    return_subset = FALSE
) {
  missing_cols = setdiff(cols, names(df))
  
  if (length(missing_cols) > 0) {
    
    if (is.null(message)) {
      stop(
        paste0(
          "Missing columns in ", df_name, ": ",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    } else {
      stop(
        paste0(
          message,
          " [", df_name, "] Missing columns: ",
          paste(missing_cols, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }
  
  if (return_subset) {
    return(df[, cols, drop = FALSE])
  }
  
  return(invisible(TRUE))
}



#' Check that all expected elements are present in a column
#'
#' Stops with an informative error if any of `elements` are missing from
#' the unique values of `df[[col]]`. Optionally returns the rows matching
#' `elements` instead of just validating.
#'
#' @param col Column name (string) to check values of.
#' @param df Data frame to check.
#' @param elements Values that must all be present in `df[[col]]`.
#' @param df_name Label used in the error message. Defaults to the name of `df`.
#' @param message Optional custom prefix for the error message.
#' @param return_subset If TRUE, returns `df` filtered to rows where `col` is in `elements`.
#' @return `invisible(TRUE)` if all elements are present (or the filtered subset
#'   if `return_subset = TRUE`); otherwise errors.
#' @export


check_col_elements = function(
    col,
    df,
    elements,
    df_name = deparse(substitute(df)),
    message = NULL,
    return_subset = FALSE
) {
  missing_elements = setdiff(elements, unique(df[[col]]))
  
  if (length(missing_elements) > 0) {
    stop(
      paste0(
        message, "\n",
        "Missing in ", df_name, ": ",
        paste(missing_elements, collapse = ", ")
      ),
      call. = FALSE
    )
  } else if (return_subset) {
    return(df[df[[col]] %in% elements, , drop = FALSE])
  }
  
  return(invisible(TRUE))
}


#' Check that two vectors have no overlapping values
#'
#' Stops with an informative error listing any values shared between
#' `vec1` and `vec2`.
#'
#' @param vec1,vec2 Vectors to compare.
#' @param message Custom prefix for the error message.
#' @return `invisible(TRUE)` if there is no overlap; otherwise errors.
#' @export

check_no_overlap = function(
    vec1,
    vec2,
    message = "Overlap found"
) {
  overlap = intersect(vec1, vec2)
  
  if (length(overlap) > 0) {
    stop(
      message,
      "\nNumber overlapping: ", length(overlap),
      "\nOverlapping values: ", paste(overlap, collapse = ", ")
    )
  }
  
  return(invisible(TRUE))
}