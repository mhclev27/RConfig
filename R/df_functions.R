
# --- PREVIEW FUNCTIONS ----------------------------------------------------------------------------

#' Preview the first (or all) rows of a data frame
#'
#' Coerces `df` to a data.frame and returns the first `n` rows. Pass
#' `n = "a"` (or "all"/"A"/"ALL"/"All"/"f") to return every row.
#'
#' @param df Object coercible to a data frame.
#' @param n Number of rows to preview, or "a"/"all"/"A"/"ALL"/"All"/"f" for all rows.
#' @return A data frame of the previewed rows.
#' @export

prev = function(df, n=5){
  
  if(n %in% c("a", "all", "A", "ALL", "All", "f")){
    n_use = nrow(df)
  } else{
    n_use = n
  }
  
  
  df %>%
    as.data.frame() %>%
    head(n_use)
}


#' Preview column-level structure of a data frame
#'
#' For each column, prints its class, number of unique values, and either
#' a numeric range (for numeric columns) or a preview of unique values
#' (for everything else, capped at `max_preview`). Then prints `prev(df)`.
#'
#' @param df Data frame to inspect.
#' @param max_preview Max number of unique values to preview per non-numeric column.
#' @return Invisible `NULL`; output is printed via `cat()`.
#' @export


prev2 = function(df, max_preview = 100) {
  for (col in names(df)) {
    x = df[[col]]
    n_unique = length(unique(x))
    
    if (is.numeric(x)) {
      rng = range(x, na.rm = TRUE)
      cat(sprintf(
        "$ %s: numeric | unique: %d | range: [%s, %s]\n\n",
        col, n_unique, format(rng[1]), format(rng[2])
      ))
    } else {
      # use plain if/else, not ifelse() (we want a single branch, not vectorized)
      if (length(x) > 1000) {
        u = unique(x)
      } else {
        # sort if sortable; if not, just unique
        can_sort = is.atomic(x) && !is.list(x)
        u = if (can_sort) sort(unique(x)) else unique(x)
      }
      preview_vals = head(u, max_preview)
      preview_str  = paste(preview_vals, collapse = ", ")
      previewed = min(max_preview, n_unique)
      cat(sprintf(
        "$ %s: %s | unique: %d | preview (%d of %d): %s\n\n",
        col, class(x)[1], n_unique, previewed, n_unique, preview_str
      ))
    }
  }
  invisible(NULL)
  prev(df)
}



#' Preview the top-left corner of a matrix
#'
#' @param matrix Matrix to preview.
#' @param r,c Number of rows/columns to show.
#' @return The `matrix[1:r, 1:c]` subset.
#' @export

mprev = function(matrix, r=2, c=2){
  matrix[1:r,1:c]
}

# --- TRANSFORM FUNCTIONS ---------------------------------------------------------------

#' Coerce to data frame
#'
#' @param x Object to coerce with `as.data.frame()`.
#' @return The coerced data frame.
#' @export

as.df = function(x){
  x = as.data.frame(x)
  return(x)
}


# --- INDEXING FUNCTIONS -----------------------------------------------------------------


#' Compare two vectors and print their overlap/difference as a summary
#'
#' Prints shared elements, elements unique to `x`, and elements unique to
#' `y`, each with percentages, wrapped to lines of 6 items for readability.
#'
#' @param x,y Vectors to compare.
#' @return Invisible `NULL`; output is printed via `cat()`.
#' @export

doub_diff = function(x, y) {
  # helper: collapse with newline every n items
  collapse_n = function(vec, n = 6) {
    if (length(vec) == 0) return("(none)")
    split_vec <- split(vec, ceiling(seq_along(vec) / n))
    paste(vapply(split_vec, function(s) paste(s, collapse = ", "), ""), collapse = "\n")
  }
  
  # sets
  shared  = sort(intersect(x, y))
  forward = sort(setdiff(x, y))
  reverse = sort(setdiff(y, x))
  
  # totals
  n1 = length(unique(x))
  n2 = length(unique(y))
  
  # messages
  shared_msg  = collapse_n(shared)
  forward_msg = collapse_n(forward)
  reverse_msg = collapse_n(reverse)
  
  # percentages
  shared_pct  = if (n1 > 0) round(100*length(shared)/n1, 1) else 0
  forward_pct = if (n1 > 0) round(100*length(forward)/n1, 1) else 0
  reverse_pct = if (n2 > 0) round(100*length(reverse)/n2, 1) else 0
  
  cat(sprintf(
    "Shared (%s%%, %s out of %s total in 1):\n%s\n\n",
    shared_pct, length(shared), n1, shared_msg
  ))
  
  cat(sprintf(
    "In First item, but not second (%s%%, %s out of %s total in 1):\n%s\n\n",
    forward_pct, length(forward), n1, forward_msg
  ))
  
  cat(sprintf(
    "In Second item, but not first (%s%%, %s out of %s total in 2):\n%s\n",
    reverse_pct, length(reverse), n2, reverse_msg
  ))
}



#' Subset x to elements also present in y
#'
#' @param x Vector to subset.
#' @param y Vector of values to keep.
#' @return Elements of `x` that are also in `y`.
#' @export

In = function(x,y){
  x[x %in% y]
}


#' Subset x to elements NOT present in y
#'
#' @param x Vector to subset.
#' @param y Vector of values to exclude.
#' @return Elements of `x` that are not in `y`.
#' @export

NotIn = function(x,y){
  x[x %!in% y]
}


# Negated %in% (no formal Rd page: roxygen2 can't cleanly document operators
# containing "!" — see checking Rd files warning in R CMD check).
#' @export
#' @noRd

`%!in%` = Negate(`%in%`)



# --- MAPPING FUNCTIONS -------------------------------------------------------------------

#' map2 + bind_rows in one step
#'
#' Applies `.f` element-wise over `.x`/`.y` (like `purrr::map2`) and
#' row-binds the results into a single data frame.
#'
#' @param .x,.y Vectors/lists to map over in parallel.
#' @param .f Function applied to each `.x`/`.y` pair.
#' @param ... Additional arguments passed to `.f`.
#' @param .id Optional column name to store the names of `.x`/`.y` in the output.
#' @return A single data frame from row-binding all results.
#' @export

map2_dfr = function(.x, .y, .f, ..., .id = NULL) {
  map2(.x, .y, .f, ...) %>%
    bind_rows(.id = .id)
}



