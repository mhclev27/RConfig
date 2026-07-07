

#' Get the parent directory of a path, whether it points to a file or folder
#'
#' Heuristic: if the last path component has a file extension, returns its
#' parent directory; otherwise treats the path itself as a folder and
#' returns it unchanged. Does not require the path to exist.
#'
#' @param path_str Path to check.
#' @return The parent directory (if `path_str` looks like a file), or `path_str` itself.
#' @export
get_parent_if_file = function(path_str) {
  # normalize, but don't require it to exist
  p = normalizePath(path_str, winslash = "/", mustWork = FALSE)
  
  # check the "basename" part
  fname = basename(p)
  
  # heuristic: if there's an extension, treat as file
  if (grepl("\\.[^.]+$", fname)) {
    return(dirname(p))   # return parent folder
  } else {
    return(p)            # looks like a folder
  }
}

#' Build a file path, creating its parent directory if needed
#'
#' Like `file.path(...)`, but ensures the resulting path's parent directory
#' exists (creating it recursively if necessary) before returning the path.
#'
#' @param ... Path components, as in `file.path()`.
#' @return The joined path (directory created as a side effect if missing).
#' @export
folder.path = function(...){
  path = file.path(...)
  
  
  path_dir = get_parent_if_file(path)
  
  if(!dir.exists(path_dir)){printg("Creating folder {path_dir}")} else{ printg("Path already exists: {path_dir}") }
  dir.create(path_dir, showWarnings = F, recursive = T)
  if(!dir.exists(path_dir)){printg("FAILED TO CREATE: {path_dir}")} 
  
  return(path)
}



#' Read a CSV, printing an accompanying README if one exists
#'
#' Reads `file` with `read.csv()` (after resolving it via `spath()`), first
#' printing the contents of a same-named `*_readme.txt` file alongside it,
#' if present. Also prints a preview of the loaded data via `prev()`.
#'
#' @param file Path to the CSV file.
#' @return The loaded data frame.
#' @export
read.csv2 = function(file) {
  file = spath(file)
  # Try reading the README
  file_base = tools::file_path_sans_ext(basename(file))
  parent_dir = dirname(spath(file))
  readme_path = file.path(parent_dir, paste0(file_base, "_readme.txt"))
  
  if (file.exists(readme_path)) {
    cat("\n--- README ---\n")
    cat(readLines(readme_path), sep = "\n")
    cat("\n--------------\n")
  }
  
  # Load the CSV
  df = read.csv(file = spath(file))
  
  print(prev(df))
  
  return(df)
}


#' Save an R object to disk, creating its directory if needed
#'
#' Like `save()`, but resolves `file` via `spath()` and creates the parent
#' directory first if it doesn't already exist.
#'
#' @param obj Object to save.
#' @param file Path to save to.
#' @return Invisible; called for its side effect of writing `file`.
#' @export
ssave = function(obj, file){
  path = spath(file)
  base=dirname(path)
  if (!dir.exists(as.character(base))) {
    print(paste0("Creating directory ", base))
    dir.create(base, recursive = TRUE)
  }
  printg("Saving object to {path}")
  save(obj, file = path)
}