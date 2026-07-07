
#' Remove objects from an environment and optionally run garbage collection
#'
#' Removes `objects` (or everything except `keep`, if `objects` is NULL)
#' from `envir`, then optionally runs `gc()` and/or restarts the R session.
#'
#' @param objects Character vector of object names to remove. If NULL,
#'   removes everything in `envir` except `keep`.
#' @param keep Character vector of object names to preserve when `objects` is NULL.
#' @param envir Environment to remove objects from. Defaults to `.GlobalEnv`.
#' @param run_gc If TRUE (default), runs `gc()` after removal.
#' @param restart If TRUE, restarts the R session (RStudio only) after cleanup.
#' @param verbose If TRUE (default), messages what was removed.
#' @return Invisible `NULL`, or the result of `gc()` if `run_gc = TRUE`.
#' @export



remove_objects = function(objects = NULL,
                          keep = NULL,
                          envir = .GlobalEnv,
                          run_gc = TRUE,
                          restart = F,
                          verbose = TRUE) {
  
  # Current objects in environment
  current_objects = ls(envir = envir, all.names = TRUE)
  
  # If objects not supplied, remove everything except keep
  if (is.null(objects)) {
    objects = setdiff(current_objects, keep)
  }
  
  # Keep only objects that actually exist
  objects = intersect(objects, current_objects)
  
  # Nothing to remove
  if (length(objects) == 0) {
    if (verbose) {
      message("No matching objects found.")
    }
    
    if (run_gc) {
      gc()
    }
    
    return(invisible(NULL))
  }
  
  # Remove objects
  rm(list = objects, envir = envir)
  
  if (verbose) {
    message("Removed: ", paste(objects, collapse = ", "))
  }
  
  # Garbage collection
  if (run_gc) {
    gc_out = gc()
    
    if (verbose) {
      message("Garbage collection completed.")
    }
    
    return(invisible(gc_out))
  }
  
  if(restart){
    .rs.restartR()
  }
  
  invisible(NULL)
}