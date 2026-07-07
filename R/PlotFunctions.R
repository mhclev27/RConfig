options(ragg.max_dim = 100000)


#' Sanitize a string for use as a filename
#'
#' Replaces any character that isn't alphanumeric, `_`, `.`, or `-` with `_`,
#' and truncates to `max_n` characters.
#'
#' @param x String to sanitize.
#' @param max_n Maximum output length.
#' @return The sanitized filename string.
#' @export

safe_filename = function(x, max_n = 180) {
  x = gsub("[^[:alnum:]_\\.-]+", "_", x)       
  if (nchar(x) > max_n) substr(x, 1, max_n) else x
}


#' Create a directory if it doesn't exist and return its normalized path
#'
#' @param path Directory path to ensure exists.
#' @return The normalized path (forward slashes), created if it didn't already exist.
#' @export

ensure_dir_quiet = function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}


#' Save a ggplot with a companion PDF, RDS, source data, and README
#'
#' Saves `plot` to `dir` as `type` (default .jpeg), plus a companion PDF
#' (unless `type` is already .pdf), the plot object as an RDS, any data
#' frames in `dfs` as CSVs, and an optional `desc` as a readme.txt — all
#' inside a subfolder named after `title`.
#'
#' @param title Base filename (without extension) and subfolder name.
#' @param plot ggplot object to save. Defaults to `last_plot()` if NULL.
#' @param dir Output directory.
#' @param w,h Width/height to save at.
#' @param s Scale multiplier applied to `w`/`h`.
#' @param folders Unused (reserved).
#' @param additional_info Option to save plot object, dfs, description readme. Default is TRUE
#' @param dfs Named list of data frames to save alongside the plot as CSVs.
#' @param dpi Resolution for raster output.
#' @param desc Optional description saved as `readme.txt` alongside the plot.
#' @param type File extension for the primary saved plot (default ".jpeg").
#' @param units Units for `w`/`h`: "px"/"pixels", or anything else treated as inches.
#' @return Invisible; called for its side effects (files written to disk).
#' @export


gsave = function(title,
                 plot = NULL,
                 dir, 
                 w,
                 h,
                 s=1,
                 folders = NULL,
                 dfs,
                 dpi = 400,
                 additional_info = TRUE,
                 desc = NULL,
                 type = ".jpeg",
                 units = "inches"){
  
  dir_loc = spath(dir)
  
  if (!dir.exists(as.character(dir_loc))) {
    print(paste0("Creating directory ", dir_loc))
    folder.path(dir_loc)
  }
  
  

  final_path = file.path(dir_loc, paste0(title,type))
  
  cat("Saving file to", final_path)
  if(is.null(plot)){plot = last_plot()}
  
  if (tolower(units) %in% c("px", "pixels")) {
    units_used = "in"
    w_in = w / 100 * s
    h_in = h / 100 * s
  } else {
    units_used = "in"
    w_in = w * s
    h_in = h * s
  }
  
  ggsave(final_path, 
         plot = plot, 
         width = w_in, 
         height = h_in, 
         units = units_used, 
         dpi = dpi,
         limitsize = F)
  
  # Save additional components
  if(additional_info){
    additional_info = file.path(dir_loc, paste0(safe_filename(title)))
    additional_info = ensure_dir_quiet(additional_info)
    
    if(type != ".pdf"){
      ggsave(file.path(additional_info, paste0(title,".pdf")), 
             plot = plot, 
             width = w_in, 
             height = h_in, 
             units = units_used, 
             limitsize = F,
             device = cairo_pdf)
    }
    
    cat("\nsaved!")
    
    
    if(!is.null(dfs)){
      imap(dfs, function(df, name){
        write.csv(as.df(df), file = file.path(additional_info, paste0("plotDF_", name, ".csv")),
                  row.names = F)
      })
    }
    
    saveRDS(plot, file = file.path(additional_info, paste0("plotObject.RDS")))
    
    if (!is.null(desc)) {
      
      if(!is.null(dfs)){
        desc = paste0(desc, "\n\nAssociated dfs include:\n\n", paste(names(dfs), collapse = "\n")) 
      }
      
      readme_path = file.path(additional_info, paste0("readme.txt"))
      print("saving your README")
      writeLines(desc, con = readme_path)
    }
    
  }
  
}


#' Name a list of objects by their argument expressions
#'
#' Like `list(...)`, but automatically names each element after the
#' expression passed in (so `df_list(a, b)` is equivalent to
#' `list(a = a, b = b)`). Also accepts a single pre-built named list or an
#' inline `list(...)` call.
#'
#' @param ... Objects to bundle into a named list.
#' @return A named list.
#' @export

df_list = function(...) {
  mc = match.call(expand.dots = FALSE)
  dots = list(...)
  deparse1 = function(x) paste(deparse(x), collapse = "")
  
  supplied_names = names(dots)
  if (is.null(supplied_names)) supplied_names = rep("", length(dots))
  
  # If a single thing was passed
  if (length(dots) == 1) {
    x = dots[[1]]
    
    # Explicit name given (e.g. df_list(df_name = df)) -> use it directly
    if (nzchar(supplied_names[1])) {
      return(setNames(list(x), supplied_names[1]))
    }
    
    # If it is a list() call inline, name from its args
    if (is.list(x) && !is.data.frame(x)) {
      expr = mc$...[[1]]
      if (is.call(expr) && identical(expr[[1]], as.name("list"))) {
        out = x
        nms = vapply(as.list(expr)[-1L], deparse1, character(1))
        names(out) = nms
        return(out)
      }
      # Pre-made unnamed list
      if (is.null(names(x))) stop("Unnamed list passed. Name its elements or pass objects directly.")
      return(x)
    }
    
    # Single non-list or data.frame, no explicit name: wrap and name by symbol
    nm = deparse1(mc$...[[1]])
    return(setNames(list(x), nm))
  }
  
  # Variadic: use supplied names where given, else name by symbol
  nms = vapply(seq_along(dots), function(i) {
    if (nzchar(supplied_names[i])) supplied_names[i] else deparse1(mc$...[[i]])
  }, character(1))
  names(dots) = nms
  dots
}