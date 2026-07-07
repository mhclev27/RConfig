

# --- FUNCTIONS FOR INLINE MESSAGING ----------------------


#' Print a message padded with blank lines
#'
#' @param msg Message text to print.
#' @return Invisible `NULL`; called for the side effect of printing.
#' @export


padMsg = function(msg){
  message("\n\n", msg, "\n\n")
}
