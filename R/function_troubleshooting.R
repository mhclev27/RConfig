# ---- Deparse functions

#' Bind a function's resolved arguments into the global environment for debugging
#'
#' Given a function `fn` and a set of arguments — named, positional, or a
#' mix, matched the same way R would match a real call to `fn` — evaluates
#' whatever you explicitly supplied and fills in `fn`'s own default values
#' for anything you didn't, then binds the whole resolved argument set
#' directly into `.GlobalEnv`. Lets you step through `fn`'s body line by
#' line as if you were inside a real call to it, without `debug()`/`browser()`.
#'
#' To debug a call like `my_func(a = 1, x)`, change it to
#' `debug_inputs(my_func, a = 1, x)` — insert `fn` as the first argument
#' and keep the rest of the call as-is, rather than renaming `my_func`
#' itself (renaming would lose the information needed to look up defaults).
#'
#' Defaults that reference other parameters are supported (e.g.
#' `function(a, b = a * 2)` resolves `b` correctly using the bound value
#' of `a`). Required arguments (no default) that you don't supply are
#' simply left unbound, since there's nothing to fill them in with.
#'
#' @param fn The function whose call you want to simulate.
#' @param ... Arguments to `fn`, named and/or positional, exactly as you
#'   would call `fn` itself.
#' @return Invisibly, the named list of resolved argument values (also
#'   assigned into `.GlobalEnv` as a side effect).
#' @export

debug_inputs = function(fn, ...) {
  fn_name = substitute(fn)

  mc   = match.call(expand.dots = FALSE)
  dots = mc$...

  # Build a placeholder call `fn_name(...)` so match.call can resolve
  # positional args to their real parameter names using fn's formals.
  pseudo_call   = as.call(c(fn_name, dots))
  matched_call  = match.call(definition = fn, call = pseudo_call)
  supplied      = as.list(matched_call)[-1]

  # Defaults should resolve free variables from where fn was defined,
  # same as real R argument-default semantics.
  enclosing_env = environment(fn)
  if (is.null(enclosing_env)) enclosing_env = parent.frame()

  resolve_env = new.env(parent = enclosing_env)

  for (nm in names(supplied)) {
    assign(nm, eval(supplied[[nm]], envir = parent.frame()), envir = resolve_env)
  }

  fn_formals = formals(fn)
  for (nm in names(fn_formals)) {
    if (!nm %in% names(supplied)) {
      default_expr = fn_formals[[nm]]
      if (!identical(default_expr, quote(expr = ))) {
        assign(nm, eval(default_expr, envir = resolve_env), envir = resolve_env)
      }
    }
  }

  resolved = as.list(resolve_env)
  list2env(resolved, envir = .GlobalEnv)
  invisible(resolved)
}
