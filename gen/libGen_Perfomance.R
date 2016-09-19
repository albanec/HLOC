# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Функции для оптимизации производительности:
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#
#' Copy arguments into env and re-bind any function's lexical scope to bindTargetEnv .
#' 
#' @param bindTargetEnv environment to bind to
#' @param objNames additional names to lookup in parent environment and bind
#' @param names of functions to NOT rebind the lexical environments of
BindToEnv <- function (bindTargetEnv = parent.frame(), objNames, doNotRebind = c()) {
  # Bind the values into environment
  # and switch any functions to this environment!
  for (var in objNames) {
    val <- get(var, envir = parent.frame())
    if (is.function(val) && (!(var %in% doNotRebind))) {
      # replace function's lexical environment with our target (DANGEROUS)
      environment(val) <- bindTargetEnv
    }
    # assign object to target environment, only after any possible alteration
    assign(var, val, envir = bindTargetEnv)
  }
}
