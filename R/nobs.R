#' @exportS3Method nobs semtree
nobs.semtree <- function(object, ...) {
  if (!inherits(object, "semtree"))
    return(NULL)
  object$N
} 