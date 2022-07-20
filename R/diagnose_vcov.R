#' Diagnose vacriance-covariance matrix
#'
#' By Ben Ben Bolker
#' https://cran.r-project.org/web/packages/glmmTMB/vignettes/troubleshooting.html
#'
#' See also diagnose_hessian
#'
#' @param model Fitted glmmTMB object
#' @param tol tol
#' @param digits digits
#' @param analyze_hessian evec.eps
#' @export



diagnose_vcov <- function(model, tol=1e-5, digits=2, analyze_hessian=FALSE) {
  vv <- vcov(model, full=TRUE)
  nn <- rownames(vv)
  if (!all(is.finite(vv))) {
    if (missing(analyze_hessian)) warning("analyzing Hessian, not vcov")
    if (!analyze_hessian) stop("can't analyze vcov")
    analyze_hessian <- TRUE
  }
  if (analyze_hessian) {
    par.fixed <- model$obj$env$last.par.best
    r <- model$obj$env$random
    if (!is.null(r)) par.fixed <- par.fixed[-r]
    vv <- optimHess(par.fixed, fn=model$obj$fn, gr=model$obj$gr)
    ## note vv is now HESSIAN, not vcov
  }
  ee <- eigen(vv)
  if (all(ee$values>tol)) {message("var-cov matrix OK"); return(invisible(NULL))}
  ## find negative or small-positive eigenvalues (flat/wrong curvature)
  bad_evals <- which(ee$values<tol)
  ## order worst to best
  bad_evals <- bad_evals[order(-ee$values[bad_evals])]
  ret <- lapply(bad_evals,
                function(i) {
                  ## extract loadings
                  v <- setNames(ee$vectors[,i], nn)
                  ## order in decreasing magnitude & round
                  list(val=ee$values[i],vec=round(v[order(-abs(v))],digits))
                })
  return(ret)
}
