#' Diagnose Hession
#'
#' By Ben Ben Bolker
#' https://cran.r-project.org/web/packages/glmmTMB/vignettes/troubleshooting.html
#'
#' @param fit Fitte glmmTMB object
#' @param h h
#' @param eval.eps eva.eps
#' @param evec.eps evec.eps
#' @export


diagnose_hessian <- function(fit,
                             h=NULL,
                             eval.eps=1e-5,
                             evec.eps=1e-2) {
  ## pull out the TMB object from the fit
  obj <- fit$obj
  ee <- environment(obj$fn)
  ## extract parameters
  pp <- ee$last.par[-ee$random]
  ## easiest way to get names corresponding to all of the parameters
  nn <- tryCatch(colnames(vcov(fit,full=TRUE)),
                 ## fall-back position
                 error = function(e) make.unique(names(pp)))
  ## fit$sdr$pdHess
  if ("sdr" %in% names(fit)) {
    cat("bad params according to sdreport:",
        paste(nn[!is.finite(suppressWarnings(sqrt(diag(fit$sdr$cov.fixed))))],
              collapse=", "),"\n")
  }
  ## two ways to compute the Hessian
  ## (1) directly from the objective function, via finite difference+Richardson extrapolation
  ## h1 <- hessian(obj$fn, pp)
  ## (2) use the gradient and compute its Jacobian (faster and probably more stable)
  if (is.null(h)) {
    if (!require(numDeriv)) stop("need numDeriv package installed")
    h <- jacobian(obj$gr, pp)
  }
  ## double-check we get the same answer (approximately)
  ## all.equal(h1,h,tolerance=1e-5)
  ## now investigate the Hessian
  eigs <- eigen(h)
  ## non-positive definite means some of the eigenvectors are <= 0
  bad <- which(eigs$values/max(eigs$values)<=eval.eps)
  if (length(bad)==0) {
    cat("Hessian seems OK\n")
    return(invisible(h))
  }
  cat(sprintf("max eigenvalue = %1.3g",eigs$values[1]),"\n")
  for (b in bad) {  ## there could be more than one 'bad' direction/eigenvector ..
    cat(sprintf("Hessian eigenvalue %d = %1.3g (relative val = %1.3g)",
                b,eigs$values[b],eigs$values[b]/eigs$values[1]),"\n")
    bad_vec <- eigs$vectors[,b]
    bad_elements <- which(abs(bad_vec)>evec.eps)
    cat("   bad elements:",nn[bad_elements],"\n")
  }
  cat("SDs computed from sqrt(diag(solve(H))):",
      paste(suppressWarnings(sqrt(diag(solve(h)))), collapse=", "),"\n")
  return(invisible(h))
}

