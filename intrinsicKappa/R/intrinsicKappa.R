###############################################
# --------------------------------------------#
# Intrinsic Kappa                             #
# --------------------------------------------#
###############################################

# --------------------------------------------
# Computation of intrinsic kappa
# --------------------------------------------

#' Compute Statistics
#'
#' @param n1 integer
#' @param n2 integer
#' @param alpha one-sided significance level
#'
computeStat <- function(n1, n2, alpha) {
  # degrees of freedom
  nu1 <- 2*(n1 + 1)
  nu2 <- 2 * n2
  # computation of adjusted alpha value
  alpha_adj <- sqrt(alpha/2)

  # alphas point estimate
  alpha_observed <- n1/(n1 + n2)

  fisher_exact <- qf(1-alpha, nu1, nu2)
  fisher_adj <- qf(1-alpha_adj, nu1, nu2)

  upper_bound_exact <- (nu1 * fisher_exact)/(nu2 + nu1 * fisher_exact)
  upper_bound_adj <- (nu1 * fisher_adj)/(nu2 + nu1 * fisher_adj)

  L <- list('alpha_observed' = alpha_observed,
            'upper_bound_exact' = upper_bound_exact,
            'upper_bound_adj' = upper_bound_adj)

  return(L)
}


#' Intrinsic Kappa
#'
#' @param M matrix to be assessed
#' @param alpha one-sided significance level
#' @param alpha_adjusted logical, whether the significance level shall be adjusted
#'
#' @details
#' Computation of intrinsic kappa with a dichotomous response and known relation of the
#' input frequencies.
#'
#' @return
#' Intrinsic kappa value
#'
#' @references
#' R. Sanchez-Marquez, F. Gerhorst and D. Schindler (2023)
#' "Effectiveness of quality inspections of attributive characteristics – A novel and practical method for estimating the “intrinsic”
#' value of kappa based on alpha and beta statistics." Computers & Industrial Engineering, 109006.
#'
#' @examples
#' M <- matrix(c(2375, 25, 10, 2390), ncol = 2)
#' rownames(M) <- c('ok-rating', 'nok-rating')
#' colnames(M) <- c('ok-standard', 'nok-standard')
#' alpha <- 0.05
#' alpha_adjusted <- FALSE
#' intrinsicKappa(M, alpha, alpha_adjusted)
#'
#' @export
intrinsicKappa <- function(M, alpha=0.05, alpha_adjusted=TRUE) {
  ok_stats <- computeStat(M[2, 1], M[1, 1], alpha)
  nok_stats <- computeStat(M[1, 2], M[2, 2], alpha)

  if (alpha_adjusted) {
    ok_upper_bound <- ok_stats$upper_bound_adj
    nok_upper_bound <- nok_stats$upper_bound_adj
  } else {
    ok_upper_bound <- ok_stats$upper_bound_exact
    nok_upper_bound <- nok_stats$upper_bound_exact
  }

  kappa_lower_bound <- 1 - ok_upper_bound - nok_upper_bound

  L <- list('kappa_lower_bound' = kappa_lower_bound,
            'alpha_observed_ok' = ok_stats$alpha_observed,
            'alpha_observed_nok' = nok_stats$alpha_observed,
            'upper_bound_ok' = ok_upper_bound,
            'upper_bound_nok' = nok_upper_bound,
            'M' = M)

  attr(L, "class") <- "intrinsicKappa"

  return(L)
}


#' @export
print.intrinsicKappa <- function(x, ...) {
  # depiction of matrix
  printmat <- function(mat) {
    out <- capture.output(mat)
    out <- paste("\t", out)
    cat(paste(out, collapse = "\n"))
  }

  cat("\n")
  cat("\t \t Intrinsic Kappa")
  cat("\n")
  cat("\n")
  cat("Input:\n")
  printmat(x$M)
  cat("\n")
  cat("\n")
  cat("Exact Kappa's lower bound:", round(x$kappa_lower_bound, digits=4), "\n")
  cat("\n")
  cat("\t\t  point estimates    upper bound \n")
  cat("alpha (ok units):\t", round(x$alpha_observed_ok, digits=4), "\t", round(x$upper_bound_ok, digits=4), "\n")
  cat("beta (nok units):\t", round(x$alpha_observed_nok, digits=4), "\t", round(x$upper_bound_nok, digits=4), "\n\n")
}

