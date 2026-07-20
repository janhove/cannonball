#' Set global variables
#'
#' @noRd
utils::globalVariables(c(
  ".", "..sample", ".abs_resid", ".cell", ".fitted", ".resid", ".sample",
  ".sd", ".sqrt_abs_resid", "block", "control", "convergence_error",
  "covariate", "difference", "group", "intervention", "score", "subject",
  "type"
))

#' Compute p-values from H0 statistics
#'
#' @noRd
compute_p <- function(obs_stat, H0_stats, eps = .Machine$double.eps^(1/2)) {
  leftsided <- mean(H0_stats <= obs_stat + eps)
  rightsided <- mean(H0_stats >= obs_stat - eps)
  twosided <- min(2*min(leftsided, rightsided), 1)

  list("left-sided p-value" = leftsided,
       "right-sided p-value" = rightsided,
       "two-sided p-value" = twosided)
}

#' Plot H0 distributions
#'
#' @noRd
plot_H0 <- function(obs_stat, H0_stats, xlab = "test statistic") {
  graphics::hist(
    H0_stats, breaks = 30,
    main = paste0("Test statistic in ", length(H0_stats), " rerandomisations"),
    xlab = xlab
  )
  abline(v = obs_stat, col = "blue", lwd = 2)
}
