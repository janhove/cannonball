#' @name test_statistics
#' @aliases mean_diff
#' @aliases stud_mean_diff
#' @aliases median_diff
#' @aliases rank_sum
#' @aliases prob_super
#' @aliases ks_stat
#' @title Test statistics for two-group comparisons
#'
#' @description These functions compute test statistics that can be used
#' in a randomisation test for a two-group comparison.
#'
#' \code{mean_diff()} computes the mean difference.
#'
#' \code{stud_mean_diff()} computes the studentised mean difference.
#' This is identical to the t-statistic used in Welch's t-test.
#'
#' \code{median_diff()} computes the difference between the medians.
#'
#' \code{rank_sum()} computes the rank sum in the treatment group.
#' This is the test statistic used in the Mann-Whitney-Wilcoxon test.
#'
#' \code{prob_super()} computes the probability of superiority, i.e.,
#' the probability that a randomly chosen unit in the treatment group
#' has a higher outcome value than a randomly chosen unit in the control group.
#' The resulting test is equivalent to when using \code{rank_sum()}.
#'
#' \code{ks_stat()} computes the Kolmogorov-Smirnov test statistic, i.e.,
#' the supremum of the difference between the ecdfs of the outcome in the
#' treatment and control groups.
#'
#' @param outcome A vector of numeric outcomes.
#' @param treatment_idx The indices of the observations in the treatment group.
#'
#' @return The test statistic for the two-group comparison.
#' @export
#'
#' @examples
#' x <- c(seq(-4, 5), 2*seq(-3, 4))
#' g <- rep(c(0, 1), times = c(10, 8))
#' idx <- which(g == 1)
#' mean_diff(x, idx)
#' stud_mean_diff(x, idx)
#' median_diff(x, idx)
#' rank_sum(x, idx)
#' prob_super(x, idx)
#' ks_stat(x, idx)
#'
#' @rdname test_statistics
#' @export
mean_diff <- function(outcome, treatment_idx) {
  mean(outcome[treatment_idx]) - mean(outcome[-treatment_idx])
}
#' @rdname test_statistics
#' @export
stud_mean_diff <- function(outcome, treatment_idx) {
  control_idx <- setdiff(seq_along(outcome), treatment_idx)
  V_hat <- stats::var(outcome[treatment_idx]) / length(treatment_idx) +
    stats::var(outcome[control_idx]) / length(control_idx)
  mean_diff(outcome, treatment_idx) / sqrt(V_hat)
}
#' @rdname test_statistics
#' @export
median_diff <- function(outcome, treatment_idx) {
  stats::median(outcome[treatment_idx]) - stats::median(outcome[-treatment_idx])
}
#' @rdname test_statistics
#' @export
rank_sum <- function(outcome, treatment_idx) {
  ranks <- rank(outcome)
  sum(ranks[treatment_idx])
}
#' @rdname test_statistics
#' @export
prob_super <- function(outcome, treatment_idx) {
  n1 <- length(treatment_idx)
  n2 <- length(outcome) - n1
  (rank_sum(outcome, treatment_idx) - n1 * (n1 + 1)/2) / (n1 * n2)
}
#' @rdname test_statistics
#' @export
ks_stat <- function(outcome, treatment_idx) {
  control_idx <- setdiff(seq_along(outcome), treatment_idx)
  ks <- suppressWarnings(
    stats::ks.test(outcome[treatment_idx], outcome[control_idx], exact = FALSE, B = 1)$statistic
  )
  unname(ks)
}
