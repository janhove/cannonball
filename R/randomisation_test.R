#' Randomisation testing for two-group comparisons
#'
#' Computes p-values under the strong null hypothesis of no differential
#' treatment effect for any unit.
#'
#' @param outcome Vector of numeric outcomes.
#' @param treatment_idx Indices of observations in treatment condition.
#' @param block When using blocking, specifies which units are blocked together.
#' @param statistic A test statistic for a two-group comparison (see \link{test_statistics}).
#' @param exact If `TRUE` (default), exhaustive rerandomisation is used. Else, Monte Carlo rerandomisation.
#' @param plot If `TRUE` (default), the distribution of test statistics under H0 is plotted.
#' @param M Number of Monte Carlo samples (only if `exact == FALSE`).
#' @param xlab x-axix label for the histogram showing the H0 distribution of the test statistic.
#'
#' @return A left-, right, and two-sided p-value.
#' @export
#'
#' @examples
#' learners <- data.frame(
#' L1 = c(rep("German", 6), rep("French", 8)),
#' Learner = c(1:6, 1:8)
#' )
#'
#' learners$Learner <- paste(learners$L1, learners$Learner)
#' learners$MethodB <- round(rnorm(14, mean = 9, sd = 1))
#' learners$MethodB <- ifelse(learners$L1 == "German",
#'                            learners$MethodB + 3,
#'                            learners$MethodB)
#' differences <- rep(c(2, 1, 3, 0, 4), times = c(8, 3, 3, 0, 0)) |> sample()
#' learners$MethodA <- learners$MethodB - differences
#' my_min <- pmin(min(learners$MethodA, learners$MethodB), 0)
#' learners$MethodA <- learners$MethodA - my_min
#' learners$MethodB <- learners$MethodB - my_min
#' learners$Condition <- c(sample(rep(c(0, 1), each = 3)),
#'                         sample(rep(c(0, 1), each = 4)))
#' learners$Outcome <- learners$Condition * learners$MethodB +
#'   (1 - learners$Condition) * learners$MethodA
#'
#' # taking blocking into account (exhaustive randomisation)
#' rand_test(learners$Outcome, which(learners$Condition == 1),
#'           block = learners$L1, statistic = mean_diff)
#'
#' # without taking blocking into account
#' rand_test(learners$Outcome, which(learners$Condition == 1),
#'           statistic = mean_diff)
rand_test <- function(
    outcome,
    treatment_idx,
    block = NULL,
    statistic = stud_mean_diff,
    exact = TRUE,
    plot = TRUE,
    M = 20000,
    xlab = "test statistic"
) {
  if (exact && is.null(block)) {
    return(exh_rerand_pval(outcome, treatment_idx, statistic, plot, xlab))
  }
  if (!exact && is.null(block)) {
    return(mc_pval(outcome, treatment_idx, statistic, plot, M = M, xlab))
  }
  if (exact) {
    return(exh_rerand_pval_blocking(outcome, treatment_idx, block,
      statistic, plot, xlab))
  }
  mc_pval_blocking(outcome, treatment_idx, block,
    statistic, M, plot, xlab)
}

#' Compute p-value through exhausitive rerandomisation
#'
#' @noRd
exh_rerand_pval <- function(
    outcome,
    treatment_idx,
    statistic = stud_mean_diff,
    plot = TRUE,
    xlab
) {
  if (choose(length(outcome), length(treatment_idx)) > 2e5) {
    warning("Large number of rerandomisations. Consider using Monte Carlo approach.")
  }
  obs_stat <- statistic(outcome, treatment_idx)

  n <- length(outcome)
  rerandomisations <- utils::combn(n, length(treatment_idx))
  H0_stats <- apply(rerandomisations, 2, statistic, outcome = outcome)

  if (plot) plot_H0(obs_stat, H0_stats, xlab)

  compute_p(obs_stat, H0_stats)
}

#' Compute p-value through Monte Carlo rerandomisation
#'
#' @noRd
mc_pval <- function(
    outcome,
    treatment_idx,
    statistic = stud_mean_diff,
    M = 20000,
    plot = TRUE,
    xlab
) {
  obs_stat <- statistic(outcome, treatment_idx)

  H0_stats <- replicate(M - 1, {
    treatment_idx <- sample(1:length(outcome), length(treatment_idx))
    statistic(outcome, treatment_idx)
  })
  H0_stats <- c(obs_stat, H0_stats)

  if (plot) plot_H0(obs_stat, H0_stats, xlab)

  compute_p(obs_stat, H0_stats)
}

#' Compute p-value for an experiment using blocking through exhausitive rerandomisation
#'
#' @noRd
exh_rerand_pval_blocking <- function(
    outcome,
    treatment_idx,
    block,
    statistic = mean_diff,
    plot = TRUE,
    xlab
) {
  obs_stat <- statistic(outcome, treatment_idx)
  blocks <- sort(unique(block))
  k <- length(blocks)
  block_indices <- lapply(blocks, function(b) which(block == b))
  n_treated <- sapply(block_indices, function(idx) sum(idx %in% treatment_idx))

  block_combos <- lapply(seq_len(k), function(i) {
    utils::combn(block_indices[[i]], n_treated[i], simplify = FALSE)
  })

  combo_grid <- expand.grid(lapply(block_combos, seq_along))

  H0_stats <- apply(combo_grid, 1, function(row) {
    treat_idx <- unlist(
      Map(function(i, j) block_combos[[i]][[j]], seq_len(k), row)
    )
    statistic(outcome, treat_idx)
  })

  if (plot) plot_H0(obs_stat, H0_stats, xlab)

  compute_p(obs_stat, H0_stats)
}

#' Compute p-value for an experiment using blocking through Monte Carlo sampling
#'
#' @noRd
mc_pval_blocking <- function(
    outcome,
    treatment_idx,
    block,
    statistic = mean_diff,
    M = 20000,
    plot = TRUE,
    xlab
) {
  obs_stat <- statistic(outcome, treatment_idx)
  blocks <- sort(unique(block))
  k <- length(blocks)
  block_indices <- lapply(blocks, function(b) which(block == b))
  n_treated <- sapply(block_indices, function(idx) sum(idx %in% treatment_idx))

  rerandomise <- function() {
    unlist(Map(function(idx, n) sample(idx, n), block_indices, n_treated))
  }

  H0_stats <- replicate(M - 1, {
    treat_idx <- rerandomise()
    statistic(outcome, treat_idx)
  })
  H0_stats <- c(obs_stat, H0_stats)

  if (plot) plot_H0(obs_stat, H0_stats, xlab)

  compute_p(obs_stat, H0_stats)
}
