#' @title Walkthrough p-values
#'
#' @description This function produces a step-by-step demonstration
#'              of a significance test for a two-group comparison.
#' @param n The number of data points per group.
#' @param diff The boost that participants in the intervention group receive.
#' @param sd The standard deviation of the normal distributions from which the data are drawn.
#' @param showdata Do you want to output a dataframe containing the plotted data (\code{TRUE})
#'                 or not (\code{FALSE}, default)?
#' @param M `NULL` (default) when using exhaustive randomisation testing; else set to the number of Monte Carlo runs desired.
#' @keywords significance test, p-value
#' @details Data are generated from a normal distribution with the requested
#' standard deviation. Then, the data points are randomly assigned to two
#' equal-sized groups. Data points in the intervention group receive a uniform boost
#' as specified by \code{diff}. Finally, a significance test is run on the data.
#' This significance test is a randomisation test using the mean difference as
#' the test statistic. The p-value reported is a two-sided one.
#'
#' If `n` is larger than 9 and `M` is not specified, `M` is set to 48620.
#'
#' @export
#' @examples
#' \dontrun{
#' walkthrough_p(n = 12, diff = 0.2, sd = 1.3)
#'
#' # Save data and double check results using Welch t-test
#' dat <- walkthrough_p(n = 10, diff = 0.2, sd = 2, showdata = TRUE)
#' t.test(score ~ group, data = dat)
#' }

walkthrough_p <- function(n = 10, diff = 0, sd = 1, showdata = FALSE, M = NULL) {
  if (sd <= 0) {
    stop(paste0("Set the 'sd' parameter to a value larger than 0. It's currently set to ", sd, "."))
  }

  if (n <= 1) {
    stop(paste0("Set the 'n' parameter to a value larger than 1. It's currently set to ", n, "."))
  }

  if (n > 9 & is.null(M)) {
    M <- 48620
    message("Since n is quite large, Monte Carlo rather than exhaustive rerandomisation is used using M = 48620.")
  }

  my_text <- paste0("You want to run a simple between-subjects two-group experiment to compare the efficacy of some intervention. Unbeknownst to you, the intervention yields a boost in performance of ", diff, " points relative to the control group. ",
                    n*2, " participants sign up for your study.")
  writeLines(strwrap(my_text, 60))

  invisible(readline(prompt ="(Press [enter] to continue.)"))

  # Generate dataset
  df <- data.frame(
    group = rep(c("intervention", "control"), times = n),
    score = c(rnorm(n, 10, sd = sd), rnorm(n, 10, sd = sd)))
  df$group <- factor(df$group)
  df$group <- relevel(df$group, "intervention")
  df <- df[sample(1:(2*n)), ]
  df$subject <- factor(1:(2*n))

  limit_plots <- c(floor(min(df$score)), ceiling(max(df$score)+diff))
  break_plots <- seq(limit_plots[1], limit_plots[2], 1)

  # Dotchart with scores and participants
  p1 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score, y = reorder(subject, score))) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("subject")

  print(p1)

  my_text <- paste0("This plot shows what your participants' scores WOULD HAVE BEEN if all of them had been assigned to the control condition. In real life, you don't have access to this information.")
  writeLines(strwrap(my_text, 60))

  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p2 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score,
                   y = reorder(subject, score),
                   colour = group)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(group ~ ., scales = "free") +
    ggplot2::ylab("subject") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::theme(legend.position = "none")

  print(p2)

  my_text <- paste0("Half of the participants are assigned to the intervention condition (red), the others to the control condition (blue).")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt ="(Press [enter] to continue.)"))

  df$score <- ifelse(df$group == "intervention", df$score + diff, df$score)
  p2b <- ggplot2::ggplot(df,
                         ggplot2::aes(x = score,
                    y = reorder(subject, score),
                    colour = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = dplyr::filter(df, group == "intervention"),
               ggplot2::aes(x = score - diff),
               shape = 1) +
    ggplot2::geom_segment(data = dplyr::filter(df, group == "intervention"),
                 ggplot2::aes(x = score - diff, xend = score,
                     yend = reorder(subject, score))) +
    ggplot2::facet_grid(group ~ ., scales = "free") +
    ggplot2::ylab("subject") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::theme(legend.position = "none")

  print(p2b)

  my_text <- paste0("The participants assigned to the intervention condition receive a boost in performance of ", diff, " points.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt ="(Press [enter] to continue.)"))

  treatment_idx <- which(df$group == "intervention")
  sample_difference <- mean(df$score[treatment_idx]) - mean(df$score[-treatment_idx])
  discrepancy <- abs(round(diff - sample_difference , 3))
  sample_difference <- round(sample_difference, 3)

  p_value <- rand_test(df$score, treatment_idx,
                       statistic = mean_diff, plot = FALSE,
                       exact = is.null(M),
                       M = M)[[3]]
  p_value <- round(p_value, 3)
  p_percentage <- 100 * p_value
  if (p_value == 0) {
    p_value <- "<0.001"
    p_percentage <- "<0.1%"
  }

  p3 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score,
                    y = reorder(subject, score),
                    colour = group)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(group ~ ., scales = "free") +
    ggplot2::geom_vline(data = dplyr::filter(df, group == "intervention"),
                        ggplot2::aes(xintercept = mean(score)), linetype = 2) +
    ggplot2::geom_vline(data = dplyr::filter(df, group == "control"),
                        ggplot2::aes(xintercept = mean(score)), linetype = 2) +
    ggplot2::ylab("subject") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::theme(legend.position = "none")

  print(p3)

  my_text <- paste0("The mean difference between the two groups is ",
                    sample_difference, " points, ",
                    "so the difference between the true efficacy of the intervention and your estimate is ", discrepancy, " points.\n\n",
                    "When you run a randomisation test on these data, the p-value is ", p_value, ".\n\n",
                    "What this means is that EVEN IF the true ",
                    "efficacy of the intervention were 0 for all units (= strong null hypothesis), ",
                    "your study still had a chance of finding ",
                    "a difference of ", abs(sample_difference), " points or more ",
                    "of ", p_percentage, ".\n\n",
                    "What it DOESN'T mean is that the null hypothesis has a ", p_percentage, " chance of being correct!")
  writeLines(strwrap(my_text, 60))

  writeLines(strwrap("\n\nRun this function again so see how randomness influences your results.", 60))

  if (showdata) {
    df
  }
}
