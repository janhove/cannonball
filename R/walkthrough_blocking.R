#' @title Walkthrough blocking on a covariate
#'
#' @description This function produces a step-by-step demonstration
#'              of how researchers can 'block' on a continuous covariate
#'              and how they can analyse the data of a randomised block design.
#' @param n The number of data points per group.
#' @param diff The boost that participants in the intervention group receive.
#' @param sd The standard deviation of the normal distributions from which the data are drawn.
#' @param rho The correlation between the covariate and the outcome (pre-intervention) in the population.
#' @param showdata Do you want to output a dataframe containing the plotted data (\code{TRUE})
#'                 or not (\code{FALSE}, default)?
#' @param M `NULL` (default) when using exhaustive randomisation testing; else set to the number of Monte Carlo runs desired.
#' @keywords significance test, p-value, blocking, control variable
#' @details Data are generated from a normal distribution with the requested
#' standard deviation; a covariate is also generated. The data points are then
#' grouped in pairs based on their covariate scores. Within each pair, the data points
#' are then randomly assigned to the control or intervention group.
#' Data points in the intervention group receive a boost
#' as specified by 'diff'. Finally, a significance test is ran on the data.
#' This significance test is a randomisation test using the mean difference as
#' the test statistic. The p-value reported is a two-sided one.
#'
#' If `n` is larger than 16 and `M` is not specified, `M` is set to 65536.
#'
#' @export
#' @examples
#' \dontrun{
#' walkthrough_blocking(n = 12, diff = 0.2, sd = 1.3, rho = 0.8)
#'
#' # Save data and double check results
#' dat <- walkthrough_blocking(n = 12, diff = 0.2, sd = 1.3, rho = 0.8, showdata = TRUE)
#' anova(lm(score ~ factor(block) + group, data = dat))
#' }

walkthrough_blocking <- function(n = 10, diff = 0, sd = 1, rho = 0.8, showdata = FALSE, M = NULL) {
  if (abs(rho) > 1) {
    stop(paste0("Set the 'rho' parameter to a value between -1 and 1. It's currently set to ", rho, "."))
  }

  if (sd <= 0) {
    stop(paste0("Set the 'sd' parameter to a value larger than 0. It's currently set to ", sd, "."))
  }

  if (n <= 1) {
    stop(paste0("Set the 'n' parameter to a value larger than 1. It's currently set to ", n, "."))
  }

  if (n > 16 & is.null(M)) {
    M <- 65536
    message("Since n is quite large, Monte Carlo rather than exhaustive rerandomisation is used using M = 65536.")
  }

  my_text <- paste0("You want to run a between-subjects two-group experiment to compare the efficacy of some intervention. Unbeknownst to you, the intervention yields a boost in performance of ", diff, " points relative to the control method. ",
                    n*2, " participants sign up for your study. ",
                    "Before running the experiment, you were able to collect a control variable that, in the population at large, is correlated at rho = ", rho,
                    " with what would have been the participants' scores if they had all been assigned to the control condition. ",
                    "This control variable may be a pretest score, some measure of their intelligence, motivation, and so on.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt ="(Press [enter] to continue.)"))

  # Generate dataset
  covariance_matrix <- (c(sd, 1) %*% t(c(sd, 1))) * rbind(c(1, rho), c(rho, 1))
  variables <- MASS::mvrnorm(n = 2*n, mu = c(10, 10),
                             Sigma = covariance_matrix)

  df <- data.frame(score = variables[, 1],
                   covariate = variables[, 2])
  df <- df[sample(1:(2*n)), ]
  df$subject <- factor(1:(2*n))

  limit_plots <- c(floor(min(df$score)), ceiling(max(df$score)+diff))
  break_plots <- seq(limit_plots[1], limit_plots[2], 1)

  # Dotchart with covariate scores and participants
  p1 <- ggplot2::ggplot(df,
      ggplot2::aes(x = covariate,
                   y = stats::reorder(subject, covariate))) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::ylab("subject")

  print(p1)

  my_text <- paste0("The graph shows how the participants compare on the control variable.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  df <- df |> dplyr::arrange(covariate)
  df$block <- rep(1:n, each = 2)
  df$block <- factor(df$block)

  p2 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = covariate,
                   y = stats::reorder(subject, covariate))) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_grid(stats::reorder(block, -covariate) ~ ., scales = "free_y") +
    ggplot2::ylab("subject")

  print(p2)

  my_text <- paste0("Sorting the participants on their covariate values, ",
                    "you group them into ", n, " blocks with two participants each.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  df <- df |>
    dplyr::group_by(block) |>
    dplyr::mutate(group = sample(c("control", "intervention"))) |>
    dplyr::ungroup()
  df$group <- factor(df$group, levels = c("intervention", "control"))

  p3 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score,
                   y = stats::reorder(subject, covariate))) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_grid(stats::reorder(block, -covariate) ~ ., scales = "free_y") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::ylab("subject")

  print(p3)

  my_text <- paste0("This graph shows what the participants' scores on the ",
                    "outcome variable would have been if all of them had been ",
                    "assigned to the control condition. ",
                    "The stronger the 'rho' you specified in the function call, ",
                    "the more similar the participants' scores within each block will be.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p4 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score,
                   y = stats::reorder(subject, covariate),
                   colour = group)) +
    ggplot2::geom_point() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::facet_grid(stats::reorder(block, -covariate) ~ ., scales = "free_y") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::ylab("subject")

  print(p4)

  my_text <- paste0("Instead of assigning the participants to the condition ",
                    "completely at random, you assign one random participant ",
                    "in each block to the intervention condition (red) and the ",
                    "other to the control condition (blue). Now you have a ",
                    "randomised block design.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p5 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score,
                   y = stats::reorder(subject, -as.numeric(group)),
                   colour = group)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(stats::reorder(block, -covariate) ~ ., scales = "free") +
    ggplot2::ylab("subject") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::theme(legend.position = "none")

  print(p5)

  my_text <- paste0("Redrawing the graph so that the participants in the ",
                    "intervention condition (red) are on the top line in ",
                    "their block.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  df$score <- ifelse(df$group == "intervention", df$score + diff, df$score)

  p6 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score,
                   y = stats::reorder(subject, -as.numeric(group)),
                   colour = group)) +
    ggplot2::geom_point() +
    ggplot2::geom_point(data = dplyr::filter(df, group == "intervention"),
                        ggplot2::aes(x = score - diff),
               shape = 1) +
    ggplot2::geom_segment(data = dplyr::filter(df, group == "intervention"),
                 ggplot2::aes(x = score - diff, xend = score,
                     yend = stats::reorder(subject, score))) +
    ggplot2::facet_grid(stats::reorder(block, -covariate) ~ ., scales = "free") +
    ggplot2::ylab("subject") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::theme(legend.position = "none")

  print(p6)
  my_text <- paste0("The participants in the intervention condition ",
                    "receive a boost of ", diff, " points.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p7 <- ggplot2::ggplot(df,
                        ggplot2::aes(x = score,
                   y = stats::reorder(subject, -as.numeric(group)),
                   colour = group)) +
    ggplot2::geom_point() +
    ggplot2::facet_grid(stats::reorder(block, -covariate) ~ ., scales = "free") +
    ggplot2::ylab("subject") +
    ggplot2::scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ggplot2::theme(legend.position = "none")

  print(p7)
  my_text <- paste0("The data we actually observe in this experiment look as plotted. ",
                    "In the next step, we calculate the difference between the red and ",
                    "the blue point within each block, yielding ", n, " differences.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  # Compute differences
  per_block <- df |>
    tidyr::pivot_wider(id_cols = "block",
                names_from = "group",
                values_from = "score") |>
    dplyr::mutate(difference = intervention - control)

  p8 <- ggplot2::ggplot(per_block,
                        ggplot2::aes(x = difference,
                   y = stats::reorder(block, difference))) +
                     ggplot2::geom_point() +
      ggplot2::xlab("difference within each block") +
      ggplot2::ylab("block") +
      ggplot2::geom_vline(xintercept = 0, linetype = 2) +
      ggplot2::geom_vline(xintercept = mean(per_block$difference), linetype = 2, colour = "red") +
      ggplot2::theme(legend.position = "none")

  print(p8)

  treatment_idx <- which(df$group == "intervention")
  p_value <- rand_test(df$score, treatment_idx, df$block,
                       statistic = mean_diff, plot = FALSE,
                       exact = is.null(M), M = M)[[3]]
  p_value <- round(p_value, 3)
  p_percentage <- 100 * p_value
  if (p_value == 0) {
    p_value <- "<0.001"
    p_percentage <- "<0.1%"
  }

  wrong_p_value <- rand_test(df$score, treatment_idx, NULL,
                             statistic = mean_diff, plot = FALSE,
                             exact = FALSE, M = 10000)[[3]]
  wrong_p_value <- round(wrong_p_value, 3)
  if (wrong_p_value == 0) {
    wrong_p_value <- "<0.001"
  }

  sample_difference <- mean(df$score[treatment_idx]) - mean(df$score[-treatment_idx])
  discrepancy <- abs(diff - sample_difference)
  discrepancy <- round(discrepancy, 3)
  sample_difference <- round(sample_difference, 3)

  my_text <- paste0(
      "The mean difference score is ", sample_difference, " points (red line), ",
      "so the difference between the true efficacy of the intervention and your estimate is ", discrepancy, " points.\n\n",
      "If you run a randomisation test on these data while taking into account the blocking, the p-value is ", p_value, ".\n\n",
      "What this means is that EVEN IF the true efficacy of the intervention were 0 (= null hypothesis), ",
      "your study still had a chance of finding a difference of ",
      abs(sample_difference), " points or more of ", p_percentage, ".\n\n",
      "What this DOESN'T mean is that there is a chance of ", p_percentage, " that the null hypothesis is true.\n\n",
      "Had you analysed these same data but without taking the 'blocks' into account, you would have ",
      "obtained an incorrect p-value of ", wrong_p_value, "."
  )


  writeLines(strwrap(my_text, 60))

  writeLines(strwrap("\n\nRun this function again to see how randomness influences your results.", 60))

  if (showdata) return(df)
}
