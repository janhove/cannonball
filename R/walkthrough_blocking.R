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
#' @param pedant Do you want to run the significance test in pedant mode (\code{TRUE}) or not (\code{FALSE}, default)? See Details.
#' @keywords significance test, p-value, blocking, control variable
#' @details Data are generated from a normal distribution with the requested
#' standard deviation; a covariate is also generated. The data points are then
#' grouped in pairs based on their covariate scores. Within each pair, the data points
#' are then randomly assigned to the control or intervention group.
#' Data points in the intervention group receive a boost
#' as specified by 'diff'. Finally, a significance test is ran on the data.
#'
#' By default, the significance test is a two-sample Student's t-test.
#' Technically, the p-value from this test is the probability
#' that a t-statistic larger than the one observed
#' would've been observed if only chance were at play, but
#' the walkthrough text says that is the probability that
#' a mean difference larger than the one observed would've
#' been observed if only chance were at play. That is,
#' I use the t-test as an
#' approximation to a permutation test.
#' Switch on pedant mode if you want to run a permutation test.
#'
#' @export
#' @examples
#' \dontrun{
#' walkthrough_blocking(n = 12, diff = 0.2, sd = 1.3, rho = 0.8, pedant = FALSE)
#'
#' # Save data and double check results
#' dat <- walkthrough_blocking(n = 12, diff = 0.2, sd = 1.3, rho = 0.8, showdata = TRUE)
#' anova(lm(score ~ factor(block) + group, data = dat))
#'
#' # Run in pedant mode (= permutation test)
#' walkthrough_blocking(n = 12, diff = 0.2, sd = 1.3, rho = 0.8, pedant = TRUE)
#' }

walkthrough_blocking <- function(n = 10, diff = 0, sd = 1, rho = 0.8, showdata = FALSE, pedant = FALSE) {
  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (!requireNamespace("MASS", quietly = TRUE)) {
    stop("The \"MASS\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (pedant == TRUE && !requireNamespace("coin", quietly = TRUE)) {
    stop("Please install the \"coin\" package if you want to run this function in pedantic mode :)",
         call. = FALSE)
  }

  if (abs(rho) > 1) {
    stop(paste0("Set the 'rho' parameter to a value between -1 and 1. It's currently set to ", rho, "."))
  }

  if (sd <= 0) {
    stop(paste0("Set the 'sd' parameter to a value larger than 0. It's currently set to ", sd, "."))
  }

  if (n <= 1) {
    stop(paste0("Set the 'n' parameter to a value larger than 1. It's currently set to ", n, "."))
  }

  require("tidyverse")

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
  p1 <- ggplot(df,
               aes(x = covariate,
                   y = reorder(subject, covariate))) +
    geom_point() +
    theme(legend.position = "none") +
    ylab("subject")

  print(p1)

  my_text <- paste0("The graph shows how the participants compare on the control variable.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  df <- df %>% arrange(covariate)
  df$block <- rep(1:n, each = 2)
  df$block <- factor(df$block)

  p2 <- ggplot(df,
               aes(x = covariate,
                   y = reorder(subject, covariate))) +
    geom_point() +
    theme(legend.position = "none") +
    facet_grid(reorder(block, -covariate) ~ ., scales = "free_y") +
    ylab("subject")

  print(p2)

  my_text <- paste0("Sorting the participants on their covariate values, ",
                    "you group them into ", n, " blocks with two participants each.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  df <- df %>%
    group_by(block) %>%
    mutate(group = sample(c("control", "intervention"))) %>%
    ungroup()
  df$group <- factor(df$group, levels = c("intervention", "control"))

  p3 <- ggplot(df,
               aes(x = score,
                   y = reorder(subject, covariate))) +
    geom_point() +
    theme(legend.position = "none") +
    facet_grid(reorder(block, -covariate) ~ ., scales = "free_y") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ylab("subject")

  print(p3)

  my_text <- paste0("This graph shows what the participants' scores on the ",
                    "outcome variable would have been if all of them had been ",
                    "assigned to the control condition. ",
                    "The stronger the 'rho' you specified in the function call, ",
                    "the more similar the participants' scores within each block will be.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p4 <- ggplot(df,
               aes(x = score,
                   y = reorder(subject, covariate),
                   colour = group)) +
    geom_point() +
    theme(legend.position = "none") +
    facet_grid(reorder(block, -covariate) ~ ., scales = "free_y") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    ylab("subject")

  print(p4)

  my_text <- paste0("Instead of assigning the participants to the condition ",
                    "completely at random, you assign one random participant ",
                    "in each block to the intervention condition (red) and the ",
                    "other to the control condition (blue). Now you have a ",
                    "randomised block design.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p5 <- ggplot(df,
               aes(x = score,
                   y = reorder(subject, -as.numeric(group)),
                   colour = group)) +
    geom_point() +
    facet_grid(reorder(block, -covariate) ~ ., scales = "free") +
    ylab("subject") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    theme(legend.position = "none")

  print(p5)

  my_text <- paste0("Redrawing the graph so that the participants in the ",
                    "intervention condition (red) are on the top line in ",
                    "their block.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  df$score <- ifelse(df$group == "intervention", df$score + diff, df$score)

  p6 <- ggplot(df,
               aes(x = score,
                   y = reorder(subject, -as.numeric(group)),
                   colour = group)) +
    geom_point() +
    geom_point(data = filter(df, group == "intervention"),
               aes(x = score - diff),
               shape = 1) +
    geom_segment(data = filter(df, group == "intervention"),
                 aes(x = score - diff, xend = score,
                     yend = reorder(subject, score))) +
    facet_grid(reorder(block, -covariate) ~ ., scales = "free") +
    ylab("subject") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    theme(legend.position = "none")

  print(p6)
  my_text <- paste0("The participants in the intervention condition ",
                    "receive a boost of ", diff, " points.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p7 <- ggplot(df,
               aes(x = score,
                   y = reorder(subject, -as.numeric(group)),
                   colour = group)) +
    geom_point() +
    facet_grid(reorder(block, -covariate) ~ ., scales = "free") +
    ylab("subject") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    theme(legend.position = "none")

  print(p7)
  my_text <- paste0("The data we actually observe in this experiment look as plotted. ",
                    "In the next step, we calculate the difference between the red and ",
                    "the blue point within each block, yielding ", n, " differences.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  # Compute differences
  per_block <- df %>%
    pivot_wider(id_cols = c("group", "block"),
                names_from = "group",
                values_from = "score") %>%
    mutate(difference = intervention - control)

  p8 <- ggplot(per_block,
               aes(x = difference,
                   y = reorder(block, difference))) +
    geom_point() +
    xlab("difference within each block") +
    ylab("block") +
    geom_vline(xintercept = 0, linetype = 2) +
    geom_vline(xintercept = mean(per_block$difference), linetype = 2, colour = "red") +
    theme(legend.position = "none")

  print(p8)

  t_test <- t.test(per_block$difference)

  sample_difference <- round(t_test$estimate, 2)
  discrepancy <- abs(round(diff - t_test$estimate , 2))

  if (pedant == FALSE) {
    p_value <- round(t_test$p.value, 3)
    my_text <- paste0(
      "The mean difference score is ", sample_difference, " points (red line), ",
      "so the difference between the true efficacy of the intervention and your estimate is ", discrepancy, " points.\n\n",
      "If you run a one-sample t-test on these difference scores, the p-value is ", p_value, ".\n\n",
      "What this means is that EVEN IF the true efficacy of the intervention were 0 (= null hypothesis), ",
      "your study still had a chance of finding a difference of ",
      abs(sample_difference), " points or more of ", p_value*100, "%.\n\n",
      "What this DOESN'T mean is that there is a chance of ", p_value*100, "% that the null hypothesis is true.\n\n",
      "An alternative way of analysing these data would be to run an analysis of variance (ANOVA) on the original data ",
      "that included both 'group' and 'block' as independent variables. The p-value obtained for the 'group' variable ",
      "will be identical to the one obtained using the method outlined above.\n\n",
      "Had you analysed these same data but without taking the 'blocks' into account, you would have ",
      "obtained an incorrect p-value of ", round(t.test(score ~ group, data = df, var.equal = TRUE)$p.value, 3), "."
    )
  } else if (pedant == TRUE) {
    means_H0 <- replicate(20000, mean(per_block$difference * sample(c(-1, 1), n, replace = TRUE)))
    p_value <- round(mean(abs(means_H0) >= abs(sample_difference)), 3)

    my_text <- paste0(
      "The mean difference score is ", sample_difference, " points (red line), ",
      "so the difference between the true efficacy of the intervention and your estimate is ", discrepancy, " points.\n\n",

      "If you run a permutation test that takes into account the randomised block design on these data, ",
      "you'll obtain a p-value of ", p_value, ".\n\n",
      "What this means is that EVEN IF the true efficacy of the intervention were 0 (= null hypothesis), ",
      "your study still had a chance of finding a mean difference score of ",
      abs(sample_difference), " points or more of ", p_value*100, "%.\n\n",

      "What this DOESN'T mean is that there is a chance of ", p_value*100, "% that the null hypothesis is true.\n\n",

      "Had you analysed these same data but without taking the 'blocks' into account, you would have ",
      "obtained an incorrect p-value of ", round(coin::pvalue(coin::independence_test(score ~ group, data = df, distribution = "approximate")), 3), "."
    )
  }

  writeLines(strwrap(my_text, 60))

  # Technically, the t-test returns the probability
  # that a t-statistic larger than the one observed
  # would've been observed if only chance were at play.
  # For ease of exposition, I use the t-test as an
  # approximation to a permutation test. Switch on
  # pedant mode if you want to run a permutation test.
  # This permutation test works by switching the signs
  # of the differences per block with probability 0.5.

  writeLines(strwrap("\n\nRun this function again to see how randomness influences your results.", 60))

  if (showdata == TRUE) {
    return(df)
  }
}
