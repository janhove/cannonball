#' @title Walkthrough p-values
#'
#' @description This function produces a step-by-step demonstration
#'              of a significance test for a two-group comparison.
#' @param n The number of data points per group.
#' @param diff The boost that participants in the intervention group receive.
#' @param sd The standard deviation of the normal distributions from which the data are drawn.
#' @param showdata Do you want to output a dataframe containing the plotted data (\code{TRUE})
#'                 or not (\code{FALSE}, default)?
#' @param pedant Do you want to run the significance test in pedant mode (\code{TRUE}) or not (\code{FALSE}, default)? See Details.
#' @keywords significance test, p-value
#' @details Data are generated from a normal distribution with the requested
#' standard deviation. Then, the data points are randomly assigned to two
#' equal-sized groups. Data points in the intervention group receive a boost
#' as specified by \code{diff}. Finally, a significance test is run on the data.
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
#' walkthrough_p(n = 12, diff = 0.2, sd = 1.3)
#'
#' # Save data and double check results
#' dat <- walkthrough_p(n = 10, diff = 0.2, sd = 2)
#' t.test(score ~ group, data = dat, var.equal = TRUE)
#'
#' # Run in pedant mode (= permutation test)
#' dat <- walkthrough_p(n = 13, diff = 1, sd = 4, pedant = TRUE)
#' t.test(score ~ group, data = dat, var.equal = TRUE)
#' }

walkthrough_p <- function(n = 10, diff = 0, sd = 1, showdata = FALSE, pedant = FALSE) {
  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (pedant == TRUE && !requireNamespace("coin", quietly = TRUE)) {
    stop("Please install the \"coin\" package if you want to run this function in pedantic mode :)",
         call. = FALSE)
  }

  require("tidyverse")

  my_text <- paste0("You want to run a simple between-subjects two-group experiment to compare the efficacy of some intervention. Unbeknownst to you, the intervention yields a boost in performance of ", diff, " points relative to the control group. ",
                    n*2, " participants sign up for your study.")
  writeLines(strwrap(my_text, 60))

  invisible(readline(prompt ="(Press [enter] to continue.)"))

  # Generate dataset
  df <- data.frame(group = rep(c("intervention", "control"), times = n),
                   score = c(rnorm(n, 10, sd = sd),
                             rnorm(n, 10, sd = sd)))
  df$group <- factor(df$group)
  df$group <- relevel(df$group, "intervention")
  df <- df[sample(1:(2*n)), ]
  df$subject <- factor(1:(2*n))

  limit_plots <- c(floor(min(df$score)), ceiling(max(df$score)+diff))
  break_plots <- seq(limit_plots[1], limit_plots[2], 1)

  # Dotchart with scores and participants
  p1 <- ggplot(df,
               aes(x = score,
                   y = reorder(subject, score))) +
    geom_point() +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    theme(legend.position = "none") +
    ylab("subject")

  print(p1)

  my_text <- paste0("This plot shows what your participants' scores WOULD HAVE BEEN if all of them had been assigned to the control condition. In real life, you don't have access to this information.")
  writeLines(strwrap(my_text, 60))

  invisible(readline(prompt = paste0("(Press [enter] to continue.)")))

  p2 <- ggplot(df,
               aes(x = score,
                   y = reorder(subject, score),
                   colour = group)) +
    geom_point() +
    facet_grid(group ~ ., scales = "free") +
    ylab("subject") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    theme(legend.position = "none")

  print(p2)

  my_text <- paste0("Half of the participants are assigned to the intervention condition (red), the others to the control condition (blue).")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt ="(Press [enter] to continue.)"))

  df$score <- ifelse(df$group == "intervention", df$score + diff, df$score)
  p2b <- ggplot(df,
                aes(x = score,
                    y = reorder(subject, score),
                    colour = group)) +
    geom_point() +
    geom_point(data = filter(df, group == "intervention"),
               aes(x = score - diff),
               shape = 1) +
    geom_segment(data = filter(df, group == "intervention"),
                 aes(x = score - diff, xend = score,
                     yend = reorder(subject, score))) +
    facet_grid(group ~ ., scales = "free") +
    ylab("subject") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    theme(legend.position = "none")

  print(p2b)

  my_text <- paste0("The participants assigned to the intervention condition receive a boost in performance of ", diff, " points.")
  writeLines(strwrap(my_text, 60))
  invisible(readline(prompt ="(Press [enter] to continue.)"))

  t_test <- t.test(score ~ group, data = df, var.equal = TRUE)

  sample_difference <- round(-diff(t_test$estimate), 2)
  discrepancy <- abs(round(diff - -diff(t_test$estimate) , 2))

  if (pedant == FALSE) {
    p_value <- round(t_test$p.value, 3)
  } else if (pedant == TRUE) {
    p_value <- round(coin::pvalue(coin::independence_test(score ~ group, data = df, distribution = "approximate")), 3)
  }

  p3 <- ggplot(df,
                aes(x = score,
                    y = reorder(subject, score),
                    colour = group)) +
    geom_point() +
    facet_grid(group ~ ., scales = "free") +
    geom_vline(data = filter(df, group == "intervention"),
               aes(xintercept = mean(score)), linetype = 2) +
    geom_vline(data = filter(df, group == "control"),
               aes(xintercept = mean(score)), linetype = 2) +
    ylab("subject") +
    scale_x_continuous(limits = limit_plots,
                       breaks = break_plots,
                       minor_breaks = NULL) +
    theme(legend.position = "none")

  print(p3)

  my_text <- paste0("The mean difference between the two groups is ",
                    sample_difference, " points, ",
                    "so the difference between the true efficacy of the intervention and your estimate is ", discrepancy, " points.\n\n",
                    "When you run a significance test on these data, the p-value is ", p_value, ".\n\n",
                    "What this means is that EVEN IF the true ",
                    "efficacy of the intervention were 0 (= null hypothesis), ",
                    "your study still had a chance of finding ",
                    "a difference of ", abs(sample_difference), " points or more ",
                    "of ", p_value*100, "%.\n\n",
                    "What it DOESN'T mean is that the null hypothesis has a ", p_value*100, "% chance of being correct!")
  writeLines(strwrap(my_text, 60))

  # Technically, the t-test returns the probability
  # that a t-statistic larger than the one observed
  # would've been observed if only chance were at play.
  # For ease of exposition, I use the t-test as an
  # approximation to a permutation test. Switch on
  # pedant mode if you want to run a permutation test.

  writeLines(strwrap("\n\nRun this function again so see how randomness influences your results.", 60))

  if (showdata == TRUE) {
    return(df)
  }
}
