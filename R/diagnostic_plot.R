#' @name diagnostic_plots
#' @aliases lin_plot
#' @aliases var_plot
#' @aliases norm_qq
#' @aliases norm_hist
#' @title Draw diagnostic parades
#'
#' @description These are short-hand functions to quickly draw
#' diagnostic parades.
#'
#' `lin_plot()` plots the models' residuals against their corresponding fitted values.
#' This is useful for checking the linearity assumption.
#'
#' `var_plot()` plots the models' absolute residuals against their corresponding fitted values
#' if it is fed an object generated using `parade()`. If it is fed an object
#' generated using `resid_summary()`, it plots the sample standard deviation of the residuals
#' per cell.
#' This is useful for checking the constant-variance assumption.
#'
#' `norm_qq()` and `norm_hist()` plot normal quantile-quantile plots and histograms of the
#' models' residuals, respectively.
#' This is useful for checking the normality assumption.
#' @param parade The name of an object generated using `parade()`.
#' For `var_plot()`, objects generated using `resid_summary()` are also accepted.
#' @param bins How many bins should the histograms contain? Defaults to 30.
#' @name diagnostic_plot
#' @keywords dogs
#' @export
#' @examples
#' # A simple regression model
#' m <- lm(mpg ~ disp, data = mtcars)
#'
#' # Generate parade and check linearity
#' my_parade <- parade(m)
#' lin_plot(my_parade)
#' reveal(my_parade)
#'
#' # Regenerate parade and check constant variance
#' my_parade <- parade(m)
#' var_plot(my_parade)
#' reveal(my_parade)
#'
#' # Regenerate parade and check normality
#' my_parade <- parade(m)
#' norm_qq(my_parade)
#' norm_hist(my_parade)
#' norm_hist(my_parade, bins = 10)
#' reveal(my_parade)

#' @rdname diagnostic_plot
#' @export
lin_plot <- function(parade) {
  
  # The parade needs to be a raw data parade.
  if (!(attr(parade, "data_type") %in% c("raw"))) {
    stop("When diagnosing linearity, make sure the parade was generated using the parade() function.")
  }
  
  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  require("tidyverse")
  
  p <- ggplot(parade,
              aes(x = .fitted,
                  y = .resid)) +
    geom_point(shape = 1) +
    geom_smooth() +
    facet_wrap(~ .sample) +
    xlab("fitted value") +
    ylab("residual")
  
  print(p)
}

#' @rdname diagnostic_plot
#' @export
var_plot <- function(parade) {
  
  # The parade needs to be either a raw data or a summary data parade.
  if (!(attr(parade, "data_type") %in% c("raw", "summary"))) {
    stop("Make sure the parade was generated using parade() or resid_summary().")
  }
  
  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  require("tidyverse")
  
  if (attr(parade, "data_type") == "raw") {
    p <- ggplot(parade,
                aes(x = .fitted,
                    y = .abs_resid)) +
      geom_point(shape = 1) +
      geom_smooth() +
      facet_wrap(~ .sample) +
      xlab("fitted value") +
      ylab("absolute value of residual")
  } else if (attr(parade, "data_type") == "summary") {
    p <- ggplot(parade,
                aes(x = .cell,
                    y = .sd,
                    group = 1)) +
      geom_point() +
      geom_line() +
      facet_wrap(~ .sample) +
      xlab("Cell") +
      ylab("Cell standard deviation")
  }
  
  print(p)
}

#' @rdname diagnostic_plot
#' @export
norm_qq <- function(parade) {
  
  # The parade needs to be a raw data parade.
  if (!(attr(parade, "data_type") %in% c("raw"))) {
    stop("When diagnosing normality, make sure the parade was generated using the parade() function.")
  }
  
  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  require("tidyverse")
  
  p <- ggplot(my_parade,
              aes(sample = .resid)) +
    stat_qq(shape = 1) +
    facet_wrap(~ .sample) +
    xlab("theoretical quantile") +
    ylab("actual residual quantile")
  
  print(p)
}

#' @rdname diagnostic_plot
#' @export
norm_hist <- function(parade, bins = 30) {
  
  # The parade needs to be a raw data parade.
  if (!(attr(parade, "data_type") %in% c("raw"))) {
    stop("When diagnosing normality, make sure the parade was generated using the parade() function.")
  }
  
  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  require("tidyverse")
  
  p <- ggplot(my_parade,
              aes(x = .resid)) +
    geom_histogram(bins = bins,
                   fill = "lightgrey", colour = "black") +
    facet_wrap(~ .sample) +
    xlab("residual") +
    ylab("frequency")
  
  print(p)
}
