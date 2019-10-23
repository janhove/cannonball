#' @name diagnostic_plots
#' @aliases lin_plot
#' @aliases var_plot
#' @aliases norm_qq
#' @aliases norm_hist
#' @title Draw diagnostic parades
#'
#' @description These are short-hand functions to quickly draw diagnostic parades.
#'
#' \code{lin_plot()} plots the models' residuals against (by default) their corresponding fitted values.
#' The residuals can also be plotted against a specific predictor if the \code{predictor}
#' parameter is set.
#' This function is useful for checking the linearity assumption.
#'
#' \code{var_plot()} plots the models' absolute residuals against (by default)
#' their corresponding fitted values if it is fed an object generated using \code{parade()}.
#' The absolute residuals can also be plotted against a specific predictor if the \code{predictor} argument
#' is set. If it is fed an object generated using \code{parade_summary()},
#' it plots the sample standard deviation of the residuals per cell.
#' This function is useful for checking the constant-variance assumption.
#'
#' \code{norm_qq()} and \code{norm_hist()} plot normal quantile-quantile plots and histograms of the
#' models' residuals, respectively. This function is useful for checking the normality assumption.
#' @param parade The name of an object generated using \code{parade()}.
#' For \code{var_plot()}, objects generated using \code{parade_summary()} are also accepted.
#' @param predictor The name of a variable in the parade object against which the residuals
#' should be plotted. If this parameter isn't specified (default), the residuals will be plotted
#' against their respective fitted values.
#' @param bins How many bins should the histograms contain? Defaults to 30.
#' @name diagnostic_plot
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
#'
#' # Example with gam
#' library(mgcv)
#' m.gam <- gam(mpg ~ s(disp) + wt + s(qsec, by = am), data = mtcars)
#' my_parade <- parade(m.gam)
#' lin_plot(my_parade)
#' lin_plot(my_parade, predictor = "wt")
#' lin_plot(my_parade, predictor = "qsec")

#' @rdname diagnostic_plot
#' @export
lin_plot <- function(parade, predictor = NULL) {

  if (is.null(attr(parade, "data_type"))) {
  	stop("The object you supplied doesn't seem to be a parade.")
  }

  # The parade needs to be a raw data parade.
  if (!(attr(parade, "data_type") %in% c("raw"))) {
    stop("When diagnosing linearity, make sure the parade was generated using the parade() function.")
  }

  # If predictor is specified, make sure it occurs in parade.

  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }

  require("tidyverse")

  # If no predictor is specified, plot the residuals against the fitted values.
  if (is.null(predictor)) {
    p <- ggplot(parade,
                aes(x = .fitted,
                    y = .resid)) +
      geom_point(shape = 1) +
      geom_smooth() +
      facet_wrap(~ .sample) +
      xlab("fitted value") +
      ylab("residual")
  } else if (predictor %in% colnames(parade)) {
    p <- ggplot(parade,
                aes_string(x = predictor,
                           y = ".resid")) +
      geom_point(shape = 1) +
      geom_smooth() +
      facet_wrap(~ .sample) +
      xlab(predictor) +
      ylab("residual")
  } else {
    stop(paste0("The variable ", predictor, " doesn't occur in the parade. ",
                "Perhaps you need to supply the full original dataset to the parade() function? ",
                "(See the full_data argument under ?parade.)"))
  }

  print(p)
}

#' @rdname diagnostic_plot
#' @export
var_plot <- function(parade, predictor = NULL) {

  if (is.null(attr(parade, "data_type"))) {
  	stop("The object you supplied doesn't seem to be a parade.")
  }

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
    # If no predictor is specified, plot the residuals against the fitted values.
    if (is.null(predictor)) {
      p <- ggplot(parade,
                  aes(x = .fitted,
                      y = .abs_resid)) +
        geom_point(shape = 1) +
        geom_smooth() +
        facet_wrap(~ .sample) +
        xlab("fitted value") +
        ylab("absolute value of residual")
    } else if (predictor %in% colnames(parade)) {
      p <- ggplot(parade,
                  aes_string(x = predictor,
                             y = ".abs_resid")) +
        geom_point(shape = 1) +
        geom_smooth() +
        facet_wrap(~ .sample) +
        xlab(predictor) +
        ylab("absolute value of residual")
    } else {
      stop(paste0("The variable ", predictor, " doesn't occur in the parade. ",
                  "Perhaps you need to supply the full original dataset to the parade() function? ",
                  "(See the full_data argument under ?parade.)"))
    }
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

  if (is.null(attr(parade, "data_type"))) {
  	stop("The object you supplied doesn't seem to be a parade.")
  }

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

  p <- ggplot(parade,
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

  if (is.null(attr(parade, "data_type"))) {
  	stop("The object you supplied doesn't seem to be a parade.")
  }

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

  p <- ggplot(parade,
              aes(x = .resid)) +
    geom_histogram(bins = bins,
                   fill = "lightgrey", colour = "black") +
    facet_wrap(~ .sample) +
    xlab("residual") +
    ylab("frequency")

  print(p)
}
