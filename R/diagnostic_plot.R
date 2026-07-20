#' @name diagnostic_plot
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
#' @param rank Should the values along the x-axis be converted to ranks (\code{TRUE}) or not (\code{FALSE}, default)?
#' When used, ties are broken randomly.
#' This may be useful when the raw values are concentrated in certain regions along the x-axis, making it difficult to
#' discern relevant patterns.
#' @param bins How many bins should the histograms contain? Defaults to 30.
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
lin_plot <- function(parade, predictor = NULL, rank = FALSE) {
  if (is.null(attr(parade, "data_type"))) {
  	stop("The object you supplied doesn't seem to be a parade.")
  }

  # The parade needs to be a raw data parade.
  if (!(attr(parade, "data_type") %in% c("raw"))) {
    stop("When diagnosing linearity, make sure the parade was generated using the parade() function.")
  }

  # If predictor is specified, make sure it occurs in parade.

  # If no predictor is specified, plot the residuals against the fitted values.
  if (is.null(predictor)) {
    p <- ggplot2::ggplot(parade,
                ggplot2::aes(x = rank_id(.fitted, .sample, rank),
                             y = .resid)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_smooth(se = FALSE) +
      ggplot2::facet_wrap(~ .sample) +
      ggplot2::xlab(label_rank("fitted value", rank)) +
      ggplot2::ylab("residual")
  } else if (predictor %in% colnames(parade)) {
    p <- ggplot2::ggplot(parade,
                ggplot2::aes(x = rank_id(!!rlang::sym(predictor), .sample, rank),
                    y = .resid)) +
      ggplot2::geom_point(shape = 1) +
      ggplot2::geom_smooth(se = FALSE) +
      ggplot2::facet_wrap(~ .sample) +
      ggplot2::xlab(label_rank(predictor, rank)) +
      ggplot2::ylab("residual")
  } else {
    stop(paste0("The variable ", predictor, " doesn't occur in the parade. ",
                "Perhaps you need to supply the full original dataset to the parade() function? ",
                "(See the full_data argument under ?parade.)"))
  }

  print(p)
}

#' @rdname diagnostic_plot
#' @export
var_plot <- function(parade, predictor = NULL, rank = FALSE) {

  if (is.null(attr(parade, "data_type"))) {
  	stop("The object you supplied doesn't seem to be a parade.")
  }

  # The parade needs to be either a raw data or a summary data parade.
  if (!(attr(parade, "data_type") %in% c("raw", "summary"))) {
    stop("Make sure the parade was generated using parade() or resid_summary().")
  }

  if (attr(parade, "data_type") == "raw") {
    # If no predictor is specified, plot the residuals against the fitted values.
    if (is.null(predictor)) {
      p <- ggplot2::ggplot(parade,
                  ggplot2::aes(x = rank_id(.fitted, .sample, rank),
                      y = .abs_resid)) +
        ggplot2::geom_point(shape = 1) +
        ggplot2::geom_smooth(se = FALSE) +
        ggplot2::facet_wrap(~ .sample) +
        ggplot2::xlab(label_rank("fitted value", rank)) +
        ggplot2::ylab("absolute value of residual")
    } else if (predictor %in% colnames(parade)) {
      p <- ggplot2::ggplot(parade,
                  ggplot2::aes(x = rank_id(!!rlang::sym(predictor), .sample, rank),
                      y = .abs_resid)) +
        ggplot2::geom_point(shape = 1) +
        ggplot2::geom_smooth(se = FALSE) +
        ggplot2::facet_wrap(~ .sample) +
        ggplot2::xlab(label_rank(predictor, rank)) +
        ggplot2::ylab("absolute value of residual")
    } else {
      stop(paste0("The variable ", predictor, " doesn't occur in the parade. ",
                  "Perhaps you need to supply the full original dataset to the parade() function? ",
                  "(See the full_data argument under ?parade.)"))
    }
  } else if (attr(parade, "data_type") == "summary") {
    p <- ggplot2::ggplot(parade,
                ggplot2::aes(x = .cell,
                    y = .sd,
                    group = 1)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() +
      ggplot2::facet_wrap(~ .sample) +
      ggplot2::xlab("Cell") +
      ggplot2::ylab("Cell standard deviation")
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

  p <- ggplot2::ggplot(parade,
              ggplot2::aes(sample = .resid)) +
    ggplot2::stat_qq(shape = 1) +
    ggplot2::facet_wrap(~ .sample) +
    ggplot2::xlab("theoretical standard normal quantile") +
    ggplot2::ylab("actual residual quantile")

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

  p <- ggplot2::ggplot(parade,
              ggplot2::aes(x = .resid)) +
    ggplot2::geom_histogram(bins = bins,
                   fill = "lightgrey", colour = "black") +
    ggplot2::facet_wrap(~ .sample) +
    ggplot2::xlab("residual") +
    ggplot2::ylab("frequency")

  print(p)
}

# Rank or identity. but need to rank within sample...
rank_id <- function(x, sample = .sample, rank = rank) {
  if (!rank) return(x)
  tapply_result <- tapply(x, sample, rank, ties.method = "random")
  unlist(tapply_result, use.names = FALSE)
}
label_rank <- function(text, rank = rank) {
  if (!rank) return(text)
  paste0(text, " (ranks)")
}
