% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diagnostic_plot.R
\name{diagnostic_plot}
\alias{diagnostic_plot}
\alias{lin_plot}
\alias{var_plot}
\alias{norm_qq}
\alias{norm_hist}
\title{Draw diagnostic parades}
\usage{
lin_plot(parade, predictor = NULL, rank = FALSE)

var_plot(parade, predictor = NULL, rank = FALSE)

norm_qq(parade)

norm_hist(parade, bins = 30)
}
\arguments{
\item{parade}{The name of an object generated using \code{parade()}.
For \code{var_plot()}, objects generated using \code{parade_summary()} are also accepted.}

\item{predictor}{The name of a variable in the parade object against which the residuals
should be plotted. If this parameter isn't specified (default), the residuals will be plotted
against their respective fitted values.}

\item{rank}{Should the values along the x-axis be converted to ranks (\code{TRUE}) or not (\code{FALSE}, default)?
When used, ties are broken randomly.
This may be useful when the raw values are concentrated in certain regions along the x-axis, making it difficult to
discern relevant patterns.}

\item{bins}{How many bins should the histograms contain? Defaults to 30.}
}
\description{
These are short-hand functions to quickly draw diagnostic parades.

\code{lin_plot()} plots the models' residuals against (by default) their corresponding fitted values.
The residuals can also be plotted against a specific predictor if the \code{predictor}
parameter is set.
This function is useful for checking the linearity assumption.

\code{var_plot()} plots the models' absolute residuals against (by default)
their corresponding fitted values if it is fed an object generated using \code{parade()}.
The absolute residuals can also be plotted against a specific predictor if the \code{predictor} argument
is set. If it is fed an object generated using \code{parade_summary()},
it plots the sample standard deviation of the residuals per cell.
This function is useful for checking the constant-variance assumption.

\code{norm_qq()} and \code{norm_hist()} plot normal quantile-quantile plots and histograms of the
models' residuals, respectively. This function is useful for checking the normality assumption.
}
\examples{
# A simple regression model
m <- lm(mpg ~ disp, data = mtcars)

# Generate parade and check linearity
my_parade <- parade(m)
lin_plot(my_parade)
reveal(my_parade)

# Regenerate parade and check constant variance
my_parade <- parade(m)
var_plot(my_parade)
reveal(my_parade)

# Regenerate parade and check normality
my_parade <- parade(m)
norm_qq(my_parade)
norm_hist(my_parade)
norm_hist(my_parade, bins = 10)
reveal(my_parade)

# Example with gam
library(mgcv)
m.gam <- gam(mpg ~ s(disp) + wt + s(qsec, by = am), data = mtcars)
my_parade <- parade(m.gam)
lin_plot(my_parade)
lin_plot(my_parade, predictor = "wt")
lin_plot(my_parade, predictor = "qsec")
}
