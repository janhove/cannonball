#' @title Draw different scatterplots corresponding to a sample correlation coefficient
#'
#' @description This function illustrates the importance of looking at your data 
#'              before moving to numeric methods. More specifically, it illustrates 
#'              that a single correlation coefficient can correspond to wildly 
#'              different patterns in the data. In all, 16 scatterplots with the 
#'              same sample Pearson correlation coefficient are drawn.
#' @param r The desired sample correlation coefficient. 
#'          This must be a number equal to or larger than -1 and smaller than 
#'          (but not equal to) 1.
#' @param n The number of data points per scatterplot.
#' @param showdata Do you want to output a dataframe containing the plotted data (TRUE) 
#'                 or not (FALSE, default)? You can also output the data for a 
#'                 specific scatterplot by setting this parameter to the 
#'                 corresponding number (see examples).
#' @param plot Do you want to draw the scatterplots (TRUE, default) or not (FALSE)?
#' @keywords correlation coefficients, scatterplots
#' @details
#' (1) The x/y data are drawn from a bivariate normal distribution. 
#' Pearson's correlation coefficient can be useful in this situation.
#' 
#' (2) The x data is uniformly distributed between 0 and 1; the y
#' data are scattered normally about the regression line. This isn't
#' too problematic either.
#' 
#' (3) The x data are drawn from a right-skewed distribution; the
#' y data are scattered normally about the regression line. While
#' all y-data follow from the same data-generating mechanism
#' (i.e., outliers reflect the same process as ordinary data points
#' and aren't the result of, say, transcription errors), outlying
#' data points may exert a large effect on the correlation coefficient.
#' (Try exporting the dataset for this plot and recomputing the correlation
#' coefficient without the largest x-value!)
#' 
#' (4) Same as (3), but this time the x data are drawn from a left-skewed
#' distribution.
#' 
#' (5) The x data are drawn from a normal distribution but the y values
#' are right-skewed about the regression line. Every now and again one
#' or a couple of datapoints will exert a large pull on the correlation
#' coefficient.
#' 
#' (6) Same as (5), except the y values are left-skewed about the regression line.
#' 
#' (7) For increasing x values, the y values are ever more 
#' scattered about the regression line. Conceptually, Pearson's correlation
#' coefficients underestimates how well you can anticipate a y-value
#' for small x-values an overestimates how well you can do so for large
#' x-values.
#' 
#' 
#' (8) Same as (7), except the scatter about the regression line 
#' becomes smaller for larger x-values.
#' 
#' (9) Pearson's correlation coefficient summarises the linear trend
#' in the data; the trend in this panel, however, is quadratic. As a
#' result, Pearson's correlation coefficient will underestimate the
#' strength of the relationship between the two variables.
#' 
#' (10) Similar to (9) but the trend is sinusoid.
#' 
#' (11) There is a single positive outlier that exerts a pull on the
#' correlation coefficient. In contrast to (3) through (6), this
#' outlier is not caused by the same data-generating mechanism as the
#' rest of the data, so it contaminates the estimated correlation 
#' coefficient. The regression line for the data without this outlier
#' is plotted as a dashed red line; you'll often find that the true
#' trend in the data go counter to the trend when the outlier isn't
#' excluded.
#' 
#' (12) Similar to (11), except the outlier is negative (i.e., it
#' pulls the correlation coefficient down).
#' 
#' (13) The y data are distributed bimodally about the regression line.
#' This suggests that an important categorical predictor wasn't
#' taken into account in the analysis.
#' 
#' (14) A more worrisome version of (13). There are actually two
#' groups in the data, and this wasn't taken into account. But within
#' each of these groups, the trend may often (not always) run counter
#' to the overall trend. So, for instance, Pearson's correlation may
#' suggest the presence of a positive correlation, but the true trend
#' may actually be negative -- provided the group factor is taken into
#' account. This is known as Simpson's paradox.
#' 
#' (15) The data in this panel were sampled from a bivariate normal
#' distribution but the middle part of the distribution was removed.
#' This may be a sensible sampling strategy when you want to investigate
#' the relationship between two variables, one of which is expensive
#' to measure and the other cheap: Screen a large number of observations
#' on the cheap variable, and only collect the expensive variable for the
#' extreme observations. In a regression analysis, this sampling strategy
#' can be highly effective in terms of power and precision. However,
#' it artificially inflates correlation coefficients.
#' 
#' (16) This is what I suspect lots of datasets actually look like. The
#' x and y data are both categorical. This needn't be too problematic; it's
#' just that when someone mentions "r = 0.4", you think of panel (1) rather
#' than panel (16).
#' 
#' @export
#' @examples
#' plot_r(r = -0.30, n = 25)
#' plot_r(r = -0.30, n = 25, showdata = TRUE)
#' 
#' # Only show the data for the 12th plot
#' plot_r(r = 0.5, n = 250, showdata = 12)
#' 
#' # Generate data for the 14th plot, don't draw scatterplots
#' plot_r(r = 0.3, n = 12, showdata = 14, plot = FALSE)

plot_r <- function(r = 0.6, n = 50, showdata = FALSE, plot = TRUE) {
  
  # This function draws 16 scatterplots of bivariate
  # data. The scatterplots look quite different, but
  # their sample correlation coefficient are all identical.
  # The take-home message is: plot your data.
  
  if (r >= 1 | r < -1) {
    stop("r needs to be equal or larger than -1 but smaller than (and not equal to) 1.")
  }
  
  # Function for scaling x and y data to [0, 1]-interval.
  scale01 <- function(x) {
    return((x - min(x))/max(x - min(x)))
  }
  
  # Function for computing y vector. Largely copy-pasted from
  # http://stats.stackexchange.com/questions/15011/generate-a-random-variable-with-a-defined-correlation-to-an-existing-variable/15040#15040
  compute.y <- function(x, y, r) {
    theta <- acos(r)
    X     <- cbind(x, y)                              # matrix
    Xctr  <- scale(X, center = TRUE, scale = FALSE)   # centered columns (mean 0)
    
    Id   <- diag(n)                                   # identity matrix
    Q    <- qr.Q(qr(Xctr[, 1, drop = FALSE]))         # QR-decomposition, just matrix Q
    P    <- tcrossprod(Q)          # = Q Q'           # projection onto space defined by x1
    x2o  <- (Id - P) %*% Xctr[, 2]                    # x2ctr made orthogonal to x1ctr
    Xc2  <- cbind(Xctr[, 1], x2o)                     # bind to matrix
    Y    <- Xc2 %*% diag(1 / sqrt(colSums(Xc2 ^ 2)))  # scale columns to length 1
    
    y <- Y[, 2] + (1 / tan(theta)) * Y[, 1]     # final new vector
    return(scale01(y))
  }
  
  # Graph settings
  if (plot == FALSE) {
    pdf(file = NULL)
  }
  op <- par(no.readonly = TRUE)
  par(
    las = 1,
    mfrow = c(4, 4),
    xaxt = "n",
    yaxt = "n",
    mar = c(1, 1.5, 2, 1.5),
    oma = c(0, 0, 3.5, 0),
    cex.main = 1.1
  )
  
  # Case 1: Textbook case with normal x distribution and normally distributed residuals
  x <- rnorm(n)           # specify x distribution
  y <- rnorm(n)           # specify y distribution
  y <- compute.y(x, y, r) # recompute y to fit with x and r
  x <- scale01(x)
  
  plot(                   # plot
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(1) Normal x, normal residuals", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df1 <- data.frame(x, y, type = 1) # store x and y to data frame
  
  
  # Case 2: Textbook case with uniform x distribution and normally distributed residuals
  x <- runif(n, 0, 1)
  y <- rnorm(n)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(2) Uniform x, normal residuals", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df2 <- data.frame(x, y, type = 2)
  
  # Case 3: Skewed x distribution (positive): A couple of outliers could have high leverage.
  x <- rlnorm(n, 5)
  y <- rnorm(n)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(3) +-skewed x, normal residuals", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df3 <- data.frame(x, y, type = 3)
  
  # Case 4: Skewed x distribution (negative): Same as 3.
  x <- rlnorm(n, 5) * -1 + 5000
  y <- rnorm(n)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(4) --skewed x, normal residuals", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df4 <- data.frame(x, y, type = 4)
  
  # Case 5: Skewed residual distribution (positive), similar to 3-4
  x <- rnorm(n)
  y <- rlnorm(n, 5)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(5) Normal x, +-skewed residuals", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df5 <- data.frame(x, y, type = 5)
  
  # Case 6: Skewed residual distribution (negative), same as 5
  x <- rnorm(n)
  y <- rlnorm(n, 5)
  y <- y * -1
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(6) Normal x, --skewed residuals", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df6 <- data.frame(x, y, type = 6)
  
  # Case 7: Variance increases with x. Correlation coefficient 
  # under/oversells predictive power depending on scenario.
  # Also, significance may be affected.
  x <- rnorm(n)
  x <- sort(x, decreasing = FALSE) + abs(min(x))
  variance <- 500*x # spread increases with x
  y <- rnorm(n, 0, sqrt(variance)) # error term has larger variance with x
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(7) Increasing spread", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df7 <- data.frame(x, y, type = 7)
  
  # Case 8: Same as 7, but variance decreases with x
  x <- rnorm(n)
  x <- x - min(x)
  a <- 500 * (max(x)) # calculate intercept of x-standard deviation function
  variance <- a - 500*x
  y <- rnorm(n, 0, sqrt(variance))
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(8) Decreasing spread", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df8 <- data.frame(x, y, type = 8)
  
  # Case 9: Nonlinearity (quadratic trend).
  x <- rnorm(n)
  y <- x ^ 2 + rnorm(n, sd = 0.2)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(9) Quadratic trend", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df9 <- data.frame(x, y, type = 9)
  
  # Case 10: Sinusoid relationship
  x <- runif(n,-2 * pi, 2 * pi)
  y <- sin(x) + rnorm(n, sd = 0.2)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(10) Sinusoid relationship", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df10 <- data.frame(x, y, type = 10)
  
  # Case 11: A single positive outlier can skew the results.
  x <- rnorm(n - 1)
  x <- c(sort(x), 10)
  y <- c(rnorm(n - 1), 15)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(11) A single positive outlier", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df11 <- data.frame(x, y, type = 11)
  
  # Regression without outlier
  group1 <- x[1:(n - 1)]
  y1 <- y[1:(n - 1)]
  
  xseg1 <- seq(
    from = min(group1),
    to = max(group1),
    length.out = 50
  )
  pred1 <- predict(lm(y1 ~ group1), newdata = data.frame(group1 = xseg1))
  lines(xseg1, pred1, lty = 2, col = "firebrick2")
  
  # Case 12: A single negative outlier - same as 11 but the other way.
  x <- rnorm(n - 1)
  x <- c(sort(x), 10)
  y <- c(rnorm(n - 1),-15)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(12) A single negative outlier", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df12 <- data.frame(x, y, type = 12)
  
  # Regression line without outlier
  group1 <- x[1:(n - 1)]
  y1 <- y[1:(n - 1)]
  
  xseg1 <- seq(
    from = min(group1),
    to = max(group1),
    length.out = 50
  )
  pred1 <- predict(lm(y1 ~ group1), newdata = data.frame(group1 = xseg1))
  lines(xseg1, pred1, lty = 2, col = "firebrick2")
  
  # Case 13: Bimodal y: Actually two groups (e.g. control and experimental). Better to model them in multiple regression model.
  x <- rnorm(n)
  y <- c(rnorm(floor(n / 2), mean = -5), rnorm(ceiling(n / 2), 5))
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(13) Bimodal residuals", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df13 <- data.frame(x, y, type = 13)
  
  # Case 14: Two groups
  # Create two groups within each of which
  # the residual XY relationship is contrary to the overall relationship.
  # This will often produce examples of Simpson's paradox.
  x <- c(rnorm(floor(n / 2),-5), rnorm(ceiling(n / 2), 5))
  y <- -sign(r)* 10 * x + rnorm(n, sd = 0.5) + c(rep(0, floor(n / 2)), rep(3 * sign(r), ceiling(n / 2)))   
  
  # Symbols / colours for each group
  colour <- c(rep("blue", floor(n / 2)), rep("red", ceiling(n / 2)))
  symbol <- c(rep(1, floor(n / 2)), rep(4, ceiling(n / 2)))
  
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    pch = symbol,
    main = paste("(14) Two groups", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df14 <- data.frame(x, y, type = 14)
  
  # Also add lines of regression within each group
  
  # Divide up y variable
  group1 <- x[colour == "blue"]
  group2 <- x[colour == "red"]
  y1 <- y[colour == "blue"]
  y2 <- y[colour == "red"]
  
  xseg1 <- seq(
    from = min(group1),
    to = max(group1),
    length.out = 50
  )
  pred1 <- predict(lm(y1 ~ group1), newdata = data.frame(group1 = xseg1))
  lines(xseg1, pred1, lty = 2, col = "firebrick2")
  
  xseg2 <- seq(
    from = min(group2),
    to = max(group2),
    length.out = 50
  )
  pred2 <- predict(lm(y2 ~ group2), newdata = data.frame(group2 = xseg2))
  lines(xseg2, pred2, lty = 2, col = "firebrick2")
  
  # Case 15: Sampling at the extremes - 
  # this would overestimate correlation coefficient based on all data.
  x <- sort(rnorm(6 * n, sd = 3))
  x1 <- x[1:floor((n / 2))]
  x2 <- x[floor(5.5 * n + 1):(6 * n)]
  x <- c(x1, x2)
  y <- rnorm(n)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(15) Sampling at the extremes", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df15 <- data.frame(x, y, type = 15)
  
  # Case 16: Lumpy x/y data, as one would observe for questionnaire items
  x <- sample(1:5, n, replace = TRUE, prob = sample(c(5, 4, 3, 2, 1))) # unequal proportions for each answer just for kicks
  y <- sample(1:7, n, replace = TRUE)
  y <- compute.y(x, y, r)
  x <- scale01(x)
  plot(
    x,
    y,
    ylab = "",
    xlab = "",
    main = paste("(16) Discrete data", sep = "")
  )
  abline(lm(y ~ x), col = "dodgerblue3")
  df16 <- data.frame(x, y, type = 16)
  
  
  
  # Overall title
  title(
    paste("All correlations: r(", n, ") = ", r, sep = ""),
    outer = TRUE,
    cex.main = 2
  )
  par(op)
  if (plot == FALSE) {
    dev.off()
  }
  
  # Define data frame with numeric output
  #df <- list(plot = 1:16, data = list(df1, df2, df3, df4,
  #                                    df5, df6, df7, df8,
  #                                    df9, df10, df11, df12,
  #                                    df13, df14, df15, df16))
  #df <- structure(df, class = c("tbl_df", "data.frame"), row.names = 1:16)
  df <- rbind(df1, df2, df3, df4,
              df5, df6, df7, df8,
              df9, df10, df11, df12,
              df13, df14, df15, df16)
  
  # Show numeric output depending on setting
  if(!(showdata %in% c(NULL, TRUE, FALSE, 1:16, "all"))) {
    warning("'showdata' must be TRUE, FALSE, 'all', or an integer from 1 to 16.")
  } else if(showdata %in% c(TRUE, "all")) {
    return(df)
  } else if(showdata %in% 1:16) {
    return(subset(df, type == showdata))
  }
}