#' @title Generate dataset for a diagnostic parade
#'
#' @description This function generates a parade (= 'lineup' in the `nullabor` package)
#' that hides the observations, fitted values, and residuals of a statistical model you
#' want to diagnose among the observations, fitted values, and residuals of 19 similar models
#' that were fitted on simulated outcome data. The 19 sets of simulated outcome data are
#' generated from the original model so that this model's assumptions are literally true
#' for the simulated data. The 'tibble' (dataframe) created by this can be used to draw
#' 20 panels of diagnostic plots (see examples).
#' @param model The name of the statistical model you want to diagnose.
#'              Currently only lm() and gam() (from the `mgcv` package) models are supported.
#' @param full_data By default, the output will only include variables that are part of the model.
#'                  If you want to include all the variables that are present in the dataframe
#'                  on which the model was fitted, supply this dataframe's name to full_data.
#' @keywords model diagnostics, assumptions, lineup protocol
#' @export
#' @examples
#' # A simple regression model
#' m <- lm(mpg ~ disp, data = mtcars)
#'
#' # Generate parade and check linearity
#' my_parade <- parade(m)
#' my_parade
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
parade <- function(model, full_data = NULL) {
  
  # Throw error for unsupported models. Support for lmer() would be nice.
  if (!(class(model)[1] %in% c("lm", "gam"))) {
    stop(paste0("This function currently only works with lm() and gam() models. ",
                "The class of the model you tried to fit was ",
                class(model), "."))
  }
  
  # We'll need the tidyverse (magrittr, broom, dplyr etc.)
  if (!requireNamespace("tidyverse", quietly = TRUE)) {
    stop("The \"tidyverse\" package is needed for this function to work. Please install it.",
         call. = FALSE)
  }
  
  require("tidyverse")
  
  # Get data frame with all variables in model
  df <- model.frame(model)
  
  # Get the name of the outcome variable
  outcome_var <- colnames(df)[1]
  
  # Simulate 19 vectors of outcome data from the model
  simulated_data <- data.frame(simulate(model, n = 19))
  
  # Check if full_data was specified, and if so, if it was correctly specified.
  if (is.null(full_data)) {
    # OK: use model frame to generate data frame for parade
    # Get all the variable names.
    other_vars <- colnames(df)[-1]
    var_names <- c(other_vars, outcome_var)
    df <- cbind(simulated_data, df)
    
  } else if (!is.data.frame(full_data)) {
    # full_data needs to be a data frame (tibbles also work)
    stop(paste0("You supplied an object to 'full_data' that isn't a data frame. ",
                "Its class is ", class(full_data), "."))
    
  } else if (!(all(colnames(df) %in% colnames(full_data)))) {
    # Not all variables in the model occur in full_data
    stop(paste0("The data frame supplied to 'full_data' needs to contains ",
                "all variables used in the model supplied to 'model'."), "\n",
         paste0("The following variables are missing in 'full_data': "), "\n",
         paste(shQuote(colnames(df)[!(colnames(df) %in% colnames(full_data))]), collapse = ", "), "\n",
         paste0("Did you specify the correct data frame?"))
    
  } else if (nrow(df) != nrow(full_data)) {
    # Different datasets?
    stop(paste0("The dataset on which the model was fitted and the one passed to 'full_data' ",
                "contain a different number of observations. ",
                "If you're sure you supplied the correct data frame and model, ",
                "check if full_data contains missing data in the outcome or predictors ",
                "and remove these if necessary."))
  } else if (!is.null(full_data)) {
    # OK: use full_data to generate date frame
    other_vars <- colnames(full_data)[!(colnames(full_data) %in% outcome_var)]
    # Get all the variable names
    var_names <- c(other_vars, outcome_var)
    df <- cbind(simulated_data, df[, outcome_var], full_data[, other_vars])
  }
  
  
  # Rename the columns with the simulated and true outcome variable
  colnames(df)[1:20] <- c(paste("..sample", 1:20))
  
  # Convert df to the long format. For now, ..sample 20 contains
  # the true data.
  df <- df %>%
    gather(key = "..sample",
           value = "outcome",
           starts_with("..sample "))
  
  # Rename the column with the true and simulated outcomes appropriately
  colnames(df)[ncol(df)] <- outcome_var
  
  # Refit the model to each vector of simulated and true outcomes.
  # For more complex models, it may save some time to not refit the
  # last model, but I'll leave that to someone with 1337 R skillz.
  df <- df %>%
    group_by(..sample) %>%
    # Refit the original model to each sample (using update()),
    # output the fitted and residual values (using augment()),
    # and retain dfs variables (using data = .):
    do(broom::augment(update(model, data = .), data = .)) %>%
    # Add transformed residuals
    mutate(.abs_resid = abs(.resid)) %>%
    mutate(.sqrt_abs_resid = sqrt(abs(.resid))) %>%
    # augment() outputs some additional info;
    # only retain variables, fitted values and residuals here
    select("..sample", var_names, ".fitted", ".resid", ".abs_resid", ".sqrt_abs_resid") %>%
    ungroup()
  
  # Generate a random number between 1 and 20. This will be
  # the position of the true data in the parade.
  pos <- sample(1:20, 1)
  
  # This ugly bit moves the true data to position 'pos':
  df_sample <- data.frame(
    .sample2 = paste("..sample", 1:20),
    .sample = c((1:20)[-pos], pos)
  )
  df_sample$.sample2 <- as.character(df_sample$.sample2)
  df <- df %>%
    left_join(df_sample, by = c("..sample" = ".sample2")) %>%
    select(-..sample)
  
  # Add the true data's position as an attribute to the parade.
  attr(df, "position") <- pos
  
  # Add the names of the variables by category
  attr(df, "outcome_var") <- outcome_var
  attr(df, "other_vars") <- other_vars
  attr(df, "predictor_vars") <- colnames(model.frame(model)[-1])
  
  # Specify that this parade contains the raw data, not summaries
  attr(df, "data_type") <- "raw"
   
  # Output the parade
  return(df)
}
