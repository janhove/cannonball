#' @title Simulate data for a cluster-randomised experiment
#'
#' @description In educational settings, it's often impossible to randomly assign each individual
#'              students to one of an experiment's conditions. Instead, all students in the same class
#'              or school are assigned to the same condition. This type of random assignment is known as
#'              cluster randomisation. Importantly, if you analyse the data from a cluster-randomised
#'              experiment as though the participants were assigned randomly on an individual basis
#'              (e.g., using a t-test on the individual outcomes), your Type-I error rate (false positive)
#'              goes through the roof. To illustrate this, this function simulates data from a cluster-randomised
#'              experiment. Covariate scores related to the outcome (referred to as pretest scores here)
#'              can also be generated.
#'
#' @details This function works as follows. The within-cluster variance for the participants'
#'          latent pre-intervention skill is set to 1. The within-cluster variance is then computed
#'          on the basis of the desired ICC. Then, pre- and posttest measurements are generated
#'          by adding random normal measurement error to the participants' latent pre-intervention skill.
#'          The variance of the measurement error is computed on the basis of the desired correlation
#'          between the pretest and pre-intervention outcome scores; in this case, the reliabilities
#'          of the pretest and outcome scores are set to be equal.
#'          If no correlation is specified, the measurement error is computed on the basis of the desired
#'          pretest and posttest reliabilities.
#'          Finally, a constant intervention effect is added to the outcome scores of the participants
#'          in the intervention condition.
#'
#'          Note that the square root of the product of the pre- and posttest reliabilities yields
#'          the correlation between them. So if you want to have a correlation between pretest and posttest
#'          scores (without an intervention effect) of 0.7 and you want your pretest to be correlated
#'          at 0.9 to the pre-intervention latent skill levels (i.e., have a reliability of 0.9),
#'          then solve \eqn{\sqrt{0.9x} = 0.7} for \eqn{x} (\eqn{x = 0.7^2/0.9 = 0.544}).
#'
#' @param n_per_class       The number of participants per class. The first half of the classes are
#'                          assigned to the control condition, the second half to the intervention condition.
#' @param ICC               The intra-class correlation. An ICC of 1 means that the observations in each class are
#'                          identical to each other; an ICC of 0 means that observations within the same class aren't
#'                          any more alike than observations from different classes. Defaults to 0.15.
#' @param effect            The population-level effect of the intervention. Defaults to 0.
#' @param rho_prepost       The desired population-level correlation between the measured pretest scores and outcomes.
#' @param reliability_post  The reliability of the outcomes. Default to 1, meaning there is no measured error on the outcome.
#' @param reliability_pre   The reliability of the pretest scores.
#' @keywords cluster randomisation, simulation
#' @export
#' @references Vanhove, Jan. 2015. Analyzing randomized controlled interventions: Three notes for applied linguists. \emph{Studies in Second Language Learning and Teaching} 5(1). 135-152. \url{http://doi.org/10.14746/ssllt.2015.5.1.7}. (Correction note: \url{http://pressto.amu.edu.pl/index.php/ssllt/article/view/5827/5895}.)
#' @examples
#' # Generate and plot data with ICC = 0
#' d <- clustered_data(ICC = 0)
#' library(tidyverse)
#' ggplot(data = d,
#'        aes(x = reorder(class, outcome, FUN = median),
#'            y = outcome)) +
#'   geom_boxplot(outlier.shape = NA) +
#'   geom_point(shape = 1,
#'              position = position_jitter(width = 0.2)) +
#'   facet_wrap(~ condition, scales = "free_x") +
#'   xlab("outcome")
#'
#' # Generate and plot data with ICC = 0.2
#' d <- clustered_data(ICC = 0.2)
#' ggplot(data = d,
#'        aes(x = reorder(class, outcome, FUN = median),
#'            y = outcome)) +
#'   geom_boxplot(outlier.shape = NA) +
#'   geom_point(shape = 1,
#'              position = position_jitter(width = 0.2)) +
#'   facet_wrap(~ condition, scales = "free_x") +
#'   xlab("outcome")
#'
#' # Generate and plot data with ICC = 0.6
#' d <- clustered_data(ICC = 0.6)
#' ggplot(data = d,
#'        aes(x = reorder(class, outcome, FUN = median),
#'            y = outcome)) +
#'   geom_boxplot(outlier.shape = NA) +
#'   geom_point(shape = 1,
#'              position = position_jitter(width = 0.2)) +
#'   facet_wrap(~ condition, scales = "free_x") +
#'   xlab("outcome")
#'
#' # Simulate 1000 datasets and analyse them using t-test
#' ps <- replicate(1000,
#'                {
#'                  d <- clustered_data(ICC = 0.2)
#'                  return(t.test(outcome ~ condition, data = d)$p.value)
#'                })
#' hist(ps, breaks = seq(0, 1, 0.05))
#' mean(ps < 0.05)
#' # Note the hugely inflated Type-I error rate!
#'
#' # Generate pretest scores with correlation 0.7 to outcome
#' # and with ICC of latent pre-intervention skill of 0.4
#' d <- clustered_data(ICC = 0.4, rho_prepost = 0.7)
#' with(d, cor(pretest, outcome))
#'
#' # ICC = 0.2, pretest reliability = 0.9, posttest reliability = 0.544
#' # and hence correlation = sqrt(0.9*0.544) =~ 0.7:
#' d <- clustered_data(ICC = 0.2, reliability_pre = 0.9, reliability_post = 0.544)
#' with(d, cor(pretest, outcome))

clustered_data <- function(n_per_class = rep(20, 10),
                           ICC = 0.15,
                           effect = 0,
                           rho_prepost = NULL,
                           reliability_post = 1,
                           reliability_pre = NULL) {
  # n_per_class: The cluster sizes. The first half of the classes is assigned to
  #              the intervention condition, the second half the control condition.
  # ICC: The intra-class correlation coefficient.
  # rho_prepost: Covariate not generated if NULL.

  if (min(n_per_class) <= 0 | !isTRUE(all(n_per_class == floor(n_per_class)))) {
    stop("All values in 'n_per_class' should be positive integers.")
  }

  if (ICC < 0 | ICC >= 1) {
    stop("'ICC' should be at least 0 and should not be 1 or higher.")
  }

  if (!is.null(rho_prepost)) {
    if (rho_prepost <= 0 | rho_prepost > 1) {
      stop("'rho_prepost' should either be set to NULL or should be a number between 0 and 1 (0 and 1 are both allowed).")
    }
  }

  if (!is.null(reliability_pre)) {
    if (reliability_pre <= 0 | reliability_pre > 1) {
      stop("'reliability_pre' should be either set to NULL or should be a number between 0 and 1 (1 is allowed, 0 isn't).")
    }
  }

  if (!is.null(reliability_post)) {
    if (reliability_post <= 0 | reliability_post > 1) {
      stop("'reliability_post' should be either set to NULL (in which case 'rho_prepost' should be specified) or should be a number between 0 and 1 (1 is allowed, 0 isn't).")
    }
  }

  # Number of classes
  nr_classes <- length(n_per_class)

  # Compute between-cluster variance. Within-cluster variance is 1.
  var_between <- ICC / (1 - ICC)

  # Generate cluster effects for latent skill without intervention
  Classes <- data.frame(
    class = factor(1:nr_classes),
    condition = factor(c(rep("control", floor(nr_classes/2)),
                         rep("intervention", ceiling(nr_classes/2)))),
    class_effect = rnorm(n = nr_classes, sd = sqrt(var_between))
  )

  # Generate participant effects for latent skill without intervention
  Participants <- data.frame(
    class = rep(Classes$class, times = n_per_class),
    participant_effect = rnorm(n = sum(n_per_class), sd = 1)
  )

  # Combine
  Data <- merge(Classes, Participants, by = "class")

  # Compute outcome and pretest score. The true variance in the latent skill
  # in the entire dataset is the sum of the within- and between-cluster variances.
  true_variance <- var_between + 1

  # If rho_prepost was not set;
  if (is.null(rho_prepost)) {

    # and neither reliability_pre nor reliability_post was set: throw error
    if (is.null(reliability_post) & is.null(reliability_pre)) {
      stop("You need to specify at least 'reliability_post' or 'rho_prepost'.")
    }

    # and reliability_pre was not set either: don't generate pretest scores
    else if (is.null(reliability_pre)) {
      Data$outcome <- Data$class_effect + Data$participant_effect + as.numeric(Data$condition) * effect + rnorm(nrow(Data), sd = sqrt((true_variance*(1-reliability_post))/reliability_post))
      Data$pretest <- NA
    }

    # otherwise generate both pre- and posttest scores
    else {
      # Generate outcomes
      Data$outcome <- Data$class_effect + Data$participant_effect + as.numeric(Data$condition) * effect + rnorm(nrow(Data), sd = sqrt((true_variance*(1-reliability_post))/reliability_post))
      Data$pretest <- Data$class_effect + Data$participant_effect + rnorm(nrow(Data), sd = sqrt((true_variance*(1-reliability_pre))/reliability_pre))
    }
  }

  # if rho_prepost was set
  else if (!is.null(rho_prepost)) {
    # and at least one of the reliabilities was set: generate pre- and posttest scores
    if (!is.null(reliability_post) | !is.null(reliability_pre)) {
      message("'rho_prepost' was set, so the 'reliability_pre' and 'reliability_post' were ignored, and both were set to the 'rho_prepost' value.")
      reliability_post <- reliability_pre <- rho_prepost
      Data$outcome <- Data$class_effect + Data$participant_effect + as.numeric(Data$condition) * effect + rnorm(nrow(Data), sd = sqrt((true_variance*(1-reliability_post))/reliability_post))
      Data$pretest <- Data$class_effect + Data$participant_effect + rnorm(nrow(Data), sd = sqrt((true_variance*(1-reliability_pre))/reliability_pre))
    }

    # if rho_prepost was 0: generate uncorrelated variable
    else if (rho_prepost == 0) {
      # Generate outcomes
      Data$outcome <- Data$class_effect + Data$participant_effect + as.numeric(Data$condition) * effect
      Data$pretest <- rnorm(nrow(Data))
      message("rho_prepost was set to 0. No measurement error was added to the outcome variable, and an uncorrelated pretest score was generated.")
    }
  }

  Data$outcome <- Data$outcome

  # Output
  Data <- subset(Data, select = c("class", "condition", "outcome", "pretest"))
}
