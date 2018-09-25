#' @title Simulate data for a cluster-randomised experiment
#'
#' @description In educational settings, it's often impossible to randomly assign each individual
#'              students to one of an experiment's conditions. Instead, all students in the same class
#'              or school are assigned to the same condition. This type of random assignment is known as
#'              cluster randomisation. Importantly, if you analyse the data from a cluster-randomised
#'              experiment as though the participants were assigned randomly on an individual basis
#'              (e.g., using a t-test on the individual outcomes), your Type-I error rate (false positive)
#'              goes through the roof. To illustrate this, this function simulates data from a cluster-randomised
#'              experiment.
#' @param var_within The variance within each class. Defaults to 1.
#' @param ICC        The intra-class correlation. An ICC of 1 means that the observations in each class are
#'                   identical to each other; an ICC of 0 means that observations within the same class aren't
#'                   any more alike than observations from different classes. Defaults to 0.1.
#' @param parts_per_class		A vector with the number of participants per class. The first half of the classes are
#'                          assigned to the control group, the second half to the experimental group.
#' @param effect The population-level effect of the treatment. Defaults to 0.
#' @keywords cluster randomisation, simulation
#' @export
#' @references Vanhove, Jan. 2015. Analyzing randomized controlled interventions: Three notes for applied linguists. \emph{Studies in Second Language Learning and Teaching} 5(1). 135-152. \url{http://dx.doi.org/10.14746/ssllt.2015.5.1.7}. (Correction note: \url{http://pressto.amu.edu.pl/index.php/ssllt/article/view/5827/5895}.)
#' @examples
#' # Generate and plot data with ICC = 0
#' d <- clustered_data(ICC = 0)
#' library(ggplot2)
#' ggplot(data = d,
#'        aes(x = reorder(class, outcome, FUN = median),
#'            y = outcome)) +
#'   geom_boxplot() +
#'   facet_wrap(~ group_class, scales = "free_x") +
#'   xlab("outcome")
#'
#' # Generate and plot data with ICC = 0.2
#' d <- clustered_data(ICC = 0.2)
#' ggplot(data = d,
#'        aes(x = reorder(class, outcome, FUN = median),
#'            y = outcome)) +
#'   geom_boxplot() +
#'   facet_wrap(~ group_class, scales = "free_x") +
#'   xlab("outcome")
#'
#' # Generate and plot data with ICC = 0.6
#' d <- clustered_data(ICC = 0.6)
#' ggplot(data = d,
#'        aes(x = reorder(class, outcome, FUN = median),
#'            y = outcome)) +
#'   geom_boxplot() +
#'   facet_wrap(~ group_class, scales = "free_x") +
#'   xlab("outcome")
#'
#' # Simulate 1000 datasets and analyse them using t-test
#' ps <- replicate(1000,
#'                {
#'                  d <- clustered_data(ICC = 0.2)
#'                  return(t.test(outcome ~ group_class, data = d)$p.value)
#'                })
#' hist(ps)
#' mean(ps < 0.05)
#' # Note the hugely inflated Type-I error rate!

clustered_data <- function(var_within = 1,
                           ICC = 0.1,
                           parts_per_class = c(10, 25, 13, 12, 14, 15, 18, 16, 23, 8),
                           effect = 0) {

  # var_within: within-cluster variance (population-level)
  # ICC: desired ICC (population-level)
  # parts_per_class: number of participants per class. The first half = control, the second half = intervention.
  # effect: population-level difference between control and intervention

  # Check if 0 <= ICC <= 1
  if (ICC < 0 | ICC > 1) {
  	stop("Only ICC values between 0 and 1 are supported.")
  }

  # Calculate between-cluster variance
  var_between <- (ICC * var_within)/(1-ICC)

  # Get number of classes
  nr_classes <- length(parts_per_class)

  Classes <- data.frame(class = factor(1:nr_classes),
                        effect_class = rnorm(nr_classes, 0, sqrt(var_between)),
                        group_class = c(rep("control", floor(nr_classes/2)),
                                        rep("intervention", ceiling(nr_classes/2)))
  )

  Data <- data.frame(class = rep(factor(1:nr_classes), parts_per_class),
                     effect_part = rnorm(sum(parts_per_class), 0, sqrt(var_within))
  )

  Data <- merge(Data, Classes, by = "class")

  Data$outcome <- Data$effect_part + Data$effect_class + (as.numeric(Data$group_class)-1)*effect

  return(Data)
}
