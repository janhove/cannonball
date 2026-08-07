# Walkthrough blocking on a covariate

This function produces a step-by-step demonstration of how researchers
can 'block' on a continuous covariate and how they can analyse the data
of a randomised block design.

## Usage

``` r
walkthrough_blocking(
  n = 10,
  diff = 0,
  sd = 1,
  rho = 0.8,
  showdata = FALSE,
  M = NULL
)
```

## Arguments

- n:

  The number of data points per group.

- diff:

  The boost that participants in the intervention group receive.

- sd:

  The standard deviation of the normal distributions from which the data
  are drawn.

- rho:

  The correlation between the covariate and the outcome
  (pre-intervention) in the population.

- showdata:

  Do you want to output a dataframe containing the plotted data (`TRUE`)
  or not (`FALSE`, default)?

- M:

  `NULL` (default) when using exhaustive randomisation testing; else set
  to the number of Monte Carlo runs desired.

## Details

Data are generated from a normal distribution with the requested
standard deviation; a covariate is also generated. The data points are
then grouped in pairs based on their covariate scores. Within each pair,
the data points are then randomly assigned to the control or
intervention group. Data points in the intervention group receive a
boost as specified by 'diff'. Finally, a significance test is ran on the
data. This significance test is a randomisation test using the mean
difference as the test statistic. The p-value reported is a two-sided
one.

If `n` is larger than 16 and `M` is not specified, `M` is set to 65536.

## Examples

``` r
if (FALSE) { # \dontrun{
walkthrough_blocking(n = 12, diff = 0.2, sd = 1.3, rho = 0.8)

# Save data and double check results
dat <- walkthrough_blocking(n = 12, diff = 0.2, sd = 1.3, rho = 0.8, showdata = TRUE)
anova(lm(score ~ factor(block) + group, data = dat))
} # }
```
