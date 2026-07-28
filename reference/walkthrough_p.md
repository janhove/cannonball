# Walkthrough p-values

This function produces a step-by-step demonstration of a significance
test for a two-group comparison.

## Usage

``` r
walkthrough_p(n = 10, diff = 0, sd = 1, showdata = FALSE, M = NULL)
```

## Arguments

- n:

  The number of data points per group.

- diff:

  The boost that participants in the intervention group receive.

- sd:

  The standard deviation of the normal distributions from which the data
  are drawn.

- showdata:

  Do you want to output a dataframe containing the plotted data (`TRUE`)
  or not (`FALSE`, default)?

- M:

  `NULL` (default) when using exhaustive randomisation testing; else set
  to the number of Monte Carlo runs desired.

## Details

Data are generated from a normal distribution with the requested
standard deviation. Then, the data points are randomly assigned to two
equal-sized groups. Data points in the intervention group receive a
uniform boost as specified by `diff`. Finally, a significance test is
run on the data. This significance test is a randomisation test using
the mean difference as the test statistic. The p-value reported is a
two-sided one.

## Examples

``` r
if (FALSE) { # \dontrun{
walkthrough_p(n = 12, diff = 0.2, sd = 1.3)

# Save data and double check results using Welch t-test
dat <- walkthrough_p(n = 10, diff = 0.2, sd = 2, showdata = TRUE)
t.test(score ~ group, data = dat)
} # }
```
