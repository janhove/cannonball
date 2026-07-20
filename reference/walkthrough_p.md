# Walkthrough p-values

This function produces a step-by-step demonstration of a significance
test for a two-group comparison.

## Usage

``` r
walkthrough_p(n = 10, diff = 0, sd = 1, showdata = FALSE, pedant = FALSE)
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

- pedant:

  Do you want to run the significance test in pedant mode (`TRUE`) or
  not (`FALSE`, default)? See Details.

## Details

Data are generated from a normal distribution with the requested
standard deviation. Then, the data points are randomly assigned to two
equal-sized groups. Data points in the intervention group receive a
boost as specified by `diff`. Finally, a significance test is run on the
data.

By default, the significance test is a two-sample Student's t-test.
Technically, the p-value from this test is the probability that a
t-statistic larger than the one observed would've been observed if only
chance were at play, but the walkthrough text says that is the
probability that a mean difference larger than the one observed would've
been observed if only chance were at play. That is, I use the t-test as
an approximation to a permutation test. Switch on pedant mode if you
want to run a permutation test.

## Examples

``` r
if (FALSE) { # \dontrun{
walkthrough_p(n = 12, diff = 0.2, sd = 1.3)

# Save data and double check results
dat <- walkthrough_p(n = 10, diff = 0.2, sd = 2, showdata = TRUE)
t.test(score ~ group, data = dat, var.equal = TRUE)

# Run in pedant mode (= permutation test)
dat <- walkthrough_p(n = 13, diff = 1, sd = 4, pedant = TRUE, showdata = TRUE)
t.test(score ~ group, data = dat, var.equal = TRUE)
} # }
```
