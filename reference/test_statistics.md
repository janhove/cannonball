# Test statistics for two-group comparisons

These functions compute test statistics that can be used in a
randomisation test for a two-group comparison.

`mean_diff()` computes the mean difference.

`stud_mean_diff()` computes the studentised mean difference. This is
identical to the t-statistic used in Welch's t-test.

`median_diff()` computes the difference between the medians.

`rank_sum()` computes the rank sum in the treatment group. This is the
test statistic used in the Mann-Whitney-Wilcoxon test.

`prob_super()` computes the probability of superiority, i.e., the
probability that a randomly chosen unit in the treatment group has a
higher outcome value than a randomly chosen unit in the control group.
The resulting test is equivalent to when using `rank_sum()`.

`ks_stat()` computes the Kolmogorov-Smirnov test statistic, i.e., the
supremum of the difference between the ecdfs of the outcome in the
treatment and control groups.

## Usage

``` r
mean_diff(outcome, treatment_idx)

stud_mean_diff(outcome, treatment_idx)

median_diff(outcome, treatment_idx)

rank_sum(outcome, treatment_idx)

prob_super(outcome, treatment_idx)

ks_stat(outcome, treatment_idx)
```

## Arguments

- outcome:

  A vector of numeric outcomes.

- treatment_idx:

  The indices of the observations in the treatment group.

## Value

The test statistic for the two-group comparison.

## Examples

``` r
x <- c(seq(-4, 5), 2*seq(-3, 4))
g <- rep(c(0, 1), times = c(10, 8))
idx <- which(g == 1)
mean_diff(x, idx)
#> [1] 0.5
stud_mean_diff(x, idx)
#> [1] 0.2526456
median_diff(x, idx)
#> [1] 0.5
rank_sum(x, idx)
#> [1] 78.5
prob_super(x, idx)
#> [1] 0.53125
ks_stat(x, idx)
#> [1] 0.25
```
