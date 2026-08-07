# Generate dataset for a diagnostic parade

This function generates a parade (= `lineup` in the nullabor package)
that hides the observations, fitted values, and residuals of a
statistical model you want to diagnose among the observations, fitted
values, and residuals of a number similar models that were fitted on
simulated outcome data. The sets of simulated outcome data are generated
from the original model so that this model's assumptions are literally
true for the simulated data. The 'tibble' (dataframe) created by this
can be used to draw panels of diagnostic plots (see examples).

## Usage

``` r
parade(model, full_data = NULL, size = 20)
```

## Arguments

- model:

  The name of the statistical model you want to diagnose. Currently only
  [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`gam()`](https://rdrr.io/pkg/mgcv/man/gam.html) (from the mgcv
  package) and [`lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) (from
  the lme4 package) models are supported. For the
  [`lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html) models, only
  residual diagnostics are supported; support for BLUP ('random
  effects') diagnostics is still lacking.

- full_data:

  By default, the output will only include variables that are part of
  the model. If you want to include all the variables that are present
  in the dataframe on which the model was fitted, supply this
  dataframe's name to full_data.

- size:

  The number of simulated and actual datasets that the parade will
  contain. This defaults to 20, meaning that the actual dataset will be
  hidden among 19 simulated datasets.

## Value

A tibble containing predictors, outcomes, fitted values and residuals
for both the real dataset and simulated datasets.

## Transformed predictors

If you want to include transformed predictors in the model call (e.g.,
`log(x)`), transform the predictor before using it in the model call
(see examples).

This function relies on
[`augment`](https://generics.r-lib.org/reference/augment.html) in the
broom package. Since `augment()` cannot handle model calls with
[`poly()`](https://rdrr.io/r/stats/poly.html) or `ns()`, `parade()`
can't handle these, either. (For `lmer` models, the
[`augment`](https://generics.r-lib.org/reference/augment.html) function
in the broom.mixed package is used.)

## Examples

``` r
# A simple regression model
m <- lm(mpg ~ disp, data = mtcars)

# Generate parade and check linearity
my_parade <- parade(m)
my_parade
#> # A tibble: 640 × 7
#>     disp   mpg .fitted .resid .abs_resid .sqrt_abs_resid .sample
#>    <dbl> <dbl>   <dbl>  <dbl>      <dbl>           <dbl>   <int>
#>  1  160  19.2     21.9 -2.69       2.69            1.64        1
#>  2  160  21.5     21.9 -0.394      0.394           0.627       1
#>  3  108  23.8     24.1 -0.363      0.363           0.602       1
#>  4  258  20.5     17.8  2.77       2.77            1.66        1
#>  5  360  12.5     13.4 -0.959      0.959           0.979       1
#>  6  225  21.6     19.2  2.41       2.41            1.55        1
#>  7  360   9.97    13.4 -3.44       3.44            1.85        1
#>  8  147. 22.7     22.5  0.247      0.247           0.497       1
#>  9  141. 27.5     22.7  4.72       4.72            2.17        1
#> 10  168. 21.2     21.6 -0.415      0.415           0.645       1
#> # ℹ 630 more rows
lin_plot(my_parade)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

reveal(my_parade)
#> The true data are in position 2.

# Regenerate parade and check constant variance
my_parade <- parade(m)
var_plot(my_parade)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

reveal(my_parade)
#> The true data are in position 1.

# Regenerate parade and check normality
my_parade <- parade(m)
norm_qq(my_parade)

norm_hist(my_parade)

norm_hist(my_parade, bins = 10)

reveal(my_parade)
#> The true data are in position 19.

# If you want to include all predictors in the dataset in the parade:
my_parade <- parade(m, full_data = mtcars)
my_parade
#> # A tibble: 640 × 16
#>      cyl  disp    hp  drat    wt  qsec    vs    am  gear  carb   mpg .fitted
#>    <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>   <dbl>
#>  1     6  160    110  3.9   2.62  16.5     0     1     4     4  22.9    23.7
#>  2     6  160    110  3.9   2.88  17.0     0     1     4     4  26.5    23.7
#>  3     4  108     93  3.85  2.32  18.6     1     1     4     1  24.9    26.0
#>  4     6  258    110  3.08  3.22  19.4     1     0     3     1  24.8    19.3
#>  5     8  360    175  3.15  3.44  17.0     0     0     3     2  13.8    14.8
#>  6     6  225    105  2.76  3.46  20.2     1     0     3     1  24.4    20.8
#>  7     8  360    245  3.21  3.57  15.8     0     0     3     4  14.1    14.8
#>  8     4  147.    62  3.69  3.19  20       1     0     4     2  27.9    24.3
#>  9     4  141.    95  3.92  3.15  22.9     1     0     4     2  21.1    24.6
#> 10     6  168.   123  3.92  3.44  18.3     1     0     4     4  20.7    23.4
#> # ℹ 630 more rows
#> # ℹ 4 more variables: .resid <dbl>, .abs_resid <dbl>, .sqrt_abs_resid <dbl>,
#> #   .sample <int>

# If you want to generate a parade with 50 instead of 20 plots:
my_parade <- parade(m, size = 50)
norm_qq(my_parade)


# The function also works for generalised additive models fitted with mgcv:
library(mgcv)
m.gam <- gam(mpg ~ s(disp) + wt + s(qsec), data = mtcars)
my_parade <- parade(m.gam)
lin_plot(my_parade)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'

my_parade <- parade(m.gam)
norm_qq(my_parade)


m.gam <- gam(mpg ~ te(disp, qsec) + wt, data = mtcars)
my_parade <- parade(m.gam)
lin_plot(my_parade)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# And has some limited support for lmer() models (from the lme4 package)
library(lme4)
#> Loading required package: Matrix
#> 
#> Attaching package: ‘Matrix’
#> The following objects are masked from ‘package:tidyr’:
#> 
#>     expand, pack, unpack
#> 
#> Attaching package: ‘lme4’
#> The following object is masked from ‘package:nlme’:
#> 
#>     lmList
m.lmer <- lmer(Reaction ~ Days + (Days|Subject), data = sleepstudy)
my_parade <- parade(m.lmer)
norm_hist(my_parade, bins = 15)

# Support for diagnosing the BLUPs would be nice.

# Transformed predictors:
# This won't work:
# m <- lm(mpg ~ log2(disp), data = mtcars)
# my_parade <- parade(m)

# This will:
mtcars$log2.disp <- log2(mtcars$disp)
m <- lm(mpg ~ log2.disp, data = mtcars)
my_parade <- parade(m)
```
