# Reveal the true data's position in a diagnostic parade

This function reveals the position of the true data in a parade.

## Usage

``` r
reveal(parade)
```

## Arguments

- parade:

  The name of an object generated using the
  [`parade`](https://janhove.github.io/cannonball/reference/parade.md)
  or
  [`parade_summary`](https://janhove.github.io/cannonball/reference/parade_summary.md)
  function.

## Examples

``` r
# Fit model
m <- lm(mpg ~ disp, data = mtcars)

# Generate parade
my_parade <- parade(m)

# Linearity check
lin_plot(my_parade)
#> `geom_smooth()` using method = 'loess' and formula = 'y ~ x'


# Reveal
reveal(my_parade)
#> The true data are in position 12.
```
