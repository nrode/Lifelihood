# Plot goodness-of-fit histogram

Plot a histogram of simulated/refitted log-likelihood values and overlay
a vertical line for the original model log-likelihood.

## Usage

``` r
# S3 method for class 'lifelihoodGOF'
plot(x, bins = 30, fill = "grey80", color = "white", line_color = "red", ...)
```

## Arguments

- x:

  Output of
  [`goodness_of_fit()`](https://nrode.github.io/Lifelihood/reference/goodness_of_fit.md).

- bins:

  Number of histogram bins.

- fill:

  Histogram fill color.

- color:

  Histogram border color.

- line_color:

  Color of the vertical line showing original log-likelihood.

- ...:

  Ignored.

## Value

A ggplot object.
