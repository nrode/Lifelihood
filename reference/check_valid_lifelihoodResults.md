# Check that an object is of class `lifelihoodResults`

Internally, `lifelihood` has to check multiple times that the passed
object is the expected one.

It basically raises an explicit error if the object is not of class
`lifelihoodResults`.

## Usage

``` r
check_valid_lifelihoodResults(object)
```

## Arguments

- object:

  An object to test.

## Examples

``` r
if (FALSE) { # \dontrun{
# raise an error
obj <- c(1,2,3)
check_valid_lifelihoodResults(obj)

# works (does nothing)
class(obj) = "lifelihoodResults"
check_valid_lifelihoodResults(obj)
} # }
```
