# Check that an object is of class `lifelihoodData`

Internally, `lifelihood` has to check multiple times that the passed
object is the expected one.

It basically raises an explicit error if the object is not of class
`lifelihoodData`.

## Usage

``` r
check_valid_lifelihoodData(object)
```

## Arguments

- object:

  An object to test.

## Examples

``` r
if (FALSE) { # \dontrun{
# raise an error
obj <- c(1,2,3)
check_valid_lifelihoodData(obj)

# works (does nothing)
class(obj) = "lifelihoodData"
check_valid_lifelihoodData(obj)
} # }
```
