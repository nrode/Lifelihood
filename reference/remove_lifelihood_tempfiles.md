# Remove all lifelihood temporary files

By default,
[`lifelihood()`](https://nrode.github.io/Lifelihood/reference/lifelihood.md)
deletes all the temp files it creates, but users can set
`delete_temp_files=FALSE` to keep them.

After multiple runs, there can be lots of temp files. This function will
just remove them.

## Usage

``` r
remove_lifelihood_tempfiles(path = ".")
```

## Arguments

- path:

  Where to look for. Default to current directory.

## Examples

``` r
remove_lifelihood_tempfiles()
```
