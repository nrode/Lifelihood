# Get the path to a built-in configuration file.

`lifelihood` embeds a few configuration files, and this function is a
simple tool to access one of them.

It takes the name one of the available configuration and returns the
path to it.

For more info about configuration files, see
[`vignette("setting-up-the-configuration-file", package = "lifelihood")`](https://nrode.github.io/Lifelihood/articles/setting-up-the-configuration-file.md)

## Usage

``` r
use_test_config(
  config_name = c("config", "config_with_tradeoff", "config_without_tradeoff",
    "config_gbg")
)
```

## Arguments

- config_name:

  Configuration name. Currently available options:

  - config

  - config_with_tradeoff

  - config_without_tradeoff

  - config_gbg By default, it will use "config".

## Value

Absolute path to the configuration file

## Examples

``` r
use_test_config("config")
#> [1] "/private/var/folders/36/tjdph2t965j8snz9_vkdnw0r0000gn/T/RtmpyNfNRC/temp_libpath1817631bed60/lifelihood/configs/config.yaml"
```
