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
  config_name = c("config", "config2", "config_pierrick", "example_config_se",
    "example_config_mcmc", "config_gbg")
)
```

## Arguments

- config_name:

  Configuration name. Currently available options:

  - config

  - config2

  - config_pierrick By default, it will use "config".

## Value

Absolute path to the configuration file

## Examples

``` r
use_test_config("config")
#> [1] "/private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/Rtmp0YMnSn/temp_libpath2b7a3f8418db/lifelihood/configs/config.yaml"
use_test_config("config2")
#> [1] "/private/var/folders/yz/zr09txvs5dn18vt4cn21kzl40000gn/T/Rtmp0YMnSn/temp_libpath2b7a3f8418db/lifelihood/configs/config2.yaml"
```
