# Get the path to a built-in configuration file.

`lifelihood` embeds a few configuration files, and this function is a
simple tool to access one of them.

It takes the name one of the available configuration and returns the
path to it.

For more info about configuration files, see
[`vignette("setting-up-the-configuration-file", package = "lifelihood")`](https://nrode.github.io/Lifelihood/articles/setting-up-the-configuration-file.md)

## Usage

``` r
get_config_path(config_name = c("config", "config2", "config_pierrick"))
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
get_config_path("config")
#> [1] "/private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/Rtmp7TZGF8/temp_libpath1fc67a1d5b74/lifelihood/configs/config.yaml"
get_config_path("config2")
#> [1] "/private/var/folders/p6/nlmq3k8146990kpkxl73mq340000gn/T/Rtmp7TZGF8/temp_libpath1fc67a1d5b74/lifelihood/configs/config2.yaml"
```
