# Create project on Github

## Create compendium
rrtools::use_compendium("/Users/rodenico/Documents/Pro/Articles/2024_Lifelihood/Lifelihood", open = FALSE)

## Add to .gitignore
usethis::use_git_ignore(".DS_Store")
usethis::use_build_ignore(".DS_Store")
usethis::use_git(message = ":see_no_evil: Ban .DS_Store files")

## Modify DESCRIPTION file
usethis::edit_file("DESCRIPTION")
usethis::use_git(message = ":bulb: Update documentation")

dir.create("data")
dir.create("data/raw_data")
dir.create("data/derived_data")
dir.create("reports")
dir.create("plots")

## Create a R directory and a file for functions
usethis::use_r("utils-pipe")
usethis::use_r("import_data")
usethis::use_r("clean_OTU")
usethis::use_r("find_chimeras")
usethis::use_r("clean_OTU2")

## Update DESCRIPTION file
usethis::use_package("here")
usethis::use_package("readxl")
usethis::use_package("ggplot2")
usethis::use_package("dplyr")
usethis::use_package("FactoMineR")
usethis::use_package("factoextra")
usethis::use_package("randomForest")
usethis::use_package("rpart")
usethis::use_package("rfUtilities")
usethis::use_package("rpart.plot")
usethis::use_package("caret")
usethis::use_package("corrplot")
usethis::use_package("ape")
usethis::use_package("phyloseq")
usethis::use_package("phyloseq.extended")
usethis::use_package("vegan")
usethis::use_package("tidyr")
usethis::use_package("tibble")
usethis::use_package("dada2")
usethis::use_package("lmtest")

## Update NAMESPACE file
devtools::document()

## Load all required packages
devtools::load_all()

