library(yaml)

config <- yaml.load_file("modele.yaml")

# Accessing list parameter
print(config$model$parameters$regularization)

# Using list parameter
for (reg in config$model$parameters$regularization) {
   print(paste("Regularization value:", reg))
}
