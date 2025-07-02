n <- 12

longevity <- 10
slope <- 1
geno <- rep(1:2, each = n / 2)
y <- rexp(n = n, rate = 1 / (longevity * geno))
tapply(y, geno, mean)

geno_l <- geno - 1
