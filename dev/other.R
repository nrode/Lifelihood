x <- rnorm(15)
y <- x + rnorm(15)
predict(lm(y ~ x), se.fit = TRUE)

new <- data.frame(x = seq(-3, 3, 0.5))
predict(lm(y ~ x), new, se.fit = TRUE)
