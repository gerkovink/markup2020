generate_data <- function(num, b0, b1, meanx, sdx, sde) {
  set.seed(10)
  x <- rnorm(num, meanx, sdx)
  y <- b0 + b1 * x + rnorm(num, 0, sde)
  data <- data.frame(x = x, y = y)
  model <- lm(y ~ x, data = data)
  beta0 <- coef(model)[1]
  beta1 <- coef(model)[2]
  return(list(data, beta0, beta1))
}

