library(devtools)

document("~/projects/casl")
library(ggplot2)
library(dplyr)

X <- matrix(runif(1000, min = -1, max = 1), ncol = 1)
y <- X[,1, drop = FALSE]^2 + rnorm(1000, sd = 0.1)

d <- tibble(X = X[,1], y = y[,1]) 

weights <- casl_nn_sgd(X, y, size = c(1, 25, 1), epochs = 25, eta = 0.01)

y_hat <- casl_nn_predict(weights, X)[,1]
d <- d %>% mutate(y_hat = y_hat)

p <- ggplot(d, aes(x = X, y = y)) + 
  geom_point() +
  geom_line(aes(x = X, y = y_hat), color = "red", size = 1.5) +
  theme_minimal()

p




