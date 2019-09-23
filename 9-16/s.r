library(tibble)
library(ggplot2)
library(dplyr)

data(iris)

iris$Sepal.Length2 <- iris$Sepal.Length

lm(Sepal.Width ~ ., data = iris)

X <- model.matrix(Sepal.Width ~ ., data = iris)

Y <- matrix(iris$Sepal.Width , ncol = 1)

# 1 on the off-diagonal means colinear.
cor(X)

# This will fail
beta <- solve( t(X) %*% X ) %*% t(X) %*% Y

tibble(Sigma = svd(X)$d, K = 1:7) %>%
  ggplot(aes(x = K, y = Sigma)) +
  geom_line(size = 4) +
  theme_minimal()

# Orthogoanlize the columns.

s <- svd(X)

X_orth <- s$u %*% diag(s$d)

cor(X_orth)

s <- svd(X)

beta <- s$v %*% diag(1/d[1:) %*% t(u) %*% Y

lm.fit(X, Y)$coefficients




