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

sd(lm.fit(X, Y)$residuals)

sd(lm.fit(X %*% s$v, Y)$residuals)

sd(lm.fit(X %*% s$v, Y)$residuals)
sd(lm.fit(X %*% s$v[,1:7] , Y)$residuals)
sd(lm.fit(X %*% s$v[,1:6] , Y)$residuals)
sd(lm.fit(X %*% s$v[,1:5] , Y)$residuals)
sd(lm.fit(X %*% s$v[,1:1] , Y)$residuals)

sd(lm.fit(X[,1, drop = FALSE], Y)$residuals)
sd(lm.fit(X[,2, drop = FALSE], Y)$residuals)
# ...

X_prime <- X %*% s$v


