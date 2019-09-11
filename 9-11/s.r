library(tibble)
library(dplyr)

# Linear algebra supplement at 
# https://ocw.mit.edu/courses/mathematics/18-06-linear-algebra-spring-2010/

data(iris)
iris <- as_tibble(iris)

form <- Sepal.Length ~ .

X <- model.matrix(form, iris)

Y <- matrix(iris$Sepal.Length, ncol = 1)

beta <- solve( t(X) %*% X ) %*% t(X) %*% Y

# base
iris <- cbind(iris, X %*% beta)
names(iris)[6] <- "Sepal.Length.Hat"

# tidy
data(iris)

# iris %>% as_tibble() == as_tibble(iris)

iris <- iris %>% 
  as_tibble() %>%
  mutate(Sepal.Length.Hat = X %*% beta)

# Is it a good solution? In what sense?

library(ggplot2)

ggplot(iris, aes(x = Sepal.Length, y = Sepal.Length.Hat)) +
  geom_point() +
  theme_minimal()


iris %>% 
  mutate(Residuals = Sepal.Length - Sepal.Length.Hat) %>%
  ggplot(aes(x = Sepal.Length, y = Residuals, color = Species)) +
    geom_point(size = 4) +
    theme_minimal()

iris$Residuals <- iris$Sepal.Length - iris$Sepal.Length.Hat

rs <- iris$Residuals

t(rs) %*% rs

fit <- lm(Sepal.Length ~ Sepal.Width + Petal.Length + Petal.Width + Species,
          iris)

t(fit$residuals) %*% fit$residuals

F <- function(beta, Y, X) {
  drop(t(Y) %*% Y + t(beta) %*% t(X) %*% X %*% beta - 2 * t(Y) %*% X %*% beta)
}

dF <- function(beta, Y, X) {
  (2 * t(X) %*% X %*% beta - 2 * t(X) %*% Y) 
}

beta_k <- matrix(1, ncol = 1, nrow = 6)

F(beta_k, Y, X)

beta_k <- beta_k -  0.0001 * dF(beta_k, Y, X) 

F(beta_k, Y, X)
