library(tibble)

data(iris)
iris <- as_tibble(iris)

# tibble implies data.frame
# data.frame implies list

fit <- lm( Sepal.Length ~ ., iris)

# Turn a data.frame into a matrix.

iris_mat <- as.matrix(iris[, -5]) 

levels(iris$Species)

# Change the reference group to virginica.
iris$Species <- relevel(iris$Species, "virginica")

levels(iris$Species)

summary(lm( Sepal.Length ~ ., iris))

# Create a model matrix.

form <- Sepal.Length ~ .

X <- model.matrix(form, iris)

Y <- matrix(iris$Sepal.Length, ncol = 1)

beta <- solve( t(X) %*% X ) %*% t(X) %*% Y






