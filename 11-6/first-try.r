library(keras)
library(glmnet)
library(doMC)

registerDoMC()

# The data, shuffled and split between train and test sets
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

x_train <- array_reshape(x_train, c(60000, 28^2))
x_test <- array_reshape(x_test, c(10000, 28^2))
y_train <- factor(y_train)
y_test <- factor(y_test)

s <- sample(seq_along(y_train), 1000)
fit <- cv.glmnet(x_train[s,], y_train[s], family = "multinomial")

preds <- predict(fit$glmnet.fit, x_train[s,], s = fit$lambda.min, 
                 type = "class")

table(as.vector(preds), y_train[s])

preds <- predict(fit$glmnet.fit, x_test, s = fit$lambda.min, 
                 type = "class")

t <- table(as.vector(preds), y_test)

sum(diag(t)) / sum(t)
