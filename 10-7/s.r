library(readr)
library(leaflet)
library(dplyr)
library(ggplot2)
library(casl) # devtools::install_github("statsmaths/casl")
library(crayon)
library(rsample)
library(foreach)

taxi <- 
  read_csv("https://github.com/statsmaths/ml_data/raw/master/nyc_taxi.csv")

taxi_viz <- FALSE

if (taxi_viz) {
  taxi_sub <- taxi %>% sample_n(1000)

  m <- leaflet() %>% 
    addTiles() %>%
    addMarkers(lng = taxi_sub$pickup_longitude, 
               lat = taxi_sub$pickup_latitude)

  m

  m <- leaflet() %>% 
    addTiles() %>%
    addMarkers(lng = taxi_sub$dropoff_longitude, 
               lat = taxi_sub$dropoff_latitude)

  m

  taxi %>% group_by(weekday) %>% summarize(n())

  ggplot(taxi, aes(x = trip_distance)) + 
    geom_histogram() + 
    theme_minimal()
}

taxi <- taxi %>% 
  mutate(lat_dist = abs(dropoff_latitude - pickup_latitude),
         lon_dist = abs(dropoff_longitude - pickup_longitude))

form <- duration ~ hour + weekday + lat_dist + lon_dist

fit <- lm(form, data = taxi)

taxi <- na.omit(taxi)

ridge_regression <- function(form, data, lambda = 0) {
  rownames(data) <- NULL
  X <- model.matrix(form, data)
  # Y <- data[as.numeric(rownames(X)), as.character(form)[2]]
  Y <- data[[as.character(form)[2]]][as.numeric(rownames(X))]
  ret <- solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
  attributes(ret)$formula <- form
  class(ret) <- c(class(ret), "ridge_regression")
  ret
}

predict.ridge_regression <- function(object, ...) {
  dots <- list(...)
  x_frame <- dots[[1]]
  if (!is.data.frame(x_frame)) {
    stop(red("The first argument should be a data.frame of values",
             "to predict"))
  }
  X <- model.matrix(attributes(object)$formula, x_frame)
  X %*% object
}


test <- sample.int(nrow(taxi), 4000)
train <- setdiff(seq_len(nrow(taxi)), test)

casl_util_rmse( taxi$duration[test], 
                predict(lm(form, data = taxi[train,]), taxi[test,]) )

ridge_fit <- ridge_regression(form, taxi[train,], lambda = 0.01)

predict(ridge_fit, taxi[test,])


casl_util_rmse(taxi$duration[test],
  predict(lm(form, taxi[train,]), taxi[test,]))

`%test%` <- function(x, y) {
  cat(x, " is on the left and ", y, " is on the right\n")
}

library(doParallel)
registerDoParallel(6)

lambdas <- seq(0, 0.5, 0.01)
rmse <- foreach(lambda = lambdas, .combine = c) %dopar% {
  casl_util_rmse(taxi$duration[test],
    predict(ridge_regression(form, taxi[train,], lambda = lambda), 
            taxi[test,]))
}

ggplot(tibble(lambda = lambdas, RMSE = rmse), aes(x = lambda, y = RMSE)) +
  geom_point(size = 5) +  
  theme_minimal()

folds <- vfold_cv(taxi, 10)

# Get the first training data set.
train1 <- training(folds$splits[[1]])
test1 <- testing(folds$splits[[1]])

rmses <- foreach(lambda = lambdas, .combine = rbind) %dopar% {
  foreach(i = seq_len(nrow(folds)), .combine = c) %do% {

    casl_util_rmse(testing(folds$splits[[i]])$duration,
      predict(ridge_regression(form, training(folds$splits[[i]]), 
                               lambda = lambda), 
                               testing(folds$splits[[i]])))
  }
}

