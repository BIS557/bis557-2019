library(readr)
library(leaflet)
library(dplyr)
library(ggplot2)
library(casl) # devtools::install_github("statsmaths/casl")
library(crayon)
library(rsample)

ridge_regression(Sepal.Length ~., iris)

taxi <- 
  read_csv("https://github.com/statsmaths/ml_data/raw/master/nyc_taxi.csv")

taxi_sub <- taxi %>% sample_n(1000)

m <- leaflet() %>% 
  addTiles() %>%
  addMarkers(lng = taxi_sub$pickup_longitude, lat = taxi_sub$pickup_latitude)

m

m <- leaflet() %>% 
  addTiles() %>%
  addMarkers(lng = taxi_sub$dropoff_longitude, lat = taxi_sub$dropoff_latitude)

m

taxi %>% group_by(weekday) %>% summarize(n())

ggplot(taxi, aes(x = trip_distance)) + geom_histogram() + theme_minimal()

taxi <- taxi %>% mutate(lat_dist = abs(dropoff_latitude - pickup_latitude),
                        lon_dist = abs(dropoff_longitude - pickup_longitude))

form <- duration ~ hour + weekday + lat_dist + lon_dist

fit <- lm(form, data = taxi)

taxi <- na.omit(taxi)

ridge_regression <- function(form, data, lambda = 0) {
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


test <- sample.int(nrow(taxi), 2000)
train <- setdiff(seq_len(nrow(taxi)), test)

casl_util_rmse( taxi$duration[test], 
                predict(lm(form, data = taxi[train,]), taxi[test,]) )

ridge_fit <- ridge_regression(form, taxi[train,], lambda = 0.01)

predict(ridge_fit, taxi[test,])

browser()

casl_util_rmse(taxi$duration[test],
predict(ridge_regression(form, taxi[train,], lambda = 0.01), taxi[test,]))

#folds <- vfold_cv(iris, 10)
#training(folds$splits[[1]])
#testing(folds$splits[[1]])
