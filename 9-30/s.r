library(readr)
library(leaflet)
library(dplyr)
library(ggplot2)
library(casl) # devtools::install_github("statsmaths/casl")

ridge_regression <- function(form, data, lambda = 0) {
  X <- model.matrix(form, data)
  Y <- data[as.numeric(rownames(X)), as.character(form)[2]]
  solve( crossprod(X) + diag(rep(lambda, ncol(X))) ) %*% t(X) %*% Y
}

ridge_regression(Sepal.Length ~., iris)

get_hour <- function(dt) {
  format(dt, "%H") %>% as.numeric
}

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

test <- sample.int(nrow(taxi), 2000)
train <- setdiff(seq_len(nrow(taxi)), test)

casl_util_rmse( taxi$duration[test], 
                predict(lm(form, data = taxi[train,]))[test] )

