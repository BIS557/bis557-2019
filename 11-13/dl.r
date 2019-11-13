library(tensorflow)
library(keras)

#use_virtualenv("~/r-reticulate")

dl_model <- keras_model_sequential() %>%
  layer_dense(units = 3, activation = "softmax") 

dl_model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c("accuracy"))

form <- Species ~ . -1

iris_x <- model.matrix(form, iris)
iris_y <- to_categorical(iris$Species)

dl_model %>% fit(
  iris_x
  iris_y
  batch_size = 50,
  validation_split = 0.1)
