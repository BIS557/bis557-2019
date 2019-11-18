library(tensorflow)
library(keras)

#use_virtualenv("~/r-reticulate")

dl_model <- keras_model_sequential() %>%
  layer_dense(units = 4) %>%
  layer_dense(units = 3, activation = "softmax") 

dl_model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c("accuracy"))

form <- Species ~ . -1

iris_x <- model.matrix(form, iris)
iris_y <- to_categorical(as.numeric(iris$Species) - 1)

dl_model %>% fit(
  iris_x,
  iris_y,
  batch_size = 150,
  epoch = 1000,
  validation_split = 0.1)

dl_acc <- 
  apply(predict(dl_model, iris_x), 1, which.max) == as.numeric(iris$Species)
sum(dl_acc) / length(dl_acc)

library(e1071)

svm_fit <- svm(form, iris)
acc <- predict(svm_fit) == iris$Species
sum(acc) / length(acc)
