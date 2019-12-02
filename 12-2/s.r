# This code was taken from 
# https://keras.rstudio.com/articles/examples/mnist_cnn.html
library(keras)

use_python("/usr/local/bin/python3")

# Data Preparation -----------------------------------------------------

batch_size <- 128
num_classes <- 10
epochs <- 2

# Input image dimensions
img_rows <- 28
img_cols <- 28

# The data, shuffled and split between train and test sets
mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# Redefine  dimension of train/test inputs
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

# Convert class vectors to binary class matrices
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)

# Define Model -----------------------------------------------------------

# Define model
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu',
                input_shape = input_shape) %>%
  layer_conv_2d(filters = 8, kernel_size = c(3,3), activation = 'relu') %>% 
  layer_max_pooling_2d(pool_size = c(2, 2)) %>% 
  layer_dropout(rate = 0.25, name = "viz-out") %>% 
  layer_flatten() %>% 
  layer_dense(units = 64, activation = 'relu') %>% 
  layer_dropout(rate = 0.5) %>% 
  layer_dense(units = 64, activation = 'relu', name = "latent-space") %>% 
  layer_dense(units = num_classes, activation = 'softmax')

# Compile model
model %>% compile(
  loss = loss_categorical_crossentropy,
  optimizer = optimizer_adadelta(),
  metrics = c('accuracy')
)

# Train model
model %>% fit(
  x_train, y_train,
  batch_size = batch_size,
  epochs = epochs,
  validation_split = 0.2
)

scores <- model %>% evaluate(
  x_test, y_test, verbose = 0
)

latent_space_map <- keras_model(inputs = model$inputs,
  outputs = get_layer(model, "latent-space")$output)

xl <- predict(latent_space_map, x_test)

# Output metrics
cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')

s <- svd(xl, nv = 3, nu = 3)
xr <- s$u %*% diag(s$d[1:3]) #%*% t(s$v)

#Test accuracy: 0.9674

library(dplyr)
library(ggplot2)

l <- cbind(tibble(y = apply(y_test, 1, which.max) - 1), as_tibble(xr)) %>%
  as_tibble()

ls <- l[1:100,]

ggplot(ls, aes( x= V1, y = V3, label = as.character(y))) + 
  geom_text(size = 10) 

lsm <- ls %>% mutate(V1 = V1 - min(V1), V2 = V2 - min(V2), V3 = V3 - min(V3))

ips <- tcrossprod(as.matrix(lsm[,2:4]))
d <- diag(ips)
for (i in seq_len(nrow(ips))) {
  ips[i,] <- ips[i,] / sqrt(d[i])
  ips[,i] <- ips[,i] / sqrt(d[i])
}

ips <- ips - diag(diag(ips))

library(igraph)
library(tidygraph)
library(networkviz)

g <- graph_from_adjacency_matrix(ips, mode = "undirected", weighted = TRUE)
tg <- as_tbl_graph(g) %N>%
  mutate(name= as.character(lsm$y))

tg %>%  trim_edges(k = 3*length(V(.))) %>% js()

clus <- cluster_louvain(tg)










