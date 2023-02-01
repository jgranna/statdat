# generate data

# structure:
# 1  2  3  4 
# 5  6  7  8
# 9  10 11 12
# 13 14 15 16
# 17 18 19 20

# set wd to nn folder and read from csv
data <- read.csv("data.csv", header = TRUE)
data$number <- factor(data$number)

# load libraries
library(neuralnet)
library(keras)
library(dplyr)

# define reLU activation function
softplus <- function(x) log(1 + exp(x))
sigmoid = function(x) {
  1 / (1 + exp(-x))
}
step <- function(x) ifelse(x<0, 0, ifelse(x>0, 1, 0))

# -----------------------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~ LNF interactive network 14x10 ~~~~~~~~~~~~~~~~~~~~~~~~~~~

# set wd to nn folder and read from csv
load("~/git/lnf/code/NeuralNet/data/train_nn_14x10.rda")
data <- data.frame(train_s)
names(data) <- c('number', paste0('c', 1:140))
data$number <- factor(data$number)

# load libraries
library(neuralnet)
library(keras)
library(dplyr)

# compute 2nd nn with keras package
y_data <- to_categorical(data$number, num_classes = 10)
x_data <- as.matrix(data[, 2:141])
model <- keras_model_sequential() %>%   
  layer_dense(units = 6, activation = "relu", input_shape = ncol(x_data)) %>%
  layer_dense(units = 6, activation = "relu") %>%
  layer_dense(units = ncol(y_data), activation = "softmax")
compile(model, loss = "categorical_crossentropy", optimizer = optimizer_rmsprop(), metrics = "accuracy")
nnet <- fit(model,  x_data, y_data, epochs = 350, batch_size = 60)
y_data_pred <- model %>% predict(x_data)
y_data_pred <- apply(y_data_pred, 1, which.max) - 1
glimpse(y_data_pred)

# save 
save_model_hdf5(model, "~/git/lnf/code/NeuralNet/data/keras_14x10_softmax.h5")
model <- load_model_hdf5("~/git/lnf/code/NeuralNet/data/keras_14x10_softmax.h5")

# see detailed accuracy
t <-table(data$number, y_data_pred)
t

# plot of neural net
library(NeuralNetTools)
inneu <- 9
wts <- get_weights(model)
wts_in <- NA
for (i in 1:4) {
  wts_in <- c(wts_in, wts[[2]][i], wts[[1]][1:inneu, i])
}
for (i in 1:4) {
  wts_in <- c(wts_in, wts[[4]][i], wts[[3]][1:4, i])
}
for (i in 1:10) {
  wts_in <- c(wts_in, wts[[6]][i], wts[[5]][1:4, i])
}
wts_in <- wts_in[-1]
struct <- c(inneu, 4, 4, 10)
plotnet(wts_in, struct = struct, bias = FALSE)
sum(diag(t) / nrow(data))
