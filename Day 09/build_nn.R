install_packages('keras')
library(keras)
install_keras()

install_packages('tensorflow')
library(tensorflow)
install_tensorflow()

### Adapted from: https://www.datacamp.com/tutorial/neural-network-models-r ###
ds <- dataset_cifar10()
c(c(x_train, y_train), c(x_test, y_test)) %<-% ds
### train is for training, test for validation.
### Models should then be validated on a third set of never-before-seen data.

x_train <- x_train / 255
x_test <-  x_test / 255

model <- keras_model_sequential()%>%
  # Start with a hidden 2D convolutional layer
  layer_conv_2d(
    filter = 16, kernel_size = c(3,3), padding = "same",
    input_shape = c(32, 32, 3), activation = 'leaky_relu'  ### activation is that threshold from the metrics slide
  ) %>%
  
  # 2nd hidden layer. Why is the input shape 32x32x3? RGB + luminance!
  layer_conv_2d(filter = 32, kernel_size = c(3,3), activation = 'leaky_relu') %>%
  
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # 3rd and 4th hidden 2D convolutional layers
  layer_conv_2d(filter = 32, kernel_size = c(3,3), padding = "same", activation = 'leaky_relu') %>%
  
  layer_conv_2d(filter = 64, kernel_size = c(3,3), activation = 'leaky_relu') %>%
  
  # Use max pooling
  layer_max_pooling_2d(pool_size = c(2,2)) %>%
  layer_dropout(0.25) %>%
  
  # Flatten max filtered output into feature vector
  # and feed into dense layer
  layer_flatten() %>%
  layer_dense(256, activation = 'leaky_relu') %>%
  layer_dropout(0.5) %>%
  
  # Outputs from dense layer
  layer_dense(10, activation = 'softmax') ### Use softmax for multi-class and sigmoid for binary classification

summary(model)


learning_rate <- learning_rate_schedule_exponential_decay(
  initial_learning_rate = 5e-3,
  decay_rate = 0.96,
  decay_steps = 1500,
  staircase = TRUE
)
opt <- optimizer_adamax(learning_rate = learning_rate)

loss <- loss_sparse_categorical_crossentropy(from_logits = TRUE)


model %>% compile(
  loss = loss,
  optimizer = opt,
  metrics = "accuracy" ### Why is accuracy ok here? 
)

hist <- model %>% fit(
  x_train, y_train,
  batch_size = 32,
  epochs = 10, ### maybe set to 2 or 3 for slower laptops
  validation_data = list(x_test, y_test),
  shuffle = TRUE ### why shuffle?
)

hist
plot(hist) ### When do we stop training?

model %>% save_model_hdf5('my_cnn_model.hdf5')

predict <- model %>% predict(x_test)
predict[4,] ### predict label of any image, as long as it can be converted to a 32x32x3 matrix

###
# Same basic idea for sentiment, NER, image segmentation, object detection (photoshop contours), 
# speech recognition, captioning, and any other classification task.
###
