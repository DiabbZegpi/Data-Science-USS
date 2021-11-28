library(keras)

model_flags <- flags(
  flag_integer("layer_1", default = 10),
  flag_integer("layer_2", default = 10),
  flag_integer("layer_3", default = 10),
  flag_integer("layer_4", default = 10),
  flag_integer("layer_5", default = 10)
)

model_keras <- keras_model_sequential() %>% 
  layer_dense(units = model_flags$layer_1, activation = "relu", input_shape = 59) %>% 
  layer_dropout(rate = .4) %>% 
  layer_dense(units = model_flags$layer_2, activation = "relu") %>% 
  layer_dropout(rate = .3) %>% 
  layer_dense(units = model_flags$layer_3, activation = "relu") %>% 
  layer_dropout(rate = .2) %>% 
  layer_dense(units = model_flags$layer_4, activation = "relu") %>% 
  layer_dropout(rate = .1) %>% 
  layer_dense(units = model_flags$layer_5, activation = "relu") %>% 
  layer_dense(units = 2, activation = "softmax")

model_keras %>% compile(
  optimizer = optimizer_rmsprop(learning_rate = .001),
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)


history <- model_keras %>% fit(
  x_train, y_train,
  epochs = 20, 
  batch_size = 128,
  validation_split = .2
)

plot(history)

score <- model_keras %>% evaluate(
  x_test, y_test,
  verbose = 0
)