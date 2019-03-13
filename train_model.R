setwd("C:/Users/user/Google Drive/unilaptop/r projects/malaria")

#load the file we saved from the data_prep script
df = load(file="feature_matrix.Rda")

#you will probably have to install these packages
library(reticulate)
library(dplyr)
library(keras)
library(pbapply)

set.seed(100)

#i set the python path to where i installed keras
use_python("C:/Users/user/ANACON~3/envs/rstudio/python.exe", required = TRUE)
use_condaenv("rstudio", conda="auto")
py_config()

#train test split
# 70% train, 30% test split
train<-sample_frac(total_feature_matrix, 0.7)
sid<-as.numeric(rownames(train)) # because rownames() returns character
test<-total_feature_matrix[-sid,]

# in data_prep we added a label to the data frame, now we need to seperate into x and y
#x is your training variable, y is the target variable to predict 
test_x<-select(test, -"label")
test_y <- select(test, "label")

#repeat for training dataset
train_x<-select(train, -"label")
train_y <- as.matrix(select(train, "label"))

#got to reshape it first for the CNN to understand c(number, dim, dim, channels)
train_array=train_x
train_array = as.matrix(train_array)
train_array <- array_reshape(train_array, c(nrow(train_array),50, 50, 1))
#train_array <- reshape(x, shape(-1, 50, 50, 1))
#checking if split done right 
dim(train_y)

dim(test_x)
dim(test_y)
dim(train_array)


# Initialize a sequential model following the example of datacamp
model <- keras_model_sequential()

# Add layers to the model
model %>%
  #first layer is the convultion layer
  #remember that our images were 50 x 50, grayscale so the input shape would be
  #50 x 50 x 1. If they were RGB( having colour) it would be 50 x 50 x3
  layer_conv_2d(64,kernel_size = c(3,3), activation = 'relu', input_shape = c(50,50,1)) %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  layer_conv_2d(32,kernel_size = c(3,3), activation = 'relu') %>%
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  
  #flatten layer serves as the connection to the dense layer
  layer_flatten() %>%
  
  #final layer must decide the range of probability  0 (bad cells) or towards 1 (goods cells)
  layer_dense(units = 8, activation = 'relu') %>% 
  layer_dense(units = 1, activation = 'sigmoid')
summary(model)

#We compile the model. Since this is a binary classification problem, we use
#binary_crossentropy. Multiple categorical? categorical_crossentropy

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = 'adam',
  metrics = 'accuracy'
)

#fit the model with 5 fold CV
model %>% fit(
  train_array,
  train_y, 
  epochs = 5, 
  batch_size = 5, 
  validation_split = 0.2,
  verbose= 1 #see progress bar
)

# Save model
save(model, file = "CNNmodel.RData")

dim(train_x)
dim(train_y)
