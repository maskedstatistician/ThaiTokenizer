rm(list=ls())
Sys.setlocale(locale = "thai")
Sys.setlocale("LC_TIME", "thai")
library(reticulate)
#choose which python environment
reticulate::use_python("Change the repertory of python ")
#check if we are able to load keras
model <- keras_model_sequential()
library("data.table")
library("dplyr")
library("caret")
library("e1071")
library("keras")

#starts here:
path <- 'Choose your repertory'
output_training <- paste0(path,'/training_set/')
output_testing <- paste0(path,'/testing_set/')
output_model <- paste0(path,'/model/')

x_train <- fread(paste(output_training,"x_train.csv",sep=""),colClasses=c(char="integer"))
y_train <- fread(paste(output_training,"y_train.csv",sep=""))
x_train[is.na(x_train)]<- 0 

## Transform into matrix 
x_train <- as.matrix(x_train)
y_train <- as.matrix(y_train)


##############
# CNN LSTM MY BEST MODEL 
############

##144 is the number of distinct character we can have

model <- keras_model_sequential() model %>%
 layer_embedding(144, 64, input_length = dim(x_train)[2])  %>%
 layer_lstm(64)  %>%
 layer_activation("relu")  %>%
 layer_dropout(0.25)  %>%
 layer_dense(1)  %>%
 layer_activation("sigmoid")
 
model <- compile(
 loss = "binary_crossentropy",
 optimizer = "adam",
 metrics = "accuracy"
)
 
history <- model %>% fit(
 x_train, y_train,
 epochs = 5, batch_size = 32
)

#Save the model 
save_model_hdf5(model, filepath=paste0(output_model,'rnn'), overwrite = TRUE,
                include_optimizer = TRUE)

