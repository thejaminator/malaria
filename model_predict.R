#i set the python path to where i installed keras, you may or may not have to do this
use_python("C:/Users/user/ANACON~3/envs/rstudio/python.exe", required = TRUE)
py_config()

library(reticulate)
library(dplyr)
library(keras)
library(pbapply)
library(stringr)
library(caret)
library(EBImage)


set.seed(100)
setwd("C:/Users/user/Google Drive/unilaptop/r projects/malaria")
#load the stuff saved in train_model.R
model = load_model_hdf5("CNN.h5")
#load the image array
load(file="test_array.Rda")
#shape the images back for display
testData=array_reshape(test_array,c(nrow(test_array),2500))
#load the ground truth for comparison
load(file="ground_truth.Rda")


#get the predictions(1 or 0) and probabiltiies (0 to 1)
predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)

#visual inspection using some code adapted from https://github.com/monogenea/CNNtutorial/blob/master/script.R
#generate random indexes
random <- sample(1:nrow(testData), 32)
preds <- predictions[random,]
probs <- as.vector(round(probabilities[random,], 2))

#to do: add correct / wrong label
par(mfrow = c(4, 8), mar = rep(0, 4))
for(i in 1:length(random)){
  image(t(apply(test_array[random[i],,,], 2, rev)),
        col = gray.colors(12), axes = F)
  legend("topright", legend = ifelse(preds[i] == 1, "Parasitized", "Uninfected"),
         text.col = ifelse(preds[i] == 0, 4, 2), bty = "n", text.font = 2)
  legend("topleft", legend = probs[i], bty = "o", col = "black",text.col = "#FFFFFF", bg = "#000000")
}


dim(predictions)
dim(test_y)
class(predictions)
class(test_y)
predictions_factor = as.factor(predictions)

#cant turn into factor??????
ground_truth=as.vector(test_y)
ground_truth=as.factor(ground_truth)
predictions_factor
ground_truth

confusionMatrix(predictions_factor, ground_truth, positive = NULL, dnn = c("Prediction", "Reference"))
