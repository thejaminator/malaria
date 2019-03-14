#i set the python path to where i installed keras, you may or may not have to do this
use_python("C:/Users/user/ANACON~3/envs/rstudio/python.exe", required = TRUE)
py_config()

library(reticulate)
library(dplyr)
library(keras)
library(pbapply)
library(stringr)
library(EBImage)


set.seed(100)
setwd("C:/Users/user/Google Drive/unilaptop/r projects/malaria")

#load the stuff saved in train_model.R
model = load_model_hdf5("CNN.h5")
load(file="test_array.Rda")
testData=array_reshape(test_array,c(8267,2500))
#look at the training history
plot(history)

#get the predictions(1 or 0) and probabiltiies (0 to 1)
predictions <-  predict_classes(model, test_array)
probabilities <- predict_proba(model, test_array)

#visual inspection using some code adapted from https://github.com/monogenea/CNNtutorial/blob/master/script.R
#generate random indexes
random <- sample(1:nrow(testData), 32)
preds <- predictions[random,]
probs <- as.vector(round(probabilities[random,], 2))


par(mfrow = c(4, 8), mar = rep(0, 4))
for(i in 1:length(random)){
  image(t(apply(test_array[random[i],,,], 2, rev)),
        col = gray.colors(12), axes = F)
  legend("topright", legend = ifelse(preds[i] == 0, "Parasitized", "Uninfected"),
         text.col = ifelse(preds[i] == 0, 2, 4), bty = "n", text.font = 2)
  legend("topleft", legend = probs[i], bty = "n", col = "white")
}
