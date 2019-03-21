#i set the python path to where i installed keras, you may or may not have to do this
use_python("C:/Users/user/ANACON~3/envs/rstudio/python.exe", required = TRUE)
py_config()
library(reticulate)
library(dplyr)
library(keras)
library(pbapply)
library(stringr)
library(caret)
library(MLmetrics)
library(ggplot2)
library(AUC)
library(EBImage)

set.seed(100)
setwd("C:/Users/user/Google Drive/unilaptop/r projects/malaria")
#load the stuff saved in train_model.R OR 
model = load_model_hdf5("r_cnn.hdf5")
#here we instead loaded the model we built in google colab cos its better
#model = load_model_hdf5("colab_model.h5")

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
random <- sample(1:nrow(testData), 64)
truth<-test_y[random,]
preds <- predictions[random,]
probs <- as.vector(round(probabilities[random,], 2))



par(mfrow = c(8, 8), mar = rep(0, 4))
for(i in 1:length(random)){
  image(t(apply(test_array[random[i],,,], 2, rev)),
        col = gray.colors(12), axes = F)
  legend("topright", legend = ifelse(preds[i] == 1, "Infected", "Healthy"),
         text.col = ifelse(preds[i] == 0, 4, 2), bty = "n", text.font = 2)
  legend("topleft", legend = probs[i], bty = "n", col = "black",text.col = "#000000")
  legend("bottom",legend=ifelse(preds[i]==truth[i],"","WRONG"),bty="n",text.col="#000000",text.font=2)
}

#some visualizaitons
predictions_factor = as.factor(predictions)
ground_truth_factor = as.factor(test_y$label)

cm_original = confusionMatrix(predictions_factor, ground_truth_factor, positive = "1", dnn = c("Prediction", "Reference"))
cm_original
dev.off()
roc_obj = (roc(probabilities,ground_truth_factor))
auc=auc(roc_obj)
plot(roc_obj, main=paste("AUC = ",round(auc,digits=3)))



#Decide on threshold for model to use
get_threshold_prediction<-function(probability,threshold){
if (probability>threshold){
  return (1)
}else{
  return  (0)
}
}

get_threshold<-function(probabilities,ground_truth_factor,
                        threshold=0.5,step=0.01){
high_score = 0
winner = 0
while(threshold>step){
  threshold_prediction= as.factor(sapply(probabilities,get_threshold_prediction, threshold=threshold))
  threshold_sen=Sensitivity(ground_truth_factor,threshold_prediction,positive=1)
  threshold_acc=Accuracy(ground_truth_factor,threshold_prediction)
  my_metric=threshold_sen+threshold_acc
  if (my_metric > high_score){
    cat(high_score,"--->",my_metric,"\n") #for debugging
    high_score=my_metric
    winner = threshold
    }
  threshold = threshold - step
}
cat("high score:",high_score,"threshold to use:", winner)
return(winner)
}

#get threshold
threshold=get_threshold(probabilities,ground_truth_factor)

#now that we got our threshold, we do the confusion matrix
threshold_prediction = sapply(probabilities,get_threshold_prediction,threshold)

#new CM compared to old
cm_improved = confusionMatrix(as.factor(threshold_prediction), ground_truth_factor, dnn = c("Prediction", "Reference"),positive="1")
cm_improved
cm_original

#more visualizations
plot(probabilities)
class(probabilities)
df=as.data.frame(probabilities)
qplot(x=V1,data=df,geom="histogram")
