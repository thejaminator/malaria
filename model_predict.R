load(file = "CNNmodel.RData")
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
