#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#LASSO


rm(list = ls())    #delete objects
cat("\014")

install.packages('rmutil')
install.packages('tictoc')
install.packages('latex2exp')
install.packages('tree')

library(class)
library(ggplot2)
library(dplyr)
library(glmnet)
library(rmutil)
library(tictoc)
library(latex2exp)
library(tree)
library(e1071)

###########################
######## LOAD DATA ########
###########################

seizure = read.csv("data.csv",header=TRUE)
seizure = subset(seizure, select = -c(X) )
seizure$y = ifelse(seizure$y == 1, 'S', 'N')


###########################
######## LASSO ############
###########################

thrs.fill <- list()

FPR.train.fill <- list()
TPR.train.fill <- list()

FPR.test.fill <- list()
TPR.test.fill <- list()


start.time <- Sys.time()

for (j in 0:49) {
  
  #################
  ##### SAMPLE ####
  #################
  
  ## 90% of the sample size
  smp_size <- floor(0.9 * nrow(seizure))
  train_ind <- sample(seq_len(nrow(seizure)), size = smp_size)
  train <- seizure[train_ind, ]
  test <- seizure[-train_ind, ]
  
  
  ##################
  ### FORMATTING ###
  ##################
  
  X.train         =   model.matrix(y~., train)[, -1]
  y.train         =   train$y
  y.train         =   (y.train=='S')*1
  n.train         =   dim(X.train)[1] # sample size
  p.train         =   dim(X.train)[2] # number of predictors/features
  
  
  X.test         =   model.matrix(y~., test)[, -1]
  y.test         =   test$y
  y.test         =   (y.test=='S')*1
  n.test         =   dim(X.test)[1] # sample size
  p.test         =   dim(X.test)[2] # number of predictors/features
  
  #############
  ### MODEL ###
  #############
  
  lasso             =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 1,  intercept = TRUE,  nfolds = 10, type.measure="auc",standardize = FALSE)
  lasso             =     glmnet(X.train, y.train, lambda = lasso$lambda.min, family = "binomial", alpha = 1,  intercept = TRUE,standardize = FALSE)
  
  for (i in 0:100) {
    if (i==0) {
      thrs = 0
    }
    else {
      thrs = i/100
    }
    
    p.hat.train             =     predict(lasso, newx = X.train,  type = "response")
    y.hat.train             =     rep("0",n.train)
    y.hat.train[p.hat.train>thrs]  =     "1"
    FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
    P.train          =    sum(y.train==1) # total positives in the data
    N.train          =    sum(y.train==0) # total negatives in the data
    FPR.train        =    FP.train/N.train
    TPR.train        =    TP.train/P.train
    
    
    p.hat.test             =     predict(lasso, newx = X.test,  type = "response")
    y.hat.test             =     rep("0",n.test)
    y.hat.test[p.hat.test>thrs]  =     "1"
    FP.test         =    sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.test         =    sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
    P.test          =    sum(y.test==1) # total positives in the data
    N.test          =    sum(y.test==0) # total negatives in the data
    FPR.test        =    FP.test/N.test
    TPR.test        =    TP.test/P.test
    
    thrs.fill <- append(thrs.fill, list(thrs))
    
    FPR.train.fill <- append(FPR.train.fill, list(FPR.train))
    TPR.train.fill <- append(TPR.train.fill, list(TPR.train))
    
    FPR.test.fill <- append(FPR.test.fill, list(FPR.test))
    TPR.test.fill <- append(TPR.test.fill, list(TPR.test))
    
  }
}

thrs_df <- as.data.frame(do.call(rbind, thrs.fill))

fpr_train_df <- as.data.frame(do.call(rbind, FPR.train.fill))
tpr_train_df <- as.data.frame(do.call(rbind, TPR.train.fill))

fpr_test_df <- as.data.frame(do.call(rbind, FPR.test.fill))
tpr_test_df <- as.data.frame(do.call(rbind, TPR.test.fill))

train_df <- cbind(thrs_df, fpr_train_df, tpr_train_df)
colnames(train_df) <- c("thrs", "FPR", "TPR")
train_df$group <- "train"

test_df <- cbind(thrs_df, fpr_test_df, tpr_test_df)
colnames(test_df) <- c("thrs", "FPR", "TPR")
test_df$group <- "test"

model_df <- rbind(train_df, test_df)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### PRESERVE DATA AND HARDCODE TIME 
write.csv(model_df ,"lasso_3_253904_hrs.csv")


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#ELNET

rm(list = ls())    #delete objects
cat("\014")

install.packages('rmutil')
install.packages('tictoc')
install.packages('latex2exp')
install.packages('tree')

library(class)
library(ggplot2)
library(dplyr)
library(glmnet)
library(rmutil)
library(tictoc)
library(latex2exp)
library(tree)
library(e1071)

###########################
######## LOAD DATA ########
###########################

seizure = read.csv("data.csv",header=TRUE)
seizure = subset(seizure, select = -c(X) )
seizure$y = ifelse(seizure$y == 1, 'S', 'N')


###########################
######## ELNET ############
###########################

thrs.fill <- list()

FPR.train.fill <- list()
TPR.train.fill <- list()

FPR.test.fill <- list()
TPR.test.fill <- list()


start.time <- Sys.time()

for (j in 0:49) {
  
  #################
  ##### SAMPLE ####
  #################
  
  ## 90% of the sample size
  smp_size <- floor(0.9 * nrow(seizure))
  train_ind <- sample(seq_len(nrow(seizure)), size = smp_size)
  train <- seizure[train_ind, ]
  test <- seizure[-train_ind, ]
  
  
  ##################
  ### FORMATTING ###
  ##################
  
  X.train         =   model.matrix(y~., train)[, -1]
  y.train         =   train$y
  y.train         =   (y.train=='S')*1
  n.train         =   dim(X.train)[1] # sample size
  p.train         =   dim(X.train)[2] # number of predictors/features
  
  
  X.test         =   model.matrix(y~., test)[, -1]
  y.test         =   test$y
  y.test         =   (y.test=='S')*1
  n.test         =   dim(X.test)[1] # sample size
  p.test         =   dim(X.test)[2] # number of predictors/features
  
  #############
  ### MODEL ###
  #############
  
  elnet             =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 0.5,  intercept = TRUE,  nfolds = 10, type.measure="auc", standardize = FALSE)
  elnet             =     glmnet(X.train, y.train, lambda = elnet$lambda[which.max(elnet$cvm)], family = "binomial", alpha = 0.5,  intercept = TRUE, standardize = FALSE)
  
  for (i in 0:100) {
    if (i==0) {
      thrs = 0
    }
    else {
      thrs = i/100
    }
    
    p.hat.train             =     predict(elnet, newx = X.train,  type = "response")
    y.hat.train             =     rep("0",n.train)
    y.hat.train[p.hat.train>thrs]  =     "1"
    FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
    P.train          =    sum(y.train==1) # total positives in the data
    N.train          =    sum(y.train==0) # total negatives in the data
    FPR.train        =    FP.train/N.train
    TPR.train        =    TP.train/P.train
    
    
    p.hat.test             =     predict(elnet, newx = X.test,  type = "response")
    y.hat.test             =     rep("0",n.test)
    y.hat.test[p.hat.test>thrs]  =     "1"
    FP.test         =    sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.test         =    sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
    P.test          =    sum(y.test==1) # total positives in the data
    N.test          =    sum(y.test==0) # total negatives in the data
    FPR.test        =    FP.test/N.test
    TPR.test        =    TP.test/P.test
    
    thrs.fill <- append(thrs.fill, list(thrs))
    
    FPR.train.fill <- append(FPR.train.fill, list(FPR.train))
    TPR.train.fill <- append(TPR.train.fill, list(TPR.train))
    
    FPR.test.fill <- append(FPR.test.fill, list(FPR.test))
    TPR.test.fill <- append(TPR.test.fill, list(TPR.test))
    
  }
}

thrs_df <- as.data.frame(do.call(rbind, thrs.fill))

fpr_train_df <- as.data.frame(do.call(rbind, FPR.train.fill))
tpr_train_df <- as.data.frame(do.call(rbind, TPR.train.fill))

fpr_test_df <- as.data.frame(do.call(rbind, FPR.test.fill))
tpr_test_df <- as.data.frame(do.call(rbind, TPR.test.fill))

train_df <- cbind(thrs_df, fpr_train_df, tpr_train_df)
colnames(train_df) <- c("thrs", "FPR", "TPR")
train_df$group <- "train"

test_df <- cbind(thrs_df, fpr_test_df, tpr_test_df)
colnames(test_df) <- c("thrs", "FPR", "TPR")
test_df$group <- "test"

model_df <- rbind(train_df, test_df)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### PRESERVE DATA AND HARDCODE TIME 
write.csv(model_df ,"elnet_2_098848_hrs.csv")


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#RIDGE

rm(list = ls())    #delete objects
cat("\014")

install.packages('rmutil')
install.packages('tictoc')
install.packages('latex2exp')
install.packages('tree')

library(class)
library(ggplot2)
library(dplyr)
library(glmnet)
library(rmutil)
library(tictoc)
library(latex2exp)
library(tree)
library(e1071)

###########################
######## LOAD DATA ########
###########################

seizure = read.csv("data.csv",header=TRUE)
seizure = subset(seizure, select = -c(X) )
seizure$y = ifelse(seizure$y == 1, 'S', 'N')


###########################
######## RIDGE ############
###########################

thrs.fill <- list()

FPR.train.fill <- list()
TPR.train.fill <- list()

FPR.test.fill <- list()
TPR.test.fill <- list()


start.time <- Sys.time()

for (j in 0:49) {
  
  #################
  ##### SAMPLE ####
  #################
  
  ## 90% of the sample size
  smp_size <- floor(0.9 * nrow(seizure))
  train_ind <- sample(seq_len(nrow(seizure)), size = smp_size)
  train <- seizure[train_ind, ]
  test <- seizure[-train_ind, ]
  
  
  ##################
  ### FORMATTING ###
  ##################
  
  X.train         =   model.matrix(y~., train)[, -1]
  y.train         =   train$y
  y.train         =   (y.train=='S')*1
  n.train         =   dim(X.train)[1] # sample size
  p.train         =   dim(X.train)[2] # number of predictors/features
  
  
  X.test         =   model.matrix(y~., test)[, -1]
  y.test         =   test$y
  y.test         =   (y.test=='S')*1
  n.test         =   dim(X.test)[1] # sample size
  p.test         =   dim(X.test)[2] # number of predictors/features
  
  #############
  ### MODEL ###
  #############
  
  ridge             =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 0,  intercept = TRUE,   nfolds = 10, type.measure="auc",standardize = FALSE)
  ridge             =     glmnet(X.train, y.train, lambda = ridge$lambda[which.max(ridge$cvm)], family = "binomial", alpha = 0,  intercept = TRUE,standardize = FALSE)
  
  for (i in 0:100) {
    if (i==0) {
      thrs = 0
    }
    else {
      thrs = i/100
    }
    
    p.hat.train             =     predict(ridge, newx = X.train,  type = "response")
    y.hat.train             =     rep("0",n.train)
    y.hat.train[p.hat.train>thrs]  =     "1"
    FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
    P.train          =    sum(y.train==1) # total positives in the data
    N.train          =    sum(y.train==0) # total negatives in the data
    FPR.train        =    FP.train/N.train
    TPR.train        =    TP.train/P.train
    
    
    p.hat.test             =     predict(ridge, newx = X.test,  type = "response")
    y.hat.test             =     rep("0",n.test)
    y.hat.test[p.hat.test>thrs]  =     "1"
    FP.test         =    sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.test         =    sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
    P.test          =    sum(y.test==1) # total positives in the data
    N.test          =    sum(y.test==0) # total negatives in the data
    FPR.test        =    FP.test/N.test
    TPR.test        =    TP.test/P.test
    
    thrs.fill <- append(thrs.fill, list(thrs))
    
    FPR.train.fill <- append(FPR.train.fill, list(FPR.train))
    TPR.train.fill <- append(TPR.train.fill, list(TPR.train))
    
    FPR.test.fill <- append(FPR.test.fill, list(FPR.test))
    TPR.test.fill <- append(TPR.test.fill, list(TPR.test))
    
  }
}

thrs_df <- as.data.frame(do.call(rbind, thrs.fill))

fpr_train_df <- as.data.frame(do.call(rbind, FPR.train.fill))
tpr_train_df <- as.data.frame(do.call(rbind, TPR.train.fill))

fpr_test_df <- as.data.frame(do.call(rbind, FPR.test.fill))
tpr_test_df <- as.data.frame(do.call(rbind, TPR.test.fill))

train_df <- cbind(thrs_df, fpr_train_df, tpr_train_df)
colnames(train_df) <- c("thrs", "FPR", "TPR")
train_df$group <- "train"

test_df <- cbind(thrs_df, fpr_test_df, tpr_test_df)
colnames(test_df) <- c("thrs", "FPR", "TPR")
test_df$group <- "test"

model_df <- rbind(train_df, test_df)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

###### PRESERVE DATA AND HARDCODE TIME 
write.csv(model_df ,"ridge_2_24651_hrs.csv")


#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#RANDOMFOREST

rm(list = ls())    #delete objects
cat("\014")

install.packages('rmutil')
install.packages('tictoc')
install.packages('latex2exp')
install.packages('tree')
install.packages('randomForest')

library(randomForest)
library(class)
library(ggplot2)
library(dplyr)
library(glmnet)
library(rmutil)
library(tictoc)
library(latex2exp)
library(tree)
library(e1071)

###########################
######## LOAD DATA ########
###########################

seizure = read.csv("data.csv",header=TRUE)
seizure = subset(seizure, select = -c(X) )
seizure$y = ifelse(seizure$y == 1, 'S', 'N')


########################
######## RF ############
########################

thrs.fill <- list()

FPR.train.fill <- list()
TPR.train.fill <- list()

FPR.test.fill <- list()
TPR.test.fill <- list()


start.time <- Sys.time()

for (j in 0:49) {
  
  #################
  ##### SAMPLE ####
  #################
  
  ## 90% of the sample size
  smp_size <- floor(0.9 * nrow(seizure))
  train_ind <- sample(seq_len(nrow(seizure)), size = smp_size)
  train <- seizure[train_ind, ]
  test <- seizure[-train_ind, ]
  
  
  ##################
  ### FORMATTING ###
  ##################
  
  X.train         =   model.matrix(y~., train)[, -1]
  y.train         =   train$y
  y.train         =   (y.train=='S')*1
  n.train         =   dim(X.train)[1] # sample size
  p.train         =   dim(X.train)[2] # number of predictors/features
  
  
  X.test         =   model.matrix(y~., test)[, -1]
  y.test         =   test$y
  y.test         =   (y.test=='S')*1
  n.test         =   dim(X.test)[1] # sample size
  p.test         =   dim(X.test)[2] # number of predictors/features
  
  #############
  ### MODEL ###
  #############
  
  train.dat     =    data.frame(x=X.train, y = as.factor(y.train))
  test.dat      =    data.frame(x=X.test, y = as.factor(y.test))
  
  rf.fit     =    randomForest(y~., data = train.dat, mtry = sqrt(p.train))
  
  for (i in 0:100) {
    if (i==0) {
      thrs = 0
    }
    else {
      thrs = i/100
    }
    
    p.hat.train      =    predict(rf.fit, train.dat, type = "prob")
    p.hat.train      =    p.hat.train[,2]
    y.hat.train      =     rep("0",n.train)
    y.hat.train[p.hat.train>thrs]  =     "1"
    FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
    P.train          =    sum(y.train==1) # total positives in the data
    N.train          =    sum(y.train==0) # total negatives in the data
    FPR.train        =    FP.train/N.train
    TPR.train        =    TP.train/P.train
    
    
    p.hat.test     =    predict(rf.fit, test.dat, type = "prob")
    p.hat.test      =    p.hat.test[,2]
    y.hat.test      =     rep("0",n.test)
    y.hat.test[p.hat.test>thrs]  = "1"
    FP.test         =    sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
    TP.test         =    sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
    P.test          =    sum(y.test==1) # total positives in the data
    N.test          =    sum(y.test==0) # total negatives in the data
    FPR.test        =    FP.test/N.test
    TPR.test        =    TP.test/P.test
    
    thrs.fill <- append(thrs.fill, list(thrs))
    
    FPR.train.fill <- append(FPR.train.fill, list(FPR.train))
    TPR.train.fill <- append(TPR.train.fill, list(TPR.train))
    
    FPR.test.fill <- append(FPR.test.fill, list(FPR.test))
    TPR.test.fill <- append(TPR.test.fill, list(TPR.test))
    
  }
}

thrs_df <- as.data.frame(do.call(rbind, thrs.fill))

fpr_train_df <- as.data.frame(do.call(rbind, FPR.train.fill))
tpr_train_df <- as.data.frame(do.call(rbind, TPR.train.fill))

fpr_test_df <- as.data.frame(do.call(rbind, FPR.test.fill))
tpr_test_df <- as.data.frame(do.call(rbind, TPR.test.fill))

train_df <- cbind(thrs_df, fpr_train_df, tpr_train_df)
colnames(train_df) <- c("thrs", "FPR", "TPR")
train_df$group <- "train"

test_df <- cbind(thrs_df, fpr_test_df, tpr_test_df)
colnames(test_df) <- c("thrs", "FPR", "TPR")
test_df$group <- "test"

model_df <- rbind(train_df, test_df)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

model_df

###### PRESERVE DATA AND HARDCODE TIME 
write.csv(model_df ,"rf_5_875264_hrs.csv")

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#AUCPLOTS

library(ggplot2)
library(DescTools)

lasso_df <- read.csv('lasso_3_253904_hrs.csv')
ridge_df <- read.csv('ridge_2_24651_hrs.csv')
elnet_df <- read.csv('elnet_2_098848_hrs.csv')
rf_df <- read.csv('rf_5_875264_hrs.csv')

lasso_df$model <- NA
ridge_df$model <- NA
elnet_df$model <- NA
rf_df$model <- NA

rf_df_guess <- rf_df[5051:5151,]
auc_train = AUC(rf_df_guess$TPR, rf_df_guess$FPR)
auc_train = 1-auc_train
auc_train

lasso_df[c(1:101), 6] = 1
lasso_df[c(102:202), 6] = 2
lasso_df[c(203:303), 6] = 3
lasso_df[c(304:404), 6] = 4
lasso_df[c(405:505), 6] = 5
lasso_df[c(506:606), 6] = 6
lasso_df[c(607:707), 6] = 7
lasso_df[c(708:808), 6] = 8
lasso_df[c(809:909), 6] = 9
lasso_df[c(910:1010), 6] = 10
lasso_df[c(1011:1111), 6] = 11
lasso_df[c(1112:1212), 6] = 12
lasso_df[c(1213:1313), 6] = 13
lasso_df[c(1314:1414), 6] = 14
lasso_df[c(1415:1515), 6] = 15
lasso_df[c(1516:1616), 6] = 16
lasso_df[c(1617:1717), 6] = 17
lasso_df[c(1718:1818), 6] = 18
lasso_df[c(1819:1919), 6] = 19
lasso_df[c(1920:2020), 6] = 20
lasso_df[c(2021:2121), 6] = 21
lasso_df[c(2122:2222), 6] = 22
lasso_df[c(2223:2323), 6] = 23
lasso_df[c(2324:2424), 6] = 24
lasso_df[c(2425:2525), 6] = 25
lasso_df[c(2526:2626), 6] = 26
lasso_df[c(2627:2727), 6] = 27
lasso_df[c(2728:2828), 6] = 28
lasso_df[c(2829:2929), 6] = 29
lasso_df[c(2930:3030), 6] = 30
lasso_df[c(3031:3131), 6] = 31
lasso_df[c(3132:3232), 6] = 32
lasso_df[c(3233:3333), 6] = 33
lasso_df[c(3334:3434), 6] = 34
lasso_df[c(3435:3535), 6] = 35
lasso_df[c(3536:3636), 6] = 36
lasso_df[c(3637:3737), 6] = 37
lasso_df[c(3738:3838), 6] = 38
lasso_df[c(3839:3939), 6] = 39
lasso_df[c(3940:4040), 6] = 40
lasso_df[c(4041:4141), 6] = 41
lasso_df[c(4142:4242), 6] = 42
lasso_df[c(4243:4343), 6] = 43
lasso_df[c(4344:4444), 6] = 44
lasso_df[c(4445:4545), 6] = 45
lasso_df[c(4546:4646), 6] = 46
lasso_df[c(4647:4747), 6] = 47
lasso_df[c(4748:4848), 6] = 48
lasso_df[c(4849:4949), 6] = 49
lasso_df[c(4950:5050), 6] = 50
lasso_df[c(5051:5151), 6] = 1
lasso_df[c(5152:5252), 6] = 2
lasso_df[c(5253:5353), 6] = 3
lasso_df[c(5354:5454), 6] = 4
lasso_df[c(5455:5555), 6] = 5
lasso_df[c(5556:5656), 6] = 6
lasso_df[c(5657:5757), 6] = 7
lasso_df[c(5758:5858), 6] = 8
lasso_df[c(5859:5959), 6] = 9
lasso_df[c(5960:6060), 6] = 10
lasso_df[c(6061:6161), 6] = 11
lasso_df[c(6162:6262), 6] = 12
lasso_df[c(6263:6363), 6] = 13
lasso_df[c(6364:6464), 6] = 14
lasso_df[c(6465:6565), 6] = 15
lasso_df[c(6566:6666), 6] = 16
lasso_df[c(6667:6767), 6] = 17
lasso_df[c(6768:6868), 6] = 18
lasso_df[c(6869:6969), 6] = 19
lasso_df[c(6970:7070), 6] = 20
lasso_df[c(7071:7171), 6] = 21
lasso_df[c(7172:7272), 6] = 22
lasso_df[c(7273:7373), 6] = 23
lasso_df[c(7374:7474), 6] = 24
lasso_df[c(7475:7575), 6] = 25
lasso_df[c(7576:7676), 6] = 26
lasso_df[c(7677:7777), 6] = 27
lasso_df[c(7778:7878), 6] = 28
lasso_df[c(7879:7979), 6] = 29
lasso_df[c(7980:8080), 6] = 30
lasso_df[c(8081:8181), 6] = 31
lasso_df[c(8182:8282), 6] = 32
lasso_df[c(8283:8383), 6] = 33
lasso_df[c(8384:8484), 6] = 34
lasso_df[c(8485:8585), 6] = 35
lasso_df[c(8586:8686), 6] = 36
lasso_df[c(8687:8787), 6] = 37
lasso_df[c(8788:8888), 6] = 38
lasso_df[c(8889:8989), 6] = 39
lasso_df[c(8990:9090), 6] = 40
lasso_df[c(9091:9191), 6] = 41
lasso_df[c(9192:9292), 6] = 42
lasso_df[c(9293:9393), 6] = 43
lasso_df[c(9394:9494), 6] = 44
lasso_df[c(9495:9595), 6] = 45
lasso_df[c(9596:9696), 6] = 46
lasso_df[c(9697:9797), 6] = 47
lasso_df[c(9798:9898), 6] = 48
lasso_df[c(9899:9999), 6] = 49
lasso_df[c(10000:10100), 6] = 50

ridge_df[c(1:101), 6] = 1
ridge_df[c(102:202), 6] = 2
ridge_df[c(203:303), 6] = 3
ridge_df[c(304:404), 6] = 4
ridge_df[c(405:505), 6] = 5
ridge_df[c(506:606), 6] = 6
ridge_df[c(607:707), 6] = 7
ridge_df[c(708:808), 6] = 8
ridge_df[c(809:909), 6] = 9
ridge_df[c(910:1010), 6] = 10
ridge_df[c(1011:1111), 6] = 11
ridge_df[c(1112:1212), 6] = 12
ridge_df[c(1213:1313), 6] = 13
ridge_df[c(1314:1414), 6] = 14
ridge_df[c(1415:1515), 6] = 15
ridge_df[c(1516:1616), 6] = 16
ridge_df[c(1617:1717), 6] = 17
ridge_df[c(1718:1818), 6] = 18
ridge_df[c(1819:1919), 6] = 19
ridge_df[c(1920:2020), 6] = 20
ridge_df[c(2021:2121), 6] = 21
ridge_df[c(2122:2222), 6] = 22
ridge_df[c(2223:2323), 6] = 23
ridge_df[c(2324:2424), 6] = 24
ridge_df[c(2425:2525), 6] = 25
ridge_df[c(2526:2626), 6] = 26
ridge_df[c(2627:2727), 6] = 27
ridge_df[c(2728:2828), 6] = 28
ridge_df[c(2829:2929), 6] = 29
ridge_df[c(2930:3030), 6] = 30
ridge_df[c(3031:3131), 6] = 31
ridge_df[c(3132:3232), 6] = 32
ridge_df[c(3233:3333), 6] = 33
ridge_df[c(3334:3434), 6] = 34
ridge_df[c(3435:3535), 6] = 35
ridge_df[c(3536:3636), 6] = 36
ridge_df[c(3637:3737), 6] = 37
ridge_df[c(3738:3838), 6] = 38
ridge_df[c(3839:3939), 6] = 39
ridge_df[c(3940:4040), 6] = 40
ridge_df[c(4041:4141), 6] = 41
ridge_df[c(4142:4242), 6] = 42
ridge_df[c(4243:4343), 6] = 43
ridge_df[c(4344:4444), 6] = 44
ridge_df[c(4445:4545), 6] = 45
ridge_df[c(4546:4646), 6] = 46
ridge_df[c(4647:4747), 6] = 47
ridge_df[c(4748:4848), 6] = 48
ridge_df[c(4849:4949), 6] = 49
ridge_df[c(4950:5050), 6] = 50
ridge_df[c(5051:5151), 6] = 1
ridge_df[c(5152:5252), 6] = 2
ridge_df[c(5253:5353), 6] = 3
ridge_df[c(5354:5454), 6] = 4
ridge_df[c(5455:5555), 6] = 5
ridge_df[c(5556:5656), 6] = 6
ridge_df[c(5657:5757), 6] = 7
ridge_df[c(5758:5858), 6] = 8
ridge_df[c(5859:5959), 6] = 9
ridge_df[c(5960:6060), 6] = 10
ridge_df[c(6061:6161), 6] = 11
ridge_df[c(6162:6262), 6] = 12
ridge_df[c(6263:6363), 6] = 13
ridge_df[c(6364:6464), 6] = 14
ridge_df[c(6465:6565), 6] = 15
ridge_df[c(6566:6666), 6] = 16
ridge_df[c(6667:6767), 6] = 17
ridge_df[c(6768:6868), 6] = 18
ridge_df[c(6869:6969), 6] = 19
ridge_df[c(6970:7070), 6] = 20
ridge_df[c(7071:7171), 6] = 21
ridge_df[c(7172:7272), 6] = 22
ridge_df[c(7273:7373), 6] = 23
ridge_df[c(7374:7474), 6] = 24
ridge_df[c(7475:7575), 6] = 25
ridge_df[c(7576:7676), 6] = 26
ridge_df[c(7677:7777), 6] = 27
ridge_df[c(7778:7878), 6] = 28
ridge_df[c(7879:7979), 6] = 29
ridge_df[c(7980:8080), 6] = 30
ridge_df[c(8081:8181), 6] = 31
ridge_df[c(8182:8282), 6] = 32
ridge_df[c(8283:8383), 6] = 33
ridge_df[c(8384:8484), 6] = 34
ridge_df[c(8485:8585), 6] = 35
ridge_df[c(8586:8686), 6] = 36
ridge_df[c(8687:8787), 6] = 37
ridge_df[c(8788:8888), 6] = 38
ridge_df[c(8889:8989), 6] = 39
ridge_df[c(8990:9090), 6] = 40
ridge_df[c(9091:9191), 6] = 41
ridge_df[c(9192:9292), 6] = 42
ridge_df[c(9293:9393), 6] = 43
ridge_df[c(9394:9494), 6] = 44
ridge_df[c(9495:9595), 6] = 45
ridge_df[c(9596:9696), 6] = 46
ridge_df[c(9697:9797), 6] = 47
ridge_df[c(9798:9898), 6] = 48
ridge_df[c(9899:9999), 6] = 49
ridge_df[c(10000:10100), 6] = 50

elnet_df[c(1:101), 6] = 1
elnet_df[c(102:202), 6] = 2
elnet_df[c(203:303), 6] = 3
elnet_df[c(304:404), 6] = 4
elnet_df[c(405:505), 6] = 5
elnet_df[c(506:606), 6] = 6
elnet_df[c(607:707), 6] = 7
elnet_df[c(708:808), 6] = 8
elnet_df[c(809:909), 6] = 9
elnet_df[c(910:1010), 6] = 10
elnet_df[c(1011:1111), 6] = 11
elnet_df[c(1112:1212), 6] = 12
elnet_df[c(1213:1313), 6] = 13
elnet_df[c(1314:1414), 6] = 14
elnet_df[c(1415:1515), 6] = 15
elnet_df[c(1516:1616), 6] = 16
elnet_df[c(1617:1717), 6] = 17
elnet_df[c(1718:1818), 6] = 18
elnet_df[c(1819:1919), 6] = 19
elnet_df[c(1920:2020), 6] = 20
elnet_df[c(2021:2121), 6] = 21
elnet_df[c(2122:2222), 6] = 22
elnet_df[c(2223:2323), 6] = 23
elnet_df[c(2324:2424), 6] = 24
elnet_df[c(2425:2525), 6] = 25
elnet_df[c(2526:2626), 6] = 26
elnet_df[c(2627:2727), 6] = 27
elnet_df[c(2728:2828), 6] = 28
elnet_df[c(2829:2929), 6] = 29
elnet_df[c(2930:3030), 6] = 30
elnet_df[c(3031:3131), 6] = 31
elnet_df[c(3132:3232), 6] = 32
elnet_df[c(3233:3333), 6] = 33
elnet_df[c(3334:3434), 6] = 34
elnet_df[c(3435:3535), 6] = 35
elnet_df[c(3536:3636), 6] = 36
elnet_df[c(3637:3737), 6] = 37
elnet_df[c(3738:3838), 6] = 38
elnet_df[c(3839:3939), 6] = 39
elnet_df[c(3940:4040), 6] = 40
elnet_df[c(4041:4141), 6] = 41
elnet_df[c(4142:4242), 6] = 42
elnet_df[c(4243:4343), 6] = 43
elnet_df[c(4344:4444), 6] = 44
elnet_df[c(4445:4545), 6] = 45
elnet_df[c(4546:4646), 6] = 46
elnet_df[c(4647:4747), 6] = 47
elnet_df[c(4748:4848), 6] = 48
elnet_df[c(4849:4949), 6] = 49
elnet_df[c(4950:5050), 6] = 50
elnet_df[c(5051:5151), 6] = 1
elnet_df[c(5152:5252), 6] = 2
elnet_df[c(5253:5353), 6] = 3
elnet_df[c(5354:5454), 6] = 4
elnet_df[c(5455:5555), 6] = 5
elnet_df[c(5556:5656), 6] = 6
elnet_df[c(5657:5757), 6] = 7
elnet_df[c(5758:5858), 6] = 8
elnet_df[c(5859:5959), 6] = 9
elnet_df[c(5960:6060), 6] = 10
elnet_df[c(6061:6161), 6] = 11
elnet_df[c(6162:6262), 6] = 12
elnet_df[c(6263:6363), 6] = 13
elnet_df[c(6364:6464), 6] = 14
elnet_df[c(6465:6565), 6] = 15
elnet_df[c(6566:6666), 6] = 16
elnet_df[c(6667:6767), 6] = 17
elnet_df[c(6768:6868), 6] = 18
elnet_df[c(6869:6969), 6] = 19
elnet_df[c(6970:7070), 6] = 20
elnet_df[c(7071:7171), 6] = 21
elnet_df[c(7172:7272), 6] = 22
elnet_df[c(7273:7373), 6] = 23
elnet_df[c(7374:7474), 6] = 24
elnet_df[c(7475:7575), 6] = 25
elnet_df[c(7576:7676), 6] = 26
elnet_df[c(7677:7777), 6] = 27
elnet_df[c(7778:7878), 6] = 28
elnet_df[c(7879:7979), 6] = 29
elnet_df[c(7980:8080), 6] = 30
elnet_df[c(8081:8181), 6] = 31
elnet_df[c(8182:8282), 6] = 32
elnet_df[c(8283:8383), 6] = 33
elnet_df[c(8384:8484), 6] = 34
elnet_df[c(8485:8585), 6] = 35
elnet_df[c(8586:8686), 6] = 36
elnet_df[c(8687:8787), 6] = 37
elnet_df[c(8788:8888), 6] = 38
elnet_df[c(8889:8989), 6] = 39
elnet_df[c(8990:9090), 6] = 40
elnet_df[c(9091:9191), 6] = 41
elnet_df[c(9192:9292), 6] = 42
elnet_df[c(9293:9393), 6] = 43
elnet_df[c(9394:9494), 6] = 44
elnet_df[c(9495:9595), 6] = 45
elnet_df[c(9596:9696), 6] = 46
elnet_df[c(9697:9797), 6] = 47
elnet_df[c(9798:9898), 6] = 48
elnet_df[c(9899:9999), 6] = 49
elnet_df[c(10000:10100), 6] = 50

rf_df[c(1:101), 6] = 1
rf_df[c(102:202), 6] = 2
rf_df[c(203:303), 6] = 3
rf_df[c(304:404), 6] = 4
rf_df[c(405:505), 6] = 5
rf_df[c(506:606), 6] = 6
rf_df[c(607:707), 6] = 7
rf_df[c(708:808), 6] = 8
rf_df[c(809:909), 6] = 9
rf_df[c(910:1010), 6] = 10
rf_df[c(1011:1111), 6] = 11
rf_df[c(1112:1212), 6] = 12
rf_df[c(1213:1313), 6] = 13
rf_df[c(1314:1414), 6] = 14
rf_df[c(1415:1515), 6] = 15
rf_df[c(1516:1616), 6] = 16
rf_df[c(1617:1717), 6] = 17
rf_df[c(1718:1818), 6] = 18
rf_df[c(1819:1919), 6] = 19
rf_df[c(1920:2020), 6] = 20
rf_df[c(2021:2121), 6] = 21
rf_df[c(2122:2222), 6] = 22
rf_df[c(2223:2323), 6] = 23
rf_df[c(2324:2424), 6] = 24
rf_df[c(2425:2525), 6] = 25
rf_df[c(2526:2626), 6] = 26
rf_df[c(2627:2727), 6] = 27
rf_df[c(2728:2828), 6] = 28
rf_df[c(2829:2929), 6] = 29
rf_df[c(2930:3030), 6] = 30
rf_df[c(3031:3131), 6] = 31
rf_df[c(3132:3232), 6] = 32
rf_df[c(3233:3333), 6] = 33
rf_df[c(3334:3434), 6] = 34
rf_df[c(3435:3535), 6] = 35
rf_df[c(3536:3636), 6] = 36
rf_df[c(3637:3737), 6] = 37
rf_df[c(3738:3838), 6] = 38
rf_df[c(3839:3939), 6] = 39
rf_df[c(3940:4040), 6] = 40
rf_df[c(4041:4141), 6] = 41
rf_df[c(4142:4242), 6] = 42
rf_df[c(4243:4343), 6] = 43
rf_df[c(4344:4444), 6] = 44
rf_df[c(4445:4545), 6] = 45
rf_df[c(4546:4646), 6] = 46
rf_df[c(4647:4747), 6] = 47
rf_df[c(4748:4848), 6] = 48
rf_df[c(4849:4949), 6] = 49
rf_df[c(4950:5050), 6] = 50
rf_df[c(5051:5151), 6] = 1
rf_df[c(5152:5252), 6] = 2
rf_df[c(5253:5353), 6] = 3
rf_df[c(5354:5454), 6] = 4
rf_df[c(5455:5555), 6] = 5
rf_df[c(5556:5656), 6] = 6
rf_df[c(5657:5757), 6] = 7
rf_df[c(5758:5858), 6] = 8
rf_df[c(5859:5959), 6] = 9
rf_df[c(5960:6060), 6] = 10
rf_df[c(6061:6161), 6] = 11
rf_df[c(6162:6262), 6] = 12
rf_df[c(6263:6363), 6] = 13
rf_df[c(6364:6464), 6] = 14
rf_df[c(6465:6565), 6] = 15
rf_df[c(6566:6666), 6] = 16
rf_df[c(6667:6767), 6] = 17
rf_df[c(6768:6868), 6] = 18
rf_df[c(6869:6969), 6] = 19
rf_df[c(6970:7070), 6] = 20
rf_df[c(7071:7171), 6] = 21
rf_df[c(7172:7272), 6] = 22
rf_df[c(7273:7373), 6] = 23
rf_df[c(7374:7474), 6] = 24
rf_df[c(7475:7575), 6] = 25
rf_df[c(7576:7676), 6] = 26
rf_df[c(7677:7777), 6] = 27
rf_df[c(7778:7878), 6] = 28
rf_df[c(7879:7979), 6] = 29
rf_df[c(7980:8080), 6] = 30
rf_df[c(8081:8181), 6] = 31
rf_df[c(8182:8282), 6] = 32
rf_df[c(8283:8383), 6] = 33
rf_df[c(8384:8484), 6] = 34
rf_df[c(8485:8585), 6] = 35
rf_df[c(8586:8686), 6] = 36
rf_df[c(8687:8787), 6] = 37
rf_df[c(8788:8888), 6] = 38
rf_df[c(8889:8989), 6] = 39
rf_df[c(8990:9090), 6] = 40
rf_df[c(9091:9191), 6] = 41
rf_df[c(9192:9292), 6] = 42
rf_df[c(9293:9393), 6] = 43
rf_df[c(9394:9494), 6] = 44
rf_df[c(9495:9595), 6] = 45
rf_df[c(9596:9696), 6] = 46
rf_df[c(9697:9797), 6] = 47
rf_df[c(9798:9898), 6] = 48
rf_df[c(9899:9999), 6] = 49
rf_df[c(10000:10100), 6] = 50


#######################
######## LASSO ########
#######################

lasso_train <- subset(lasso_df , group == "train")
lasso_test <- subset(lasso_df , group == "test")
nrow(lasso_train)
nrow(lasso_test)

lasso_train_auc <- list()
lasso_test_auc <- list()

for (mod in unique(lasso_train$model)) { 
  model_df = subset(lasso_train, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  lasso_train_auc = append(lasso_train_auc, list(auc))
}

for (mod in unique(lasso_test$model)) { 
  model_df = subset(lasso_test, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  lasso_test_auc = append(lasso_test_auc, list(auc))
}

lasso_train_df <- as.data.frame(do.call(rbind, lasso_train_auc))
lasso_test_df <- as.data.frame(do.call(rbind, lasso_test_auc))
lasso <- rbind(lasso_train_df,lasso_test_df)
colnames(lasso) <- c("auc")
lasso$group <- NA
data.frame(colnames(lasso))
lasso[c(1:50), 2] = "train"
lasso[c(51:100), 2] = "test"

lasso$fit <- "lasso"

#######################
######## RIDGE ########
#######################

ridge_train <- subset(ridge_df , group == "train")
ridge_test <- subset(ridge_df , group == "test")
nrow(ridge_train)
nrow(ridge_test)

ridge_train_auc <- list()
ridge_test_auc <- list()

for (mod in unique(ridge_train$model)) { 
  model_df = subset(ridge_train, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  ridge_train_auc = append(ridge_train_auc, list(auc))
}

for (mod in unique(ridge_test$model)) { 
  model_df = subset(ridge_test, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  ridge_test_auc = append(ridge_test_auc, list(auc))
}

ridge_train_df <- as.data.frame(do.call(rbind, ridge_train_auc))
ridge_test_df <- as.data.frame(do.call(rbind, ridge_test_auc))
ridge <- rbind(ridge_train_df,ridge_test_df)
colnames(ridge) <- c("auc")
ridge$group <- NA
data.frame(colnames(ridge))
ridge[c(1:50), 2] = "train"
ridge[c(51:100), 2] = "test"

ridge$fit <- "ridge"


#######################
######## ELNET ########
#######################

elnet_train <- subset(elnet_df , group == "train")
elnet_test <- subset(elnet_df , group == "test")
nrow(elnet_train)
nrow(elnet_test)

elnet_train_auc <- list()
elnet_test_auc <- list()

for (mod in unique(elnet_train$model)) { 
  model_df = subset(elnet_train, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  elnet_train_auc = append(elnet_train_auc, list(auc))
}

for (mod in unique(elnet_test$model)) { 
  model_df = subset(elnet_test, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  elnet_test_auc = append(elnet_test_auc, list(auc))
}

elnet_train_df <- as.data.frame(do.call(rbind, elnet_train_auc))
elnet_test_df <- as.data.frame(do.call(rbind, elnet_test_auc))
elnet <- rbind(elnet_train_df,elnet_test_df)
colnames(elnet) <- c("auc")
elnet$group <- NA
data.frame(colnames(elnet))
elnet[c(1:50), 2] = "train"
elnet[c(51:100), 2] = "test"

elnet$fit <- "elnet"

head(elnet,3)


####################
######## RF ########
####################

rf_train <- subset(rf_df , group == "train")
rf_test <- subset(rf_df , group == "test")
nrow(rf_train)
nrow(rf_test)

rf_train_auc <- list()
rf_test_auc <- list()

for (mod in unique(rf_train$model)) { 
  model_df = subset(rf_train, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  rf_train_auc = append(rf_train_auc, list(auc))
}

for (mod in unique(rf_test$model)) { 
  model_df = subset(rf_test, model==mod)
  auc = AUC(model_df$TPR, model_df$FPR)
  auc = 1-auc
  rf_test_auc = append(rf_test_auc, list(auc))
}

rf_train_df <- as.data.frame(do.call(rbind, rf_train_auc))
rf_test_df <- as.data.frame(do.call(rbind, rf_test_auc))
rf <- rbind(rf_train_df,rf_test_df)
colnames(rf) <- c("auc")
rf$group <- NA
data.frame(colnames(rf))
rf[c(1:50), 2] = "train"
rf[c(51:100), 2] = "test"

rf$fit <- "rf"

head(rf,3)

binded_fits <- rbind(lasso,ridge,elnet,rf)
nrow(binded_fits)

train_binded_fits <- subset(binded_fits, group=="train")
nrow(train_binded_fits)

test_binded_fits <- subset(binded_fits, group=="test")
nrow(test_binded_fits)

head(binded_fits,3)

ggplot(train_binded_fits, aes(x=as.factor(fit), y=auc)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("fit") +
  ylab("auc") +
  ggtitle("TRAIN") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(test_binded_fits, aes(x=as.factor(fit), y=auc)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("fit") +
  ylab("auc") +
  ggtitle("TEST") +
  theme(plot.title = element_text(hjust = 0.5))


min(sapply(lasso_test_auc, min))
max(sapply(lasso_test_auc, max))

min(sapply(ridge_test_auc, min))
max(sapply(ridge_test_auc, max))


min(sapply(elnet_test_auc, min))
max(sapply(elnet_test_auc, max))

min(sapply(rf_test_auc, min))
max(sapply(rf_test_auc, max))

quantile(unlist(lasso_test_auc), c(.05, .95)) 
quantile(unlist(ridge_test_auc), c(.05, .95)) 
quantile(unlist(elnet_test_auc), c(.05, .95)) 
quantile(unlist(rf_test_auc), c(.05, .95)) 

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#CVCURVES

m(list = ls())    #delete objects
cat("\014")

install.packages('rmutil')
install.packages('tictoc')
install.packages('latex2exp')
install.packages('tree')

library(class)
library(ggplot2)
library(dplyr)
library(glmnet)
library(rmutil)
library(tictoc)
library(latex2exp)
library(tree)
library(e1071)

###########################
######## LOAD DATA ########
###########################

seizure = read.csv("data.csv",header=TRUE)
seizure = subset(seizure, select = -c(X) )
seizure$y = ifelse(seizure$y == 1, 'S', 'N')

#################
##### SAMPLE ####
#################

## 90% of the sample size
smp_size <- floor(0.9 * nrow(seizure))
train_ind <- sample(seq_len(nrow(seizure)), size = smp_size)
train <- seizure[train_ind, ]
test <- seizure[-train_ind, ]


##################
### FORMATTING ###
##################

X.train         =   model.matrix(y~., train)[, -1]
y.train         =   train$y
y.train         =   (y.train=='S')*1
n.train         =   dim(X.train)[1] # sample size
p.train         =   dim(X.train)[2] # number of predictors/features


X.test         =   model.matrix(y~., test)[, -1]
y.test         =   test$y
y.test         =   (y.test=='S')*1
n.test         =   dim(X.test)[1] # sample size
p.test         =   dim(X.test)[2] # number of predictors/features

###########################
######## LASSO ############
###########################

lasso.start.time <- Sys.time()

lasso.thrs.fill <- list()

lasso.FPR.train.fill <- list()
lasso.TPR.train.fill <- list()

lasso.FPR.test.fill <- list()
lasso.TPR.test.fill <- list()


lasso_cv          =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 1,  intercept = TRUE,  nfolds = 10, type.measure="auc",standardize = FALSE)
lasso             =     glmnet(X.train, y.train, lambda = lasso_cv$lambda.min, family = "binomial", alpha = 1,  intercept = TRUE,standardize = FALSE)

for (i in 0:100) {
  if (i==0) {
    thrs = 0
  }
  else {
    thrs = i/100
  }
  
  p.hat.train             =     predict(lasso, newx = X.train,  type = "response")
  y.hat.train             =     rep("0",n.train)
  y.hat.train[p.hat.train>thrs]  =     "1"
  FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
  P.train          =    sum(y.train==1) # total positives in the data
  N.train          =    sum(y.train==0) # total negatives in the data
  FPR.train        =    FP.train/N.train
  TPR.train        =    TP.train/P.train
  
  
  p.hat.test             =     predict(lasso, newx = X.test,  type = "response")
  y.hat.test             =     rep("0",n.test)
  y.hat.test[p.hat.test>thrs]  =     "1"
  FP.test         =    sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.test         =    sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
  P.test          =    sum(y.test==1) # total positives in the data
  N.test          =    sum(y.test==0) # total negatives in the data
  FPR.test        =    FP.test/N.test
  TPR.test        =    TP.test/P.test
  
  lasso.thrs.fill <- append(lasso.thrs.fill, list(thrs))
  
  lasso.FPR.train.fill <- append(lasso.FPR.train.fill, list(FPR.train))
  lasso.TPR.train.fill <- append(lasso.TPR.train.fill, list(TPR.train))
  
  lasso.FPR.test.fill <- append(lasso.FPR.test.fill, list(FPR.test))
  lasso.TPR.test.fill <- append(lasso.TPR.test.fill, list(TPR.test))
  
}

lasso.thrs_df <- as.data.frame(do.call(rbind, lasso.thrs.fill))

lasso.fpr_train_df <- as.data.frame(do.call(rbind, lasso.FPR.train.fill))
lasso.tpr_train_df <- as.data.frame(do.call(rbind, lasso.TPR.train.fill))

lasso.fpr_test_df <- as.data.frame(do.call(rbind, lasso.FPR.test.fill))
lasso.tpr_test_df <- as.data.frame(do.call(rbind, lasso.TPR.test.fill))

lasso.train_df <- cbind(lasso.thrs_df, lasso.fpr_train_df, lasso.tpr_train_df)
colnames(lasso.train_df) <- c("thrs", "FPR", "TPR")
lasso.train_df$group <- "train"

lasso.test_df <- cbind(lasso.thrs_df, lasso.fpr_test_df, lasso.tpr_test_df)
colnames(lasso.test_df) <- c("thrs", "FPR", "TPR")
lasso.test_df$group <- "test"

lasso_model_df <- rbind(lasso.train_df, lasso.test_df)

lasso.end.time <- Sys.time()
lasso.time.taken <- lasso.end.time - lasso.start.time
lasso.time.taken


###########################
######## RIDGE ############
###########################

ridge.start.time <- Sys.time()

ridge.thrs.fill <- list()

ridge.FPR.train.fill <- list()
ridge.TPR.train.fill <- list()

ridge.FPR.test.fill <- list()
ridge.TPR.test.fill <- list()


ridge_cv          =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 0,  intercept = TRUE,   nfolds = 10, type.measure="auc",standardize = FALSE)
ridge             =     glmnet(X.train, y.train, lambda = ridge_cv$lambda[which.max(ridge_cv$cvm)], family = "binomial", alpha = 0,  intercept = TRUE,standardize = FALSE)

for (i in 0:100) {
  if (i==0) {
    thrs = 0
  }
  else {
    thrs = i/100
  }
  
  p.hat.train             =     predict(ridge, newx = X.train,  type = "response")
  y.hat.train             =     rep("0",n.train)
  y.hat.train[p.hat.train>thrs]  =     "1"
  FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
  P.train          =    sum(y.train==1) # total positives in the data
  N.train          =    sum(y.train==0) # total negatives in the data
  FPR.train        =    FP.train/N.train
  TPR.train        =    TP.train/P.train
  
  
  p.hat.test             =     predict(ridge, newx = X.test,  type = "response")
  y.hat.test             =     rep("0",n.test)
  y.hat.test[p.hat.test>thrs]  =     "1"
  FP.test         =    sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.test         =    sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
  P.test          =    sum(y.test==1) # total positives in the data
  N.test          =    sum(y.test==0) # total negatives in the data
  FPR.test        =    FP.test/N.test
  TPR.test        =    TP.test/P.test
  
  ridge.thrs.fill <- append(ridge.thrs.fill, list(thrs))
  
  ridge.FPR.train.fill <- append(ridge.FPR.train.fill, list(FPR.train))
  ridge.TPR.train.fill <- append(ridge.TPR.train.fill, list(TPR.train))
  
  ridge.FPR.test.fill <- append(ridge.FPR.test.fill, list(FPR.test))
  ridge.TPR.test.fill <- append(ridge.TPR.test.fill, list(TPR.test))
  
}

ridge.thrs_df <- as.data.frame(do.call(rbind, ridge.thrs.fill))

ridge.fpr_train_df <- as.data.frame(do.call(rbind, ridge.FPR.train.fill))
ridge.tpr_train_df <- as.data.frame(do.call(rbind, ridge.TPR.train.fill))

ridge.fpr_test_df <- as.data.frame(do.call(rbind, ridge.FPR.test.fill))
ridge.tpr_test_df <- as.data.frame(do.call(rbind, ridge.TPR.test.fill))

ridge.train_df <- cbind(ridge.thrs_df, ridge.fpr_train_df, ridge.tpr_train_df)
colnames(ridge.train_df) <- c("thrs", "FPR", "TPR")
ridge.train_df$group <- "train"

ridge.test_df <- cbind(ridge.thrs_df, ridge.fpr_test_df, ridge.tpr_test_df)
colnames(ridge.test_df) <- c("thrs", "FPR", "TPR")
ridge.test_df$group <- "test"

ridge_model_df <- rbind(ridge.train_df, ridge.test_df)

ridge.end.time <- Sys.time()
ridge.time.taken <- ridge.end.time - ridge.start.time
ridge.time.taken


###########################
######## ELNET ############
###########################

elnet.start.time <- Sys.time()

elnet.thrs.fill <- list()

elnet.FPR.train.fill <- list()
elnet.TPR.train.fill <- list()

elnet.FPR.test.fill <- list()
elnet.TPR.test.fill <- list()


elnet_cv          =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 0.5,  intercept = TRUE,  nfolds = 10, type.measure="auc", standardize = FALSE)
elnet             =     glmnet(X.train, y.train, lambda = elnet_cv$lambda[which.max(elnet_cv$cvm)], family = "binomial", alpha = 0.5,  intercept = TRUE, standardize = FALSE)

for (i in 0:100) {
  if (i==0) {
    thrs = 0
  }
  else {
    thrs = i/100
  }
  
  p.hat.train             =     predict(elnet, newx = X.train,  type = "response")
  y.hat.train             =     rep("0",n.train)
  y.hat.train[p.hat.train>thrs]  =     "1"
  FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
  P.train          =    sum(y.train==1) # total positives in the data
  N.train          =    sum(y.train==0) # total negatives in the data
  FPR.train        =    FP.train/N.train
  TPR.train        =    TP.train/P.train
  
  
  p.hat.test             =     predict(elnet, newx = X.test,  type = "response")
  y.hat.test             =     rep("0",n.test)
  y.hat.test[p.hat.test>thrs]  =     "1"
  FP.test         =    sum(y.test[y.hat.test==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.test         =    sum(y.hat.test[y.test==1] == 1) # true positives = positives in the data that were predicted as positive
  P.test          =    sum(y.test==1) # total positives in the data
  N.test          =    sum(y.test==0) # total negatives in the data
  FPR.test        =    FP.test/N.test
  TPR.test        =    TP.test/P.test
  
  elnet.thrs.fill <- append(elnet.thrs.fill, list(thrs))
  
  elnet.FPR.train.fill <- append(elnet.FPR.train.fill, list(FPR.train))
  elnet.TPR.train.fill <- append(elnet.TPR.train.fill, list(TPR.train))
  
  elnet.FPR.test.fill <- append(elnet.FPR.test.fill, list(FPR.test))
  elnet.TPR.test.fill <- append(elnet.TPR.test.fill, list(TPR.test))
  
}

elnet.thrs_df <- as.data.frame(do.call(rbind, elnet.thrs.fill))

elnet.fpr_train_df <- as.data.frame(do.call(rbind, elnet.FPR.train.fill))
elnet.tpr_train_df <- as.data.frame(do.call(rbind, elnet.TPR.train.fill))

elnet.fpr_test_df <- as.data.frame(do.call(rbind, elnet.FPR.test.fill))
elnet.tpr_test_df <- as.data.frame(do.call(rbind, elnet.TPR.test.fill))

elnet.train_df <- cbind(elnet.thrs_df, elnet.fpr_train_df, elnet.tpr_train_df)
colnames(elnet.train_df) <- c("thrs", "FPR", "TPR")
elnet.train_df$group <- "train"

elnet.test_df <- cbind(elnet.thrs_df, elnet.fpr_test_df, elnet.tpr_test_df)
colnames(elnet.test_df) <- c("thrs", "FPR", "TPR")
elnet.test_df$group <- "test"

elnet_model_df <- rbind(elnet.train_df, elnet.test_df)

elnet.end.time <- Sys.time()
elnet.time.taken <- elnet.end.time - elnet.start.time
elnet.time.taken

plot(lasso_cv)
plot(ridge_cv)
plot(elnet_cv)

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################

#SINGLE RIDGE/LASSO/ELNET LOGISTIC REGRESSION

m(list = ls())    #delete objects
cat("\014")

install.packages('rmutil')
install.packages('tictoc')
install.packages('latex2exp')
install.packages('tree')

library(class)
library(ggplot2)
library(dplyr)
library(glmnet)
library(rmutil)
library(tictoc)
library(latex2exp)
library(tree)
library(e1071)

###########################
######## LOAD DATA ########
###########################

seizure = read.csv("data.csv",header=TRUE)
seizure = subset(seizure, select = -c(X) )
seizure$y = ifelse(seizure$y == 1, 'S', 'N')


##################
### FORMATTING ###
##################

X.train         =   model.matrix(y~., seizure)[, -1]
y.train         =   seizure$y
y.train         =   (y.train=='S')*1
n.train         =   dim(X.train)[1] # sample size
p.train         =   dim(X.train)[2] # number of predictors/features

###########################
######## LASSO ############
###########################

lasso.start.time <- Sys.time()

lasso.thrs.fill <- list()

lasso.FPR.train.fill <- list()
lasso.TPR.train.fill <- list()


lasso_cv          =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 1,  intercept = TRUE,  nfolds = 10, type.measure="auc",standardize = FALSE)
lasso             =     glmnet(X.train, y.train, lambda = lasso_cv$lambda.min, family = "binomial", alpha = 1,  intercept = TRUE,standardize = FALSE)

for (i in 0:100) {
  if (i==0) {
    thrs = 0
  }
  else {
    thrs = i/100
  }
  
  p.hat.train             =     predict(lasso, newx = X.train,  type = "response")
  y.hat.train             =     rep("0",n.train)
  y.hat.train[p.hat.train>thrs]  =     "1"
  FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
  P.train          =    sum(y.train==1) # total positives in the data
  N.train          =    sum(y.train==0) # total negatives in the data
  FPR.train        =    FP.train/N.train
  TPR.train        =    TP.train/P.train
  
  lasso.thrs.fill <- append(lasso.thrs.fill, list(thrs))
  
  lasso.FPR.train.fill <- append(lasso.FPR.train.fill, list(FPR.train))
  lasso.TPR.train.fill <- append(lasso.TPR.train.fill, list(TPR.train))
  
}

lasso.thrs_df <- as.data.frame(do.call(rbind, lasso.thrs.fill))

lasso.fpr_train_df <- as.data.frame(do.call(rbind, lasso.FPR.train.fill))
lasso.tpr_train_df <- as.data.frame(do.call(rbind, lasso.TPR.train.fill))

lasso.train_df <- cbind(lasso.thrs_df, lasso.fpr_train_df, lasso.tpr_train_df)
colnames(lasso.train_df) <- c("thrs", "FPR", "TPR")
lasso.train_df$group <- "train"

lasso.end.time <- Sys.time()
lasso.time.taken <- lasso.end.time - lasso.start.time
lasso.time.taken


###########################
######## RIDGE ############
###########################

ridge.start.time <- Sys.time()

ridge.thrs.fill <- list()

ridge.FPR.train.fill <- list()
ridge.TPR.train.fill <- list()


ridge_cv          =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 0,  intercept = TRUE,   nfolds = 10, type.measure="auc",standardize = FALSE)
ridge             =     glmnet(X.train, y.train, lambda = ridge_cv$lambda[which.max(ridge_cv$cvm)], family = "binomial", alpha = 0,  intercept = TRUE,standardize = FALSE)

for (i in 0:100) {
  if (i==0) {
    thrs = 0
  }
  else {
    thrs = i/100
  }
  
  p.hat.train             =     predict(ridge, newx = X.train,  type = "response")
  y.hat.train             =     rep("0",n.train)
  y.hat.train[p.hat.train>thrs]  =     "1"
  FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
  P.train          =    sum(y.train==1) # total positives in the data
  N.train          =    sum(y.train==0) # total negatives in the data
  FPR.train        =    FP.train/N.train
  TPR.train        =    TP.train/P.train
  
  
  ridge.thrs.fill <- append(ridge.thrs.fill, list(thrs))
  
  ridge.FPR.train.fill <- append(ridge.FPR.train.fill, list(FPR.train))
  ridge.TPR.train.fill <- append(ridge.TPR.train.fill, list(TPR.train))
  
}

ridge.thrs_df <- as.data.frame(do.call(rbind, ridge.thrs.fill))

ridge.fpr_train_df <- as.data.frame(do.call(rbind, ridge.FPR.train.fill))
ridge.tpr_train_df <- as.data.frame(do.call(rbind, ridge.TPR.train.fill))

ridge.train_df <- cbind(ridge.thrs_df, ridge.fpr_train_df, ridge.tpr_train_df)
colnames(ridge.train_df) <- c("thrs", "FPR", "TPR")
ridge.train_df$group <- "train"

ridge.end.time <- Sys.time()
ridge.time.taken <- ridge.end.time - ridge.start.time
ridge.time.taken


###########################
######## ELNET ############
###########################

elnet.start.time <- Sys.time()

elnet.thrs.fill <- list()

elnet.FPR.train.fill <- list()
elnet.TPR.train.fill <- list()


elnet_cv          =     cv.glmnet(X.train, y.train, family = "binomial", alpha = 0.5,  intercept = TRUE,  nfolds = 10, type.measure="auc", standardize = FALSE)
elnet             =     glmnet(X.train, y.train, lambda = elnet_cv$lambda[which.max(elnet_cv$cvm)], family = "binomial", alpha = 0.5,  intercept = TRUE, standardize = FALSE)

for (i in 0:100) {
  if (i==0) {
    thrs = 0
  }
  else {
    thrs = i/100
  }
  
  p.hat.train             =     predict(elnet, newx = X.train,  type = "response")
  y.hat.train             =     rep("0",n.train)
  y.hat.train[p.hat.train>thrs]  =     "1"
  FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
  P.train          =    sum(y.train==1) # total positives in the data
  N.train          =    sum(y.train==0) # total negatives in the data
  FPR.train        =    FP.train/N.train
  TPR.train        =    TP.train/P.train
  
  
  elnet.thrs.fill <- append(elnet.thrs.fill, list(thrs))
  
  elnet.FPR.train.fill <- append(elnet.FPR.train.fill, list(FPR.train))
  elnet.TPR.train.fill <- append(elnet.TPR.train.fill, list(TPR.train))
  
}

elnet.thrs_df <- as.data.frame(do.call(rbind, elnet.thrs.fill))

elnet.fpr_train_df <- as.data.frame(do.call(rbind, elnet.FPR.train.fill))
elnet.tpr_train_df <- as.data.frame(do.call(rbind, elnet.TPR.train.fill))

elnet.train_df <- cbind(elnet.thrs_df, elnet.fpr_train_df, elnet.tpr_train_df)
colnames(elnet.train_df) <- c("thrs", "FPR", "TPR")
elnet.train_df$group <- "train"

elnet_model_df <- rbind(elnet.train_df, elnet.test_df)

elnet.end.time <- Sys.time()
elnet.time.taken <- elnet.end.time - elnet.start.time
elnet.time.taken

###########################
########### RF ############
###########################

rf.start.time <- Sys.time()

rf.thrs.fill <- list()

rf.FPR.train.fill <- list()
rf.TPR.train.fill <- list()

train.dat     =    data.frame(x=X.train, y = as.factor(y.train))

rf.fit     =    randomForest(y~., data = train.dat, mtry = sqrt(p.train))


for (i in 0:100) {
  if (i==0) {
    thrs = 0
  }
  else {
    thrs = i/100
  }
  
  p.hat.train      =    predict(rf.fit, train.dat, type = "prob")
  p.hat.train      =    p.hat.train[,2]
  y.hat.train      =     rep("0",n.train)
  y.hat.train[p.hat.train>thrs]  =     "1"
  FP.train         =    sum(y.train[y.hat.train==1] == 0) # false positives = negatives in the data that were predicted as positive
  TP.train         =    sum(y.hat.train[y.train==1] == 1) # true positives = positives in the data that were predicted as positive
  P.train          =    sum(y.train==1) # total positives in the data
  N.train          =    sum(y.train==0) # total negatives in the data
  FPR.train        =    FP.train/N.train
  TPR.train        =    TP.train/P.train
  
  
  rf.thrs.fill <- append(rf.thrs.fill, list(thrs))
  
  rf.FPR.train.fill <- append(rf.FPR.train.fill, list(FPR.train))
  rf.TPR.train.fill <- append(rf.TPR.train.fill, list(TPR.train))
  
}

rf.thrs_df <- as.data.frame(do.call(rbind, rf.thrs.fill))

rf.fpr_train_df <- as.data.frame(do.call(rbind, rf.FPR.train.fill))
rf.tpr_train_df <- as.data.frame(do.call(rbind, rf.TPR.train.fill))

rf.train_df <- cbind(rf.thrs_df, rf.fpr_train_df, rf.tpr_train_df)
colnames(rf.train_df) <- c("thrs", "FPR", "TPR")
rf.train_df$group <- "train"

rf.end.time <- Sys.time()
rf.time.taken <- rf.end.time - rf.start.time
rf.time.taken


############################
########## ANSWERS ########
############################

library(DescTools)
auc_train_lasso = AUC(lasso.train_df$TPR, lasso.train_df$FPR)
auc_train_lasso = 1-auc_train_lasso
auc_train_ridge = AUC(ridge.train_df$TPR, ridge.train_df$FPR)
auc_train_ridge = 1-auc_train_ridge
auc_train_elnet = AUC(elnet.train_df$TPR, elnet.train_df$FPR)
auc_train_elnet = 1-auc_train_elnet
auc_train_rf = AUC(rf.train_df$TPR, rf.train_df$FPR)
auc_train_rf = 1-auc_train_rf

lasso.time.taken
ridge.time.taken
elnet.time.taken
rf.time.taken

auc_train_lasso
auc_train_ridge
auc_train_elnet
auc_train_rf

### FOR 90 PER AUC FOR 50SAMPLES GO BACK TO AUC BOXPLOT SECTION AT BOTTOM

#############################################################################################################################
#############################################################################################################################
#############################################################################################################################


#####################################
########## Beta Coefficients ########
#####################################

all_betas <- read.csv("C:\\Users\\its_t\\Documents\\CUNY\\Fall 2020\\9891 - Machine Learning\\Project\\Project\\all betas.csv")



nsim_betas_elast$Index  <- factor(nsim_betas_elast$Index, levels = nsim_betas_elast$Index[order(nsim_betas_elast$Beta._values_enet, decreasing = TRUE)])
nsim_betas_forest$Index <- factor(nsim_betas_forest$Index, levels = nsim_betas_elast$Index[order(nsim_betas_elast$Beta._values_enet, decreasing = TRUE)])
nsim_betas_lasso$Index  <- factor(nsim_betas_lasso$Index, levels = nsim_betas_elast$Index[order(nsim_betas_elast$Beta._values_enet, decreasing = TRUE)])
nsim_betas_ridge$Index  <- factor(nsim_betas_ridge$Index, levels = nsim_betas_elast$Index[order(nsim_betas_elast$Beta._values_enet, decreasing = TRUE)])


## Run the batch for which you want to plot the graph
first_batch <-  all_betas[1:61,]
nsim_betas_elast <- first_batch %>% select(1,2)
nsim_betas_forest <- first_batch %>% select(1,5)
nsim_betas_lasso <- first_batch %>% select(1,4)
nsim_betas_ridge <- first_batch %>% select(1,3)


second_batch <-  all_betas[62:122,]
nsim_betas_elast <- second_batch %>% select(1,2)
nsim_betas_forest <- second_batch %>% select(1,5)
nsim_betas_lasso <- second_batch %>% select(1,4)
nsim_betas_ridge <- second_batch %>% select(1,3)


third_batch <-  all_betas[123:178,]
nsim_betas_elast <- third_batch %>% select(1,2)
nsim_betas_forest <- third_batch %>% select(1,5)
nsim_betas_lasso <- third_batch %>% select(1,4)
nsim_betas_ridge <- third_batch %>% select(1,3)

# Plot the beta coefficients in order of elastic net
enet_beta_Plot =  ggplot() + 
  aes(x= nsim_betas_elast$Index, y= nsim_betas_elast$Beta._values_enet)+
  geom_bar(stat = "identity", fill="white", colour="black")+
  ggtitle('Elastic Net') + ylab('Beta Value')+
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
enet_beta_Plot

ridge_beta_Plot  =  ggplot() + aes(x= nsim_betas_ridge$Index, y= nsim_betas_ridge$Beta._values_ridge )+
  geom_bar(stat = "identity", fill="white", colour="black") +
  ggtitle('Ridge')+ ylab('Beta Value')+ 
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
ridge_beta_Plot

lasso_beta_Plot  =  ggplot() + aes(x= nsim_betas_lasso$Index, y= nsim_betas_lasso$Beta._values_lasso )+
  geom_bar(stat = "identity", fill="white", colour="black")    +
  ggtitle('Lasso') + ylab('Beta Value')+
  theme(axis.title.x=element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
lasso_beta_Plot


rf_beta_Plot  =  ggplot() + aes(x= nsim_betas_forest$Index, y= nsim_betas_forest$Beta._values_random.forest )+
  geom_bar(stat = "identity", fill="white", colour="black")    +
  ggtitle('Random Forest')+  ylab('Beta Value')+ xlab("Features") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
rf_beta_Plot


## save these plots in a grid
grid.arrange(enet_beta_Plot, lasso_beta_Plot, ridge_beta_Plot,rf_beta_Plot,nrow = 4)
