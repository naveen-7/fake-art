---
  title: "fake_art"
author: 'Naveen Sendhilnathan'
date: "12/08/2018"
output:
  pdf_document: default
word_document: default
---
  
  setwd('/Users/Naveen/Dropbox (Personal)/PROJECT')

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

NOTE: In this project I have tried several models including random forest, bagging, boosting, svm, logarthimic regression, naive basyes, ridge regression and neural network. Each model had its own pros and cons. Finally, bagging nad random forest gave me the best estimate. Therefore, I have commented out several models that didnt quite give good results.


##1
### 1a
```{r}
data = read.csv("train_dataset.csv", header = TRUE)
data = data[-which(names(data) %in% c("RT13Id"))]
N = nrow(data)
TEST = read.csv("test_dataset.csv", header = TRUE)
TEST = TEST[-which(names(TEST) %in% c("RT13Id"))]

TEST = data.frame(1,TEST)
colnames(TEST)[1] <- "IsBadBuy"
TEST$IsBadBuy =NA

data = rbind(data,TEST)
M = nrow(data)



ind = 1:N
set.seed(1)
ind = sample.int(N)
#trn_data = data[ind[1:round(0.6*N)],]
#cv_data = data[ind[(round(0.6*N)+1):round(0.8*N)],]
#tst_data = data[ind[(round(0.8*N)+1):N],]

trn_ind = ind[1:round(0.6*N)]
cv_ind = ind[(round(0.6*N)+1):round(0.8*N)]
tst_ind = ind[(round(0.8*N)+1):N]
TST_ind = (N+1):M



```


### 1b
```{r}
AvgPrice <- as.numeric(as.character(data[trn_ind,]$MMRCurrentRetailAveragePrice))
ClnPrice <- as.numeric(as.character(data[trn_ind,]$MMRCurrentRetailCleanPrice))
plot(AvgPrice,ClnPrice,'p')
```
### 1c
```{r}
par(mfrow=c(2,2))
hist(as.numeric(as.character(data[trn_ind,]$PaintingAge)),xlab='Age',main='')
hist(as.numeric(as.character(data[trn_ind,]$Bids)),xlab='Bids',main='')
hist(as.numeric(as.character(data[trn_ind,]$WarrantyCost)),xlab='Warranty Cost',main='')
hist(as.numeric(as.character(data[trn_ind,]$PaintingBCost)),xlab='Cost',main='')
```
Bids, Waranty cost and painitng cost are skewed. 

##2
```{r}
#install.packages('CRAN')
library("pracma")

## 2a ------- dropping the ID
#data = data[-which(names(data) %in% c("RT13Id"))]


Tcategories = c("T1","T2","T3","T4","T5","T6","T7","T8","T9","T10","T11","T12","T13")
allsubtypes = c("genre","history","still life","real life","landscape","portrait","fine art","other")

Names <- as.character(data$PaintingName)
SubType <- as.character(data$SubType)

TCategories_dummy = matrix(0,nrow(data),13)
Num_Checks_dummy = matrix(0,nrow(data),2)
Size1_dummy = matrix(0,nrow(data),1)
Subtype_dummy = matrix(0,nrow(data),1)
SubtypeD_dummy = matrix(0,nrow(data),2)
ALLNAMES = matrix(0,nrow(data),1)

for (i in 1:M){
  temp = (strsplit(Names[i]," "))
  
  ALLNAMES[i] = temp[[1]][1]
  
  ## 2b ------- extracting categories 
  for (j in 1:length(temp[[1]])){
    flag = matrix(0,1,13)
    for (k in 1:13){
      if (strcmp(temp[[1]][j],Tcategories[k])=='TRUE') flag[k]=1
    }
    TCategories_dummy[i,which(flag==1)] = 1
    
    ## 2d ------- extracting painting size
    if (grepl('L',temp[[1]][j])){
      tempy = gsub('.{1}$', '', temp[[1]][j])
      if (grepl("\\d", tempy)=='TRUE'){
        Size1_dummy[i,1]=tempy
      }
    }
  }
  
  ## 2c ------- extracting quality checks 
  if (grepl('I4',Names[i]) | grepl('I-4',Names[i]) | grepl('I 4',Names[i])) Num_Checks_dummy[i,1]=1
  if (grepl('I6',Names[i]) | grepl('I-6',Names[i]) | grepl('I 6',Names[i])) Num_Checks_dummy[i,2]=1
  
  
  ## 2e ------- extracting subtype information
  for (j in 1:length(allsubtypes)){
    if (grepl(allsubtypes[j],tolower(SubType[i]))=='TRUE'){
      Subtype_dummy[i] = allsubtypes[j]  
    }
  }
  
  if (grepl('2D',SubType[i])) SubtypeD_dummy[i,1]=1
  if (grepl('4D',SubType[i])) SubtypeD_dummy[i,2]=1
  
}


TCategories_dummy = data.frame(TCategories_dummy)
colnames(TCategories_dummy) <- Tcategories

Num_Checks_dummy = data.frame(Num_Checks_dummy)
colnames(Num_Checks_dummy) <- c("I4","I6")

SubtypeD_dummy = data.frame(SubtypeD_dummy)
colnames(SubtypeD_dummy) <- c("2D","4D")



## 2f ------- ratio for prices and bids
Bids = as.numeric(as.character(data$Bids))

AA_avg = as.numeric(as.character(data$MMRAcquisitionAuctionAveragePrice))
AA_cln = as.numeric(as.character(data$MMRAcquisitionAuctionCleanPrice))
AR_avg = as.numeric(as.character(data$MMRAcquisitionRetailAveragePrice))
AR_cln = as.numeric(as.character(data$MMRAcquisitonRetailCleanPrice))

CA_avg = as.numeric(as.character(data$MMRCurrentAuctionAveragePrice))
CA_cln = as.numeric(as.character(data$MMRCurrentAuctionCleanPrice))
CR_avg = as.numeric(as.character(data$MMRCurrentRetailAveragePrice))
CR_cln = as.numeric(as.character(data$MMRCurrentRetailCleanPrice))

Ratios = matrix(0,nrow(data[trn_ind,]),8)
Ratios[,1] = Bids[trn_ind]/AA_avg[trn_ind]
Ratios[,2] = Bids[trn_ind]/AA_cln[trn_ind]
Ratios[,3] = Bids[trn_ind]/AR_avg[trn_ind]
Ratios[,4] = Bids[trn_ind]/AR_cln[trn_ind]
Ratios[,5] = Bids[trn_ind]/CA_avg[trn_ind]
Ratios[,6] = Bids[trn_ind]/CA_cln[trn_ind]
Ratios[,7] = Bids[trn_ind]/CR_avg[trn_ind]
Ratios[,8] = Bids[trn_ind]/CR_cln[trn_ind]


## 2g ------- dummy factors
CreateDummy = function(Field){
  UNIQUES = unique(Field)
  dummy = matrix(0,length(Field),length(UNIQUES))
  for (ii in 1:length(UNIQUES)){
    dummy[which(Field==UNIQUES[ii]),ii]=1
  }
  dummy = data.frame(dummy)
  colnames(dummy) <- UNIQUES
  return(dummy)
}

NAMES_dummy = CreateDummy(ALLNAMES)
Size1_dummy = CreateDummy(Size1_dummy)
Artist_dummy = CreateDummy(data$Artist)
Year_dummy = CreateDummy(data$PaintingYear)
CColor_dummy = CreateDummy(data$CanvasColor)
Auction_dummy = CreateDummy(data$Auction)
Market_dummy = CreateDummy(data$Market)
FrameType_dummy = CreateDummy(data$FrameType)
Size2_dummy = CreateDummy(data$Size)
PRIME_dummy = CreateDummy(data$PRIMEUNIT)
Age_dummy = CreateDummy(data$PaintingAge)
Trim_dummy = CreateDummy(data$Trim)
TopThree_dummy = CreateDummy(data$TopThreeNYCName)
GUART_dummy = CreateDummy(data$AUCGUART)
BYRNO_dummy = CreateDummy(data$BYRNO)
VNZIP_dummy = CreateDummy(data$VNZIP1)
VNST_dummy = CreateDummy(data$VNST)
Subtype_dummy = CreateDummy(Subtype_dummy)

```


##3
```{r}
# Seperate day, month and year
DATE = as.character.Date(data$PurchDate)
DATES = matrix(0,length(DATE),3)

temp = (strsplit(DATE,"/"))
for (i in 1:length(DATE)){
  DATES[i,1] = temp[[i]][1]
  DATES[i,2] = temp[[i]][2]
  DATES[i,3] = temp[[i]][3]
}
DATES = data.frame(DATES)
colnames(DATES) <- c("Month","Day","Year")

PurchDD_dummy = CreateDummy(DATES$Day)
PurchMM_dummy = CreateDummy(DATES$Month)

UNIQUES = c(2009,2010)
dummy = matrix(0,length(DATES$Year),length(UNIQUES))
for (ii in 1:length(UNIQUES)){
  dummy[which(DATES$Year==UNIQUES[ii]),ii]=1
}
PurchYY_dummy = data.frame(dummy)
colnames(PurchYY_dummy) <- UNIQUES
```


### Selecting good predictors 
## PART 1: continuous variables
```{r}
# checking which of the continuous variables are well correlated and removing them
Bids = as.numeric(as.character(data$Bids))
PBcost = as.numeric(as.character(data$PaintingBCost))
Wcost = as.numeric(as.character(data$WarrantyCost))

pairs(data.frame(AA_avg[trn_ind],AA_cln[trn_ind],AR_avg[trn_ind],AR_cln[trn_ind]))
pairs(data.frame(CA_avg[trn_ind],CA_cln[trn_ind],CR_avg[trn_ind],CR_cln[trn_ind]))
pairs(data.frame(Bids[trn_ind],PBcost[trn_ind],Wcost[trn_ind]))

library('infotheo')
# checking which of the continuous variables chosen above has highest MI 
sprintf('MI for AA_avg = %s', mutinformation(AA_avg[1:N],data[1:N,]$IsBadBuy))
sprintf('MI for CA_avg = %s', mutinformation(CA_avg[1:N],data[1:N,]$IsBadBuy))
sprintf('MI for Bids = %s', mutinformation(Bids[1:N],data[1:N,]$IsBadBuy))
sprintf('MI for Wcost = %s', mutinformation(Wcost[1:N],data[1:N,]$IsBadBuy))

# based on the above polts and calculations, for further analyses,
# I'll only use AA_avg, CA_avg, Bids, Painting cost and Waranty cost as continuous predictors


```

## PART 2: categorical variables
```{r}
#calculating mutual entropies

CATAGORIES = data.frame(data$Auction, data$PaintingYear, data$PaintingAge, data$Artist, data$Trim, data$CanvasColor, data$Market, data$FrameTypeID, data$FrameType, data$Nationality, data$Size, data$BYRNO, data$VNZIP1, data$VNST, data$IsOnlineSale, data$PRIMEUNIT, data$Trim, data$AUCGUART, data$TopThreeNYCName)

cal_labels = c("Auction","PaintingYear","PaintingAge","Artist","Trim","CanvasColor","Market","FrameTypeID","FrameType","Nationality","Size","BYRNO","VNZIP1","VNST","IsOnlineSale","PRIMEUNIT","Trim","AUCGUART","TopThreeNYCName")
colnames(CATAGORIES) = cal_labels 

library("infotheo")
MI = matrix(1,ncol(CATAGORIES))
for (i in 1:ncol(CATAGORIES)){
  MI[i] = mutinformation(data[1:N,]$IsBadBuy,CATAGORIES[1:N,i])
}

temp = sort(MI, index.return=TRUE, decreasing = FALSE)
sprintf("Remove %s from further analyses",cal_labels[temp$ix[1:3]])

#######################################
# concatinating all the dummies and using the ones with most entropy

ALL_DUMMIES = data.frame(Auction_dummy,Year_dummy,Age_dummy,Artist_dummy,Trim_dummy,TCategories_dummy,Num_Checks_dummy,Size1_dummy,Size2_dummy,Subtype_dummy,FrameType_dummy,TopThree_dummy,GUART_dummy,BYRNO_dummy,VNZIP_dummy,VNST_dummy,CColor_dummy,PurchDD_dummy,PurchMM_dummy,PurchYY_dummy, SubtypeD_dummy, NAMES_dummy)

library('infotheo')
entropies = matrix(0,1,ncol(ALL_DUMMIES))
MI = matrix(0,1,ncol(ALL_DUMMIES))
for (i in 1:ncol(ALL_DUMMIES)){
  entropies[i] = condentropy(ALL_DUMMIES[,i])
  MI[i] = mutinformation(ALL_DUMMIES[1:N,i],data[1:N,]$IsBadBuy)
}

#entropyind = sort(entropies,index.return=TRUE, decreasing = TRUE)
MIind = sort(MI,index.return=TRUE, decreasing = TRUE)
#ALL_DUMMIES = ALL_DUMMIES[,entropyind$ix]

ALL_DUMMIES = ALL_DUMMIES[,MIind$ix]
entropies = entropies[MIind$ix]
MI = MI[MIind$ix]

IMP_IND = which(entropies>0.01)
SEL_DUMMIES = ALL_DUMMIES[,IMP_IND]

ALLVARAIBLES = data.frame(AA_avg, CA_avg, Bids, Wcost, PBcost, SEL_DUMMIES)


```

### DEFINING TRAINING AND TEST DATA
```{r}
BADBUY = matrix(0,length(data[1:N,]$IsBadBuy),1)
BADBUY[which(data[1:N,]$IsBadBuy==0)]='FALSE'
BADBUY[which(data[1:N,]$IsBadBuy==1)]='TRUE'
BADBUY = data.frame(BADBUY)

badbuy = data[1:N,]$IsBadBuy

usebadbuy = badbuy

TRN_DATA = data.frame(usebadbuy[trn_ind],ALLVARAIBLES[trn_ind,])
TST_DATA = data.frame(usebadbuy[tst_ind],ALLVARAIBLES[tst_ind,])
CV_DATA = data.frame(usebadbuy[cv_ind],ALLVARAIBLES[cv_ind,])
TEST_SET = data.frame(ALLVARAIBLES[TST_ind,])
colnames(TRN_DATA)[1] <- "BADBUY"
colnames(TST_DATA) <- colnames(TRN_DATA, do.NULL = TRUE, prefix = "col")
colnames(CV_DATA) <- colnames(TRN_DATA, do.NULL = TRUE, prefix = "col")
colnames(TEST_SET) <- colnames(TRN_DATA[2:ncol(TRN_DATA)], do.NULL = TRUE, prefix = "col")

TRN_DATA = TRN_DATA[complete.cases(TRN_DATA), ]
TST_DATA = TST_DATA[complete.cases(TST_DATA), ]
CV_DATA  = CV_DATA[complete.cases(CV_DATA), ]


rm(list= ls()[!(ls() %in% c('TRN_DATA','TST_DATA','TEST_SET','BADBUY','CV_DATA','badbuy','trn_ind','tst_ind','TST_ind','cv_ind'))])

```


```{r}
calculate_acc = function(predicted, actual){
  
  ACC = matrix(0,1,100)
  TH = matrix(0,1,100)
  for (kk in 0:100){
    THRESHOLD = kk/100
    TH[kk] = THRESHOLD
    Predicted <- matrix(NA,1,length(predicted))
    Predicted[which(predicted>THRESHOLD)]=1
    Predicted[which(predicted<=THRESHOLD)]=0
    Predicted <- factor(Predicted)
    actual <- factor(actual)
    library('caret')
    CMat_trn = confusionMatrix(Predicted,actual)
    ACC[kk] = CMat_trn$overall[1]
  }
  THRESHOLD = TH[which.max(ACC)]
  Predicted <- matrix(NA,1,length(predicted))
  Predicted[which(predicted>THRESHOLD)]=1
  Predicted[which(predicted<=THRESHOLD)]=0
  Predicted <- factor(Predicted)
  actual <- factor(actual)
  library('caret')
  CMat_trn = confusionMatrix(Predicted,actual)
  
  return(c(THRESHOLD,CMat_trn$byClass[1],CMat_trn$byClass[2],CMat_trn$overall[1]))
}
```

### PERFORMING SVM -------------------
```{r}
N = 50
# ONE -- MODEL
library('e1071')
result_svm <- svm(BADBUY ~. , data=TRN_DATA[,1:N])
print(result_svm)

# TWO -- PREDICT
predicted_svm = (predict(result_svm, newdata = TST_DATA, type = "response"))
actual_svm = (TST_DATA$BADBUY)

# THREE -- ROC
library('pROC')
ROC_N = roc(actual_svm,predicted_svm)
plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))

# FOUR -- ACCURACY
vals =calculate_acc(predicted_svm,actual_svm)
THRESHOLD_svm = vals[1]
sprintf('sensitivity = %f', vals[2])
sprintf('specificity = %f', vals[3])
sprintf('Accuracy = %f', vals[4])


```

### PERFORMING RANDOM FOREST / BAGGING -------------------
```{r}
library("randomForest")
N = 50 #ncol(TRN_DATA)
# ONE -- MODEL
result_RF <- randomForest(BADBUY ~. , data=TRN_DATA[,1:N], importance=TRUE, na.action = na.exclude)
print(result_RF)
result_RF$confusion

# TWO -- PREDICT
predicted_RF = (predict(result_RF, newdata = TST_DATA, type = "response"))
actual_RF = (TST_DATA$BADBUY)

# THREE -- ROC
library('pROC')
ROC_N = roc(actual_RF,predicted_RF)
plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))


# FOUR -- ACCURACY
vals =calculate_acc(predicted_RF,actual_RF)
THRESHOLD_RF = vals[1]
sprintf('sensitivity = %f', vals[2])
sprintf('specificity = %f', vals[3])
sprintf('Accuracy = %f', vals[4])

```


### PERFORMING LOGISTIC REGRESSION -------------------
```{r}
# ONE -- MODEL
result_log <- glm(BADBUY ~. , data=TRN_DATA ,family=binomial)
summary(result_log) 

# TWO -- PREDICT
predicted_log = predict(result_log, newdata = TST_DATA, type = "response")
actual_log = (TST_DATA$BADBUY)

# THREE -- ROC
ROC_N = roc(actual_log,predicted_log)
plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))

# FOUR -- ACCURACY
vals = calculate_acc(predicted_log, actual_log)
THRESHOLD_log = vals[1]
sprintf('sensitivity = %f', vals[2])
sprintf('specificity = %f', vals[3])
sprintf('Accuracy = %f', vals[4])


```

### PERFORMING RIDGE REGRESSION -------------------
```{r}
# # ONE -- MODEL
# #install.packages('ridge')
# library('ridge')
# N=5
# result_ridge <- logisticRidge(BADBUY ~. , data=TRN_DATA[1:N])
# summary(result_ridge) 
# 
# 
# # TWO -- PREDICT
# predicted_ridge = predict(result_ridge, newdata = TST_DATA, type = "response")
# actual_ridge = (TST_DATA$BADBUY)
# 
# # THREE -- ROC
# ROC_N = roc(actual_ridge,predicted_ridge)
# plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))
# 
# # FOUR -- ACCURACY
# vals = calculate_acc(predicted_ridge, actual_ridge)
# THRESHOLD_ridge = vals[1]
# sprintf('sensitivity = %f', vals[2])
# sprintf('specificity = %f', vals[3])
# sprintf('Accuracy = %f', vals[4])

```



### PERFORMING ADABOOSTING -------------------
```{r}
# N = 200
# # ONE -- MODEL
# #install.packages('JOUSBoost')
# library('JOUSBoost')
# Y = TRN_DATA[,1]
# Y[which(TRN_DATA[,1]==0)]=-1
# X = as.matrix(TRN_DATA[,2:N])
# result_boost <- adaboost(X,Y, tree_depth = 10, n_rounds = 300, verbose = TRUE)
# print(result_boost)
# 
# # TWO -- PREDICT
# X_test = as.matrix(TST_DATA[,2:N])
# Y_test = TST_DATA[,1]
# Y_test[which(TST_DATA[,1]==0)]=-1
# predicted_boost = predict(result_boost, X_test)
# actual_boost = Y_test
# 
# # THREE -- ROC
# library('pROC')
# ROC_N = roc(actual_boost,predicted_boost)
# plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))
# 
# # FOUR -- ACCURACY
# library('caret')
# CMat_trn = confusionMatrix(factor(predicted_boost),factor(actual_boost))
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1]) 
```

### PERFORMING GRADIENT BOOSTING -------------------
```{r}
# N = 20
# # ONE -- MODEL
# #install.packages('gbm')
# library('gbm')
# 
# result_gboost <- gbm(BADBUY ~ . ,data = TRN_DATA[,1:N],distribution = "gaussian",n.trees = 10000,
#                   shrinkage = 0.01, interaction.depth = 4)
# print(result_gboost)
# 
# # TWO -- PREDICT
# n.trees = seq(from=100 ,to=10000, by=100) #no of trees-a vector of 100 values 
# 
# predicted_gboost = predict(result_gboost, TST_DATA, n.trees)
# actual_gboost = (TST_DATA$BADBUY)
# 
# test.error<-with(TST_DATA,apply( (predicted_gboost-BADBUY)^2,2,mean))
# acc = 1-test.error
# predicted_gboost_fin = predicted_gboost[,which.max(acc)]
# 
# 
# 
# # THREE -- ROC
# ROC_N = roc(actual_gboost,predicted_gboost_fin)
# plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))
# 
# # FOUR -- ACCURACY
# ACC = matrix(0,1,100)
# TH = matrix(0,1,100)
# actual <- factor(actual_gboost)
# for (kk in 0:100){
#   THRESHOLD = kk/100
#   TH[kk] = THRESHOLD
#   Predicted <- matrix(NA,1,length(predicted_gboost_fin))
#   Predicted[which(predicted_gboost_fin>THRESHOLD)]=1
#   Predicted[which(predicted_gboost_fin<=THRESHOLD)]=0
#   Predicted <- factor(Predicted)
#   library('caret')
#   CMat_trn = confusionMatrix(Predicted,actual)
#   ACC[kk] = CMat_trn$overall[1]
# }
# THRESHOLD_gboost = TH[which.max(ACC)]
# Predicted <- matrix(NA,1,length(predicted_gboost_fin))
# Predicted[which(predicted_gboost_fin>THRESHOLD)]=1
# Predicted[which(predicted_gboost_fin<=THRESHOLD)]=0
# Predicted <- factor(Predicted)
# actual <- factor(actual_gboost)
# library('caret')
# CMat_trn = confusionMatrix(Predicted,actual)
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1]) 

```

### PERFORMING NEURAL NETWORK -------------------
```{r}
# #install.packages("neuralnet")
# library("neuralnet")
# 
# TRN_DATA_NN = apply(TRN_DATA,2,as.numeric)
# TST_DATA_NN = apply(TST_DATA,2,as.numeric)
# 
# TRN_DATA_NN = TRN_DATA_NN[complete.cases(TRN_DATA_NN), ]
# TRN_DATA_NN = TRN_DATA_NN[complete.cases(TRN_DATA_NN), ]
# 
# N = 10 #ncol(TRN_DATA)
# # ONE -- MODEL
# nam = colnames(TRN_DATA_NN[,2:N])
# f <- paste("BADBUY ~", paste(nam, collapse=" + "))
# result_NN <- neuralnet(formula(f) , data=TRN_DATA_NN[,1:N], hidden = 10, act.fct='logistic', threshold = 0.01, stepmax = 200000, lifesign = 'full', lifesign.step=1000)
# 
# # TWO -- PREDICT
# predicted_NN = neuralnet::compute(result_NN, TST_DATA_NN[,2:N])
# predicted_NN = predicted_NN$net.result
# actual_NN = (TST_DATA$BADBUY)
# 
# # THREE -- ROC
# library('pROC')
# ROC_N = roc(actual_NN,predicted_NN)
# plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))
# 
# # FOUR -- ACCURACY
# ACC_NN = matrix(0,1,100)
# TH_NN = matrix(0,1,100)
# for (kk in 0:100){
#   THRESHOLD_NN = kk/100
#   TH_NN[kk] = THRESHOLD_NN
#   Predicted_NN <- matrix(NA,1,length(predicted_NN))
#   Predicted_NN[which(predicted_NN>THRESHOLD_NN)]=1
#   Predicted_NN[which(predicted_NN<=THRESHOLD_NN)]=0
#   Predicted_NN <- factor(Predicted_NN)
#   actual_NN <- factor(actual_NN)
#   library('caret')
#   CMat_trn = confusionMatrix(Predicted_NN,actual_NN)
#   ACC_NN[kk] = CMat_trn$overall[1]
# }
# THRESHOLD_NN = TH_NN[which.max(ACC_NN)]
# Predicted_NN <- matrix(NA,1,length(predicted_NN))
# Predicted_NN[which(predicted_NN>THRESHOLD_NN)]=1
# Predicted_NN[which(predicted_NN<=THRESHOLD_NN)]=0
# Predicted_NN <- factor(Predicted_NN)
# actual_NN <- factor(actual_NN)
# library('caret')
# CMat_trn = confusionMatrix(Predicted_NN,actual_NN)
# 
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1])
# 
# 
# 
#  # FIVE -- PREDICT
# predicted_NNCV = (predict(result_NN, newdata = CV_DATA, type = "response"))
# actual_NNCV = (CV_DATA$BADBUY)
# ACC_NNCV = matrix(0,1,100)
# TH_NNCV = matrix(0,1,100)
# for (kk in 0:100){
#   THRESHOLD_NNCV = kk/100
#   TH_NNCV[kk] = THRESHOLD_NNCV
#   Predicted_NNCV <- matrix(NA,1,length(predicted_NNCV))
#   Predicted_NNCV[which(predicted_NNCV>THRESHOLD_NNCV)]=1
#   Predicted_NNCV[which(predicted_NNCV<=THRESHOLD_NNCV)]=0
#   Predicted_NNCV <- factor(Predicted_NNCV)
#   actual_NNCV <- factor(actual_NNCV)
#   library('caret')
#   CMat_trn = confusionMatrix(Predicted_NNCV,actual_NNCV)
#   ACC_NNCV[kk] = CMat_trn$overall[1]
# }
# THRESHOLD_NNCV = TH_NNCV[which.max(ACC_NNCV)]
# Predicted_NNCV <- matrix(NA,1,length(predicted_NNCV))
# Predicted_NNCV[which(predicted_NNCV>THRESHOLD_NNCV)]=1
# Predicted_NNCV[which(predicted_NNCV<=THRESHOLD_NNCV)]=0
# Predicted_NNCV <- factor(Predicted_NNCV)
# actual_NNCV <- factor(actual_NNCV)
# library('caret')
# CMat_trn = confusionMatrix(Predicted_NNCV,actual_NNCV)
# 
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1])
# 
# THRESHOLD_NN = (THRESHOLD_NN+THRESHOLD_NNCV)/2

```

### PERFORMING ANOTHER NEURAL NETWORK 
```{r}
# #install.packages("nnet")
# library("nnet")
# 
# TRN_DATA_NN = apply(TRN_DATA,2,as.numeric)
# TST_DATA_NN = apply(TST_DATA,2,as.numeric)
# 
# TRN_DATA_NN = TRN_DATA_NN[complete.cases(TRN_DATA_NN), ]
# TRN_DATA_NN = TRN_DATA_NN[complete.cases(TRN_DATA_NN), ]
# 
# N = 100 #ncol(TRN_DATA)
# # ONE -- MODEL
# 
# TRN_DATA_NNnormed = TRN_DATA_NN
# for (i in 2:6){
#   TRN_DATA_NNnormed[,i]=TRN_DATA_NNnormed[,i]-min(TRN_DATA_NNnormed[,i])
#   TRN_DATA_NNnormed[,i]=TRN_DATA_NNnormed[,i]/max(TRN_DATA_NNnormed[,i])
# }
# 
# result_NN <- nnet(TRN_DATA_NNnormed[,2:N], TRN_DATA_NNnormed[,1], size = 5, decay=5e-4, maxit=200)
# 
# 
# TST_DATA_NNnormed = TST_DATA_NN
# for (i in 2:6){
#   TST_DATA_NNnormed[,i]=TST_DATA_NNnormed[,i]-min(TST_DATA_NNnormed[,i])
#   TST_DATA_NNnormed[,i]=TST_DATA_NNnormed[,i]/max(TST_DATA_NNnormed[,i])
# }
# 
# 
# # TWO -- PREDICT
# predicted_NN = predict(result_NN, TST_DATA_NN[,2:N], type="raw")
# actual_NN = (TST_DATA$BADBUY)
# 
# # THREE -- ROC
# library('pROC')
# ROC_N = roc(actual_NN,predicted_NN)
# plot.roc(ROC_N,main = sprintf('AUC = %f', ROC_N$auc))
# 
# # FOUR -- ACCURACY
# ACC_NN = matrix(0,1,100)
# TH_NN = matrix(0,1,100)
# for (kk in 0:100){
#   THRESHOLD_NN = kk/100
#   TH_NN[kk] = THRESHOLD_NN
#   Predicted_NN <- matrix(NA,1,length(predicted_NN))
#   Predicted_NN[which(predicted_NN>THRESHOLD_NN)]=1
#   Predicted_NN[which(predicted_NN<=THRESHOLD_NN)]=0
#   Predicted_NN <- factor(Predicted_NN)
#   actual_NN <- factor(actual_NN)
#   library('caret')
#   CMat_trn = confusionMatrix(Predicted_NN,actual_NN)
#   ACC_NN[kk] = CMat_trn$overall[1]
# }
# THRESHOLD_NN = TH_NN[which.max(ACC_NN)]
# Predicted_NN <- matrix(NA,1,length(predicted_NN))
# Predicted_NN[which(predicted_NN>THRESHOLD_NN)]=1
# Predicted_NN[which(predicted_NN<=THRESHOLD_NN)]=0
# Predicted_NN <- factor(Predicted_NN)
# actual_NN <- factor(actual_NN)
# library('caret')
# CMat_trn = confusionMatrix(Predicted_NN,actual_NN)
# 
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1])
# 
# 
# 
#  # FIVE -- PREDICT
# predicted_NNCV = (predict(result_NN, newdata = CV_DATA, type = "response"))
# actual_NNCV = (CV_DATA$BADBUY)
# ACC_NNCV = matrix(0,1,100)
# TH_NNCV = matrix(0,1,100)
# for (kk in 0:100){
#   THRESHOLD_NNCV = kk/100
#   TH_NNCV[kk] = THRESHOLD_NNCV
#   Predicted_NNCV <- matrix(NA,1,length(predicted_NNCV))
#   Predicted_NNCV[which(predicted_NNCV>THRESHOLD_NNCV)]=1
#   Predicted_NNCV[which(predicted_NNCV<=THRESHOLD_NNCV)]=0
#   Predicted_NNCV <- factor(Predicted_NNCV)
#   actual_NNCV <- factor(actual_NNCV)
#   library('caret')
#   CMat_trn = confusionMatrix(Predicted_NNCV,actual_NNCV)
#   ACC_NNCV[kk] = CMat_trn$overall[1]
# }
# THRESHOLD_NNCV = TH_NNCV[which.max(ACC_NNCV)]
# Predicted_NNCV <- matrix(NA,1,length(predicted_NNCV))
# Predicted_NNCV[which(predicted_NNCV>THRESHOLD_NNCV)]=1
# Predicted_NNCV[which(predicted_NNCV<=THRESHOLD_NNCV)]=0
# Predicted_NNCV <- factor(Predicted_NNCV)
# actual_NNCV <- factor(actual_NNCV)
# library('caret')
# CMat_trn = confusionMatrix(Predicted_NNCV,actual_NNCV)
# 
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1])
# 
# THRESHOLD_NN = (THRESHOLD_NN+THRESHOLD_NNCV)/2
# 

```


### PERFORMING NAIVE BAYSIAN
### note this model was not used because of overfitting

```{r}
# #install.packages("naivebayes")
# library("naivebayes")
# 
# N = 20 #ncol(TRN_DATA)
# # ONE -- MODEL
# result_NB <- naive_bayes(TRN_DATA[,1:N], TRN_DATA$BADBUY, prior = c(1,1)/2)
# print(result_NB)
# 
# # TWO -- PREDICT
# predicted_NB = (predict(result_NB, newdata = TST_DATA, type = "class"))
# actual_NB = (TST_DATA$BADBUY)
# 
# 
# # FOUR -- ACCURACY
# Predicted_NB <- factor(predicted_NB)
# actual_NB <- factor(actual_NB)
# library('caret')
# CMat_trn = confusionMatrix(Predicted_NB,actual_NB)
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1])
# 
# 
#  # FIVE -- PREDICT
# predicted_NBCV = (predict(result_NB, newdata = CV_DATA, type = "class"))
# actual_NBCV = (CV_DATA$BADBUY)
# 
# Predicted_NBCV <- factor(predicted_NBCV)
# actual_NBCV <- factor(actual_NBCV)
# library('caret')
# CMat_trn = confusionMatrix(Predicted_NBCV,actual_NBCV)
# 
# sprintf('sensitivity = %f', CMat_trn$byClass[1])
# sprintf('specificity = %f', CMat_trn$byClass[2])
# sprintf('Accuracy = %f', CMat_trn$overall[1])

```






### APPLYING MODELS TO TEST TDATA
```{r}


### LOG REGRESSION
predicted_logTEST = predict(result_log, newdata = TEST_SET, type = "response")

Predicted_logTEST <- matrix(NA,1,length(predicted_logTEST))
Predicted_logTEST[which(predicted_logTEST>=THRESHOLD_log)]=1
Predicted_logTEST[which(predicted_logTEST<THRESHOLD_log)]=0
Predicted_logTEST <- factor(Predicted_logTEST)

TEST = read.csv("test_dataset.csv", header = TRUE)

ANSWER = data.frame(TEST$RT13Id,Predicted_logTEST)
colnames(ANSWER)[1]="RT13Id"
colnames(ANSWER)[2]="IsBadBuy"
write.csv(ANSWER,'NAVEEN_ANSWER_log.csv')




### SVM
predicted_svmTEST = predict(result_svm, newdata = TEST_SET, type = "response", na.action = na.exclude)

Predicted_svmTEST <- matrix(NA,1,length(predicted_svmTEST))
Predicted_svmTEST[which(predicted_svmTEST>=THRESHOLD_svm)]=1
Predicted_svmTEST[which(predicted_svmTEST<THRESHOLD_svm)]=0
Predicted_svmTEST <- factor(Predicted_svmTEST)

TEST = read.csv("test_dataset.csv", header = TRUE)

ANSWER = data.frame(TEST$RT13Id,Predicted_svmTEST)
colnames(ANSWER)[1]="RT13Id"
colnames(ANSWER)[2]="IsBadBuy"
write.csv(ANSWER,'NAVEEN_ANSWER_svm.csv')




### RF
predicted_RFTEST = predict(result_RF, newdata = TEST_SET, type = "response", na.action = na.exclude)

Predicted_RFTEST <- matrix(NA,1,length(predicted_RFTEST))
Predicted_RFTEST[which(predicted_RFTEST>=THRESHOLD_RF)]=1
Predicted_RFTEST[which(predicted_RFTEST<THRESHOLD_RF)]=0
Predicted_RFTEST <- factor(Predicted_RFTEST)

TEST = read.csv("test_dataset.csv", header = TRUE)

ANSWER = data.frame(TEST$RT13Id,Predicted_RFTEST)
colnames(ANSWER)[1]="RT13Id"
colnames(ANSWER)[2]="IsBadBuy"
write.csv(ANSWER,'NAVEEN_ANSWER_RF.csv')




# ### NB
# predicted_NBTEST = predict(result_NB, newdata = TEST_SET, type = "class", na.action = na.exclude)
# Predicted_NBTEST <- factor(predicted_NBTEST)
# 
# TEST = read.csv("test_dataset.csv", header = TRUE)
# 
# ANSWER = data.frame(TEST$RT13Id,Predicted_NBTEST)
# colnames(ANSWER)[1]="RT13Id"
# colnames(ANSWER)[2]="IsBadBuy"
# write.csv(ANSWER,'NAVEEN_ANSWER_NB.csv')



```


rmarkdown::render("sendhilnathan_PROJECT.Rmd", "pdf_document")