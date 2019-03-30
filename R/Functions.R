setwd("~/OneDrive - IESEG/Semestre_2/Statistics and Machine Learning for MKT/Kaggle/data")
train<-readRDS(file = "train.rds")
train1<-train

# Function for reducing levels in factor variable
# Inputs: fvar: factor variable, perc: % min to be considered as a relevant level.
redu_levels<- function(fvar,perc=0.01){
  fvar<-as.factor(fvar)
  # Condition level usage less that perc%
  condition<-table(fvar)/length(fvar) < perc
  # Creating the level LFL
  levels(fvar)<-c(levels(fvar),"LFL")
  lfl<-names(which(condition))
  # Replacing Low Frequency Levels with LFL
  fvar[fvar %in% lfl] <-"LFL"
  # Dropping levels with freq zero
  fvar<-droplevels(fvar)
  # Retuning the new level distribution in %
  flevels<-round(table(fvar)/length(fvar)*100,3)
  print(flevels)
  return(fvar)
}

# Running the function for 27-1 variables
train1[,2:27]<-as.data.frame(lapply(train1[,2:27],redu_levels,perc=0.01))

# Verifying
lapply(train1[,2:27],function(x) round(table(x)/length(x)*100,3))

# Comparing before/after levels
before<-sapply(train[,2:27], function(x) length(levels(as.factor(x))))
after<-sapply(train1[,2:27], function(x) length(levels(x)))
cbind(before,after)



