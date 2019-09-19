#########################
#########################
#    Functions in R     #
#   By: James Ponce     #
#########################
#########################

####################################################
# Function for reducing levels in factor variable  #
# Inputs:                                          #
# fvar: variable in vector format                  #
# perc: % min to be considered as a relevant level #
# Output:
# fvar with factors reduced.
####################################################
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

###########################################################################
# Function to calculate the most frequent category in categorial variable.
# Input: vector x (categorical)
# Output: most popular category value
###########################################################################
most_frequent_category <- function(x) {
  categories <- table(x)
  return(names(categories[which.max(categories)]))
}





