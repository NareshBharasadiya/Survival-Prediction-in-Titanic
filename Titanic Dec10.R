#Add "Survived" variable to the test set to allow for combining data sets, create a new data frame test.survived

test.survived <- data.frame(Survived=rep("None",nrow(test)),test[,])

#Combine data sets

data.combined <- rbind(train,test.survived)

# R data type
str(data.combined)

#change some variables to factor from character - categorical variables

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$pclass <- as.factor(data.combined$Pclass)
data.combined$Name <- as.factor(data.combined$Name)
data.combined$Sex <- as.factor(data.combined$Sex)
data.combined$Ticket <- as.factor(data.combined$Ticket)
data.combined$Cabin <- as.factor(data.combined$Cabin)
data.combined$Embarked <- as.factor(data.combined$Embarked)

# Look at survival data, check the skewness in data. if its 10 to 1 then we need to do other thing. here in this case it is around 2 to 1
table(data.combined$Survived)

# Distribution across classes - Most in 3rd class, here people in class 1 is greater than class 2 which is intresting
table(data.combined$Pclass)

# load ggplot2
library(ggplot2)

str(train)
train$Pclass <- as.integer(train$Pclass)
train$Survived <- as.factor(train$Survived)

# hypothesis - Rich folks survived at higher rate

ggplot(train, aes(x=Pclass, fill=factor(Survived)))+geom_histogram()+xlab("PClass")+ylab("Total Count")+labs(fill="Survived")

# Examine the first few names in the training dataset

head(train$Name)

# How many unique names are there across both train and test?

length(unique(as.character(data.combined$Name)))
# data frame size is 1309 but unique based on name came to 1307 hence there are 2 duplicate names


# Two duplicate names which we have to find

dup.names <- data.combined[which(duplicated(as.character(data.combined$Name))),"Name"]
dup.names[] <- lapply(dup.names,as.character)

#---------------------------------------------------------------------------------
# Take a look at the records in the combined data set # This part didnt worked
data.combined[which(data.combined$Name %in% dup.names),]  # look in data.comined$Name where name are equal to one available in dup.names
#----------------------------------------------------------------------------------

#What is up with 'Miss' and 'Mr.' thing
library(stringr)

misses <- data.combined[which(str_detect(data.combined$Name,"Miss.")),]
misses[1:5,]

# Mrs. - name titles correlate with age
mrses <- data.combined[which(str_detect(data.combined$Name,"Mrs.")),]
mrses[1:5,]

# Check out males to see if pattern continues
males <- data.combined[which(data.combined$Sex=='male'),]
males[1:5,]

# Expand upomn the relstionship between 'Survived' and 'PClass'by adding new column 'Title'


data.combined$Name = as.character(data.combined$Name)
head(data.combined)

extractTitle <- function(Name)
{
  
  if(length(grep("Miss.", Name))>0)
    {
    return("Miss.")
  }
  else if(length(grep("Master", Name))>0)
    {
    return("Master.")
  }
  else if(length(grep("Mrs.", Name))>0)
    {
    return("Mrs.")
  }
  else if(length(grep("Mr.", Name))>0)
    {
    return("Mr.")
  }
    else
      {
      return("Other.")
    }
}


titles = NULL
for(i in 1:nrow(data.combined)){
  titles = c(titles,extractTitle(data.combined[i,"Name"]))
}

data.combined$title = as.factor(titles)
  
# ggplot

ggplot(data.combined[1:891,],aes(x=title,fill=Survived))+geom_bar()+facet_wrap(~pclass)+ggtitle("PClass")+
        xlab("Title")+ylab("Total Count")+labs(fill="Survived")

# Family size

data.combined$familysize <- as.factor(data.combined$SibSp+data.combined$Parch+1)

# Ticket
str(data.combined$Ticket)

# Cobvert to character

data.combined$Ticket <- as.character(data.combined$Ticket)
data.combined$Ticket[1:20]

ticket.first.char <- ifelse(data.combined$title==""," ",substr(data.combined$Ticket,1,1))
unique(ticket.first.char)

# We can combine ticket first char in data frame and do graphic analysis

data.combined$ticket.first.char = as.factor(ticket.first.char)

ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=Survived))+geom_bar()+
  xlab("Title")+ylab("Total Count")+labs(fill="Survived")


ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=Survived))+geom_bar()+facet_wrap(~pclass)+ggtitle("PClass")+
  xlab("Title")+ylab("Total Count")+labs(fill="Survived")

# To remove column from data frame
data.combined$pclass <- NULL


ggplot(data.combined[1:891,],aes(x=ticket.first.char,fill=Survived))+geom_bar()+facet_wrap(~Pclass + title)+ggtitle("PClass")+
  xlab("Title")+ylab("Total Count")+labs(fill="Survived")

# Price

ggplot(data.combined,aes(x=Fare))+geom_histogram(binwidth = 5)+ggtitle("Combined fare distribution")+
  xlab("Fare")+ylab("Total Count")

# Lets check to see if fare hs predictive power

ggplot(data.combined[1:891,],aes(x=Fare,fill=Survived))+geom_histogram(binwidth = 5)+facet_wrap(~Pclass + title)+ggtitle("PClass, Title")+
  xlab("Fare")+ylab("Total Count")+labs(fill="Survived")


# Cabin
str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)
data.combined$Cabin[1:100]

# Replace empty cabins with U

data.combined[which(is.na(data.combined$Cabin)),"Cabin"] <- "U"
data.combined$Cabin[1:100]

# Take first character of cabin
cabin.first.char <- as.factor(substr(data.combined$Cabin,1,1))
str(cabin.first.char)
levels(cabin.first.char)
unique(cabin.first.char)

# add to data frame

data.combined$cabin.first.char<-as.factor(cabin.first.char)


############################################################################################
# Video 4 - Exploratory Model
###############################################################################################

library(randomForest)

rf.train.1 <- data.combined[1:891,c("Pclass","title")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.1 <- randomForest(x=rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1)


# Train random forest using Pclass, title & sibsp

rf.train.2 <- data.combined[1:891,c("Pclass","title","SibSp")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.2 <- randomForest(x=rf.train.2, y=rf.label, importance = TRUE, ntree = 1000)
rf.2
varImpPlot(rf.2)


# Train random forest using Pclass, title & parch

rf.train.3 <- data.combined[1:891,c("Pclass","title","Parch")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.3 <- randomForest(x=rf.train.3, y=rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)


# Train random forest using Pclass, title, sibsp & parch

rf.train.4 <- data.combined[1:891,c("Pclass","title","Parch","SibSp")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.4 <- randomForest(x=rf.train.4, y=rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)


# Train random forest using Pclass, title, & family size

rf.train.5 <- data.combined[1:891,c("Pclass","title","familysize")]
rf.label <- as.factor(train$Survived)

set.seed(1234)
rf.5 <- randomForest(x=rf.train.5, y=rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)

#_____________________________________________________________________________________________________
# Video #5
#_____________________________________________________________________________________________________


# test the model on test data

test.submit.df <- data.combined[892:1309, c("Pclass","title","familysize")]

#Make predictions

rf.5.preds <- predict(rf.5,test.submit.df)
table(rf.5.preds)

# write out a csv file for submission to kaggle

submit.df <- data.frame(PassengerID=rep(892:1309),Survived=rf.5.preds)

write.csv(submit.df,file = "NB_Sub.csv",row.names = FALSE)


# cross validation to measure performance on test and training data

# Caret package

install.packages("e1071") 
install.packages("psych") 
install.packages("caret") # classification and regression training package
library(psych)
install.packages("doSNOW")
library(caret)
library(e1071) 
library(doSNOW)# Parellel threads are created by R

# leverage caret to create a 100 total folds with startification

set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label,k=10,times=10)


# check stratification

table(rf.label)
342/549

# random check of 33 rd sample for stratification, it should meet the proportion of 0 to 1 as in actual data i.e. 62.2
# Verification of stratification

table(rf.label[cv.10.folds[[33]]])
308/494


# setup carets train control object per above

ctrl.1 <- trainControl(method = "repeatedcv",number = 10,repeats = 10,index = cv.10.folds)

# Setup doSNOW package for multi core training. This is helpful as we are going to be training a lot of trees.

cl <- makeCluster(6,type = "SOCK")
registerDoSNOW(cl)

# set seed for reproducibility and train
set.seed(34324)
rf.5.cv.1 <- train(x=rf.train.5,y=rf.label,method="rf",tuneLength=3,ntree=1000,trControl=ctrl.1)

# shutdown cluster
stopCluster(cl)

#check results
rf.5.cv.1

# result say accuracy is 81.2 on average, however that is more than what we got from kaggle result which was around 79
# so, here we are still overfitting the model to data, possible reason is we are training the model with 90% data i.e. 10/90
# so we try with k equals to 5, which means we are training it on 80% data by changing k=5
# with 5 we got the same accuracy so we try k=3 this time, which means we train on 66% data.


#_________________________________________________________________________________________________________________________________________
# Video 6 - Explarotary Model
#_________________________________________________________________________________________________________________________________________

# Lets use ssingled decision tree to better understand whats going on with our feeatures. Obviously Random Forests are far more powerful than single trees,
# but single trees have the advantage of being easier to understand

# Decision trees uses a CART algoritham

library(rpart)
install.packages("rpart.plot")
library(rpart.plot)

# as per video#5 lets use 3 fold CV repeated 10 times

# Create utility functions


rpart.cv <- function(seed, training, labels, ctrl)
{
  cl <- makeCluster(6, type = "SOCK")
  registerDoSNOW(cl)
  
  set.seed(seed)
  
  #Leverage formula interface for training
  
  rpart.cv <- train(x=training, y=labels, method = "rpart", tuneLength=30, trControl = ctrl)
  
  # shutdown cluster
  stopCluster(cl)
  
  return(rpart.cv)
  
  
}


# Grab Features
features <- c("Pclass","title","familysize")
rpart.train.1 <- data.combined[1:891,features]

# Run CV and check out results
set.seed(2348)
cv.3.folds <- createMultiFolds(rf.label,k=3,times=10)
ctrl.3 <- trainControl(method = "repeatedcv",number = 10,repeats = 10,index = cv.3.folds)
rpart.1.cv.1 <- rpart.cv(94622, rpart.train.1, rf.label, ctrl.3)

rpart.1.cv.1

# plot

prp(rpart.1.cv.1$finalModel, type = 0, extra = 1, under = TRUE)

# parse out last name and title
data.combined[1:25,"Name"]

name.splits <- str_split(data.combined$Name, ",")
name.splits[1]
last.names <- sapply(name.splits,"[",1)
last.names[1:10]

# add last names to dta frames to find it useful later
data.combined$last.name <- last.name

# Now for titles

name.splits <- str_split(sapply(name.splits, "[",2)," ")
titless <- sapply(name.splits,"[",2)
unique(titless)

# find what is the
data.combined[which(titless=='the'),]

# Re-Map titles to be more wxact

titless[titless %in% c("Dona.","the")] <- "Lady."
titless[titless %in% c("Ms.","Mlle.")] <- "Miss."
titless[titless == "Mme."] <- "Mrs."
titless[titless %in% c("Jonkheer.","Don.")] <- "Sir."
titless[titless %in% c("Col.","Capt.","Major.")] <- "Officer"
table(titless)

# Make title a factor
data.combined$titles <- as.factor(titless)

# Visualize new data
ggplot(data.combined[1:891,], aes(x=titles, fill=Survived))+geom_bar()+facet_wrap(~Pclass)+ggtitle("Survival Rates for new.title by Pclass")

# Collapse titles based on visualanalysis
indexes <- which(data.combined$titles=="Lady.")
data.combined$titles[indexes] <- "Mrs."

indexes <- which(data.combined$titles=="Dr." | data.combined$titles=="Rev." | data.combined$titles=="Sir." | data.combined$titles=="Officer")
data.combined$titles[indexes] <- "Mr."

# Visualize new data
ggplot(data.combined[1:891,], aes(x=titles, fill=Survived))+geom_bar()+facet_wrap(~Pclass)+ggtitle("Survival Rates for new.title by Pclass")


# Grab features

features <- c("Pclass","titles","familysize")
rpart.train.2 <- data.combined[1:891,features]

# Run CV and check out results

rpart.2.cv.1 <- rpart.cv(94622, rpart.train.2, rf.label, ctrl.3)

rpart.2.cv.1


# plot

prp(rpart.2.cv.1$finalModel, type = 0, extra = 1, under = TRUE)


# Dive into 1st class Mr

indexes.first.mr <- which(data.combined$titles == "Mr." & data.combined$Pclass=="1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

# One Female?
first.mr.df[first.mr.df$Sex=="female",]

# Update new.title feature

indexes <- which(data.combined$titles=="Mr." & data.combined$Sex=="female")
data.combined$titles[indexes] <- "Mrs."

# Any other gender slipups

length(which(data.combined$Sex=="female" & (data.combined$titles=="Master" & data.combined$titles=="Mr.")))


# Refresh 1st class Mr data frame

indexes.first.mr <- which(data.combined$titles == "Mr." & data.combined$Pclass=="1")
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

# look at surviving of 1st class "Mr."
summary(first.mr.df[first.mr.df$Survived=="1",])
View(first.mr.df[first.mr.df$Survived=="1",])

#Take a look at some high fares
indexes <- which(data.combined$Ticket=="PC 17755" |data.combined$Ticket=="PC 17611" |data.combined$Ticket=="PC 113760")
View(data.combined[indexes,])

# Visualize survival rates of 1st class by mr by fare
ggplot(first.mr.df, aes(x=Fare, fill=Survived))+geom_density(alpha=0.5)+ggtitle("First class Mr. by fare")

# Engineer features based on all the passengers with the same ticket
ticket.party.size <- rep(0,nrow(data.combined))
avg.fare <- rep(0.0, nrow(data.combined) )
tickets <- unique(data.combined$Ticket)

for(i in 1:length(tickets)){
  current.ticket <- tickets[i]
  party.indexes <- which(data.combined$Ticket==current.ticket)
  current.avg.fare <- data.combined[party.indexes[1], "Fare"]/length(party.indexes)
  
  for(k in 1:length(party.indexes)){
    ticket.party.size[party.indexes[k]] <- length(party.indexes)
    avg.fare[party.indexes[k]] <- current.avg.fare
  }
  
}

data.combined$ticket.party.size <- ticket.party.size
data.combined$avg.fare <- avg.fare

# Refresh 1st class Mr. data frame  # Not working from hereon
first.mr.df <- data.combined[indexes.first.mr,]
summary(first.mr.df)

#____________________________________________________________________________________________________________________________
# Video 7 - Submitting, scoring and features
#_______________________________________________________________________________________________________________________


# Subset test records and features

test.submit.df <- data.combined[892:1309,features]

# Make Predictions


# feature engineer to improve the misclassification rate

# Mutual information, to do feature analysis - check nil.stanford.edu

install.packages("infotheo") # Package on mutual information
library(infotheo)

mutinformation(rf.label, data.combined$Pclass[1:891])
mutinformation(rf.label, data.combined$Sex[1:891])
mutinformation(rf.label, data.combined$SibSp[1:891])
mutinformation(rf.label, data.combined$Parch[1:891])
mutinformation(rf.label, discretize(data.combined$Fare[1:891])) # fare is a decimal number, continuous variable, discretize turns continuous to discrete variable
mutinformation(rf.label, data.combined$Embarked[1:891])
mutinformation(rf.label, data.combined$title[1:891])
mutinformation(rf.label, data.combined$familysize[1:891])
mutinformation(rf.label, data.combined$ticket.first.char[1:891])
mutinformation(rf.label, data.combined$titles[1:891])
mutinformation(rf.label, data.combined$ticket.party.size[1:891])

# entropy and gini in decision trees are based on mutual information only, hence in a way result of this and decision tree weightage should match

# Leverage the tsne algorithm to create a 2D representation of our data
# These are used for dimension reduction - these are not required in major way in trees, but could be used for linear predictions i.e. regression models

install.packages("Rtsne")
library(Rtsne)

most.correct <- data.combined[data.combined$titles != "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.1 <- Rtsne(most.correct[,features],check_duplicates=FALSE)
ggplot(NULL, aes(x=tsne.1$Y[indexes,1],y=tsne.1$Y[indexes,2],color=most.correct$Survived[indexes]))+geom_point()+labs(color="Survived") + ggtitle("tsne 2D Visualization of Females and Boys")

# Condition information
condinformation(most.correct$Survived[indexes],discretize(tsne.1$Y[indexes,]))

condinformation(rf.label,data.combined[1:891,c("titles","Pclass")])

# Now look at the Mr. where we did incorrect

most.correct <- data.combined[data.combined$titles == "Mr.",]
indexes <- which(most.correct$Survived != "None")

tsne.2 <- Rtsne(most.correct[,features],check_duplicates=FALSE)
ggplot(NULL, aes(x=tsne.2$Y[indexes,1],y=tsne.2$Y[indexes,2],color=most.correct$Survived[indexes]))+geom_point()+labs(color="Survived") + ggtitle("tsne 2D Visualization of Females and Boys")

# Use tsne to create features and use it in model

tsne.3 <- Rtsne(data.combined[,features],check_duplicates=FALSE)
ggplot(NULL, aes(x=tsne.3$Y[1:891,1],y=tsne.3$Y[1:891,2],color=data.combined$Survived[1:891]))+geom_point()+labs(color="Survived") + ggtitle("tsne 2D Visualization of Females and Boys")




