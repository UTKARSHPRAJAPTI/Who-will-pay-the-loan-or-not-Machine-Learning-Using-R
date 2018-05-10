#jig14947

#Ste_Path
setwd("C:\\Data Science with R\\Assignments\\Graded Assignments\\Topic 11.2 -  Decison Trees")

#Import_File
data<-read.csv("BH.csv",stringsAsFactors = T)
str(data)
summary(data)

names(data)
head(data)
sum(is.na(data))

#Import_libraries
library(dplyr)
library(irr)
library(rpart)
library(caret)
library(rattle)
library(rpart.plot)
library(RColorBrewer)

#Store in data1, cus_employename and TARGET variables 
data1<-select(data,9,110)
str(data1)

df<-

#Convet the TARGET variable from int to Factor
data1$TARGET<-as.factor(data1$TARGET)
str(data1)
summary(data1)

#Check Missing values from the TARGET variable and also Delete them
sum(is.na(data1$TARGET))
index<-which(is.na(data1$TARGET))
data1<-data1[-index,]

summary(data1)
View(data1)
dim(data1)

#Decision tree modelling using rpart function

#mod1<-rpart(TARGET~.,data = data1,control = rpart.control(cp=0.005,minsplit = 2),parms = list(split="gini"))

mod<-rpart(TARGET~cus_employername,data = data1,method="class",
              control = rpart.control(cp=0.005,minsplit = 2),parms = list(split="gini"))
summary(mod)
#Ploting the tree

#plot(mod1,margin = 0.1,main="classification tree for good and bad customers")
#text(mod,use.n = TRUE,all = TRUE,cex=.7)
fancyRpartPlot(mod)
options(scipen = 999)

#Extracting_the_Rules
asRules(mod)
# [TARGET=1 cover=8692 (38%) prob=0.88]  
# [TARGET=0 cover=14196 (62%) prob=0.19]

#Extracting the_Rules
mod$node<-mod$where
head(mod$node,100)

#Create two Group
data1 %>% mutate(Groups=ifelse(mod$node==2,"Group1","Group2"))->data1
View(data1)

#Checking Bad rate in each group.
data1$Groups<-as.factor(data1$Groups)
table(data1$Groups,data1$TARGET)/(nrow(data1))*100

#                 0         1
#  Group1 50.179133 11.844635   Bad rate is High  50%
#  Group2  4.674939 33.301293   Bad rate is low   4.7%



#Before the model is built

#Q1  :-- How many missing values are there for the variable name "TARGET"
sum(is.na(data$TARGET))
#Ans :-- 6217 missing values are present in TARGET Variable.

#Q2  :-- How many unique observation are there for the variable named "cus_emplotername"
length(unique(data$cus_employername))            #  16193 Unique values are avaliable in cus_emplotername(data1)

#Q3  :-- The variable name  "GOOD_BAD" represent
names(data[56])
unique(data[56])
str(data[56])
sum(is.na(data[56]))
#Ans  :-- 3 are unique

#Q4  :-- How many Rows are left in data1
sum(is.na(data1))
dim(data1)
#Ans  :-- 22888 Rows are present

#Q5  :-- How many duplicates are there in data1$cus_employername dataframe
sum(duplicated(data1$cus_employername))
#Ans  :--  9605 duplicates are avaliable in $cus_employername

#Q6  :--  How many rows will be there in data frame from which missing values in target column have been eliminated
#Store in data1, cus_employename and TARGET variables 
data2<-select(data,9,110)
sum(is.na(data2$TARGET))
str(data2)
summary(data2)

#Convet the TARGET variable from int to Factor
data2$TARGET<-as.factor(data2$TARGET)
str(data2)
summary(data2)

#Check Missing values from the TARGET variable and also Delete them
sum(is.na(data2$TARGET))
#Ans  :-- 6217 Rows

#Q8  :-- How many entries exist in the group having high bad rate
unique(data1$Groups)
table(data1$Groups)
#Ans :--  14196






