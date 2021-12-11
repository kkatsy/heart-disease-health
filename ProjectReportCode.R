##Final Project Group 11:
##Topic: Heart Failure Prediction###################################################
rm(list=ls()) 
setwd("~/Desktop/Final Project")
new_data<-read.csv(file ='new_data.csv') 
new_data
##Subsetting variables
new_data1 <- subset(new_data, select= c("HeartDiseaseorAttack","AnyHealthcare","BMI",
                                        "Education","HighBP","HighChol","Income","PhysActivity","Sex","Smoker"))
new_data1
print(new_data1)

##/* Function*/
is_valid <- function(df) {
  expected_columns <- c('Age',
                        'AnyHealthcare',
                        'BMI',
                        'CholCheck',
                        'Diabetes',
                        'DiffWalk',
                        'Education',
                        'Fruits',
                        'GenHlth',
                        'HeartDiseaseorAttack',
                        'HighBP',
                        'HighChol',
                        'HvyAlcoholConsump',
                        'Income',
                        'MentHlth',
                        'NoDocbcCost',
                        'PhysActivity',
                        'PhysHlth',
                        'Sex',
                        'Smoker',
                        'Stroke',
                        'Veggies')
  # Assert that the input dataframe contains expacetd values
  columns_identical <- (identical(sort(names(df)), expected_columns))
  # Assert that there are no missing values in the dataframe
  empty_columns <- sum(is.na(df))
  # Assert that there are no negative values in the dataframe
  negative_columns <- any(df < 0)
  # Assert that the values for BMI column fall within 0 < BMI < 100
  BMI_out_of_range <-  df$BMI > 0 && df$BMI < 100
  
  return (columns_identical 
          && (empty_columns==0)
          && !negative_columns 
          && BMI_out_of_range)
}

df <- read.csv("C:/Users/lgigo/Downloads/heart-disease-health-main/heart-disease-health-main/heart_disease_health_indicators.csv")

is_valid(df)


##/* Function*/
is_valid <- function(df) {
  expected_columns <- c('Age',
                        'AnyHealthcare',
                        'BMI',
                        'CholCheck',
                        'Diabetes',
                        'DiffWalk',
                        'Education',
                        'Fruits',
                        'GenHlth',
                        'HeartDiseaseorAttack',
                        'HighBP',
                        'HighChol',
                        'HvyAlcoholConsump',
                        'Income',
                        'MentHlth',
                        'NoDocbcCost',
                        'PhysActivity',
                        'PhysHlth',
                        'Sex',
                        'Smoker',
                        'Stroke',
                        'Veggies')
  # Assert that the input dataframe contains expacetd values
  columns_identical <- (identical(sort(names(df)), expected_columns))
  # Assert that there are no missing values in the dataframe
  empty_columns <- sum(is.na(df))
  # Assert that there are no negative values in the dataframe
  negative_columns <- any(df < 0)
  # Assert that the values for BMI column fall within 0 < BMI < 100
  BMI_out_of_range <-  df$BMI > 0 && df$BMI < 100
  
  return (columns_identical 
          && (empty_columns==0)
          && !negative_columns 
          && BMI_out_of_range)
}
df <- read.csv(file ='new_data.csv') 
is_valid(df)

##Use descriptive stats for the data
library(Hmisc)
describe(new_data1)
str(new_data1)

##Provide Factor for categorical variables
new_data1$HeartDiseaseorAttack.f<-factor(new_data1$HeartDiseaseorAttack)
new_data1$AnyHealthcare.f<-factor(new_data1$AnyHealthcare)
new_data1$Education.f<-factor(new_data1$Education)
new_data1$HighBP.f<-factor(new_data1$HighBP)
new_data1$HighChol.f<-factor(new_data1$HighChol)
new_data1$Income.f<-factor(new_data1$Income)
new_data1$PhysActivity.f<-factor(new_data1$PhysActivity)
new_data1$Sex.f<-factor(new_data1$Sex)
new_data1$Smoker.f<-factor(new_data1$Smoker)
new_data1$BMI.L<-log(new_data1$BMI)
new_data2 <- subset(new_data1, select= c("HeartDiseaseorAttack.f","AnyHealthcare.f","BMI.L","Education.f","HighBP.f","HighChol.f","Income.f","PhysActivity.f","Sex.f","Smoker.f"))
##Use descriptive stat for new_data2
table(new_data2$HeartDiseaseorAttack.f)
table(new_data2$AnyHealthcare.f)
table(new_data2$Education.f)
table(new_data2$HighBP.f)
table(new_data2$HighChol.f)
table(new_data2$Income.f)
table(new_data2$Smoker.f)
table(new_data2$Sex.f)
table(new_data2$PhysActivity.f)
##Check the variables based on Heart Disease
table(new_data2$HeartDiseaseorAttack.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$AnyHealthcare.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$Education.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$HighBP.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$HighChol.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$Income.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$Smoker.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$Sex.f)
table(new_data2$HeartDiseaseorAttack.f,new_data2$PhysActivity.f)

##Frequency Graph
library(ggplot2)
install.packages("ggpubr")
library(ggpubr)
theme_set(theme_pubr())
a<-ggplot(new_data2, aes(HeartDiseaseorAttack.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

b<-ggplot(new_data2, aes(AnyHealthcare.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


c<-ggplot(new_data2, aes(Education.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


d<-ggplot(new_data2, aes(HighBP.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


e<-ggplot(new_data2, aes(HighChol.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


f<-ggplot(new_data2, aes(Income.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


g<-ggplot(new_data2, aes(Smoker.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

h<-ggplot(new_data2, aes(Sex.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()


i<-ggplot(new_data2, aes(PhysActivity.f)) +
  geom_bar(fill = "#0073C2FF") +
  theme_pubclean()

ggarrange(a, b, c,d,e,f,g,h,i + rremove("x.text"), 
          labels = c("A", "B", "C","D","E","F","G","H","I"),
          ncol = 3, nrow = 3)

##ggplot2 histogram
library(ggplot2)
# Basic histogram
a<-ggplot(new_data1, aes(x=BMI)) + geom_histogram()
##Use log transform to make distribution normal
BMI.L<-log(new_data1$BMI)
b<-ggplot(new_data1, aes(x=BMI.L)) + geom_histogram()
ggarrange(a, b + rremove("x.text"), 
          labels = c("BMI before log transform", "BMI after log transform"),
          ncol = 2, nrow = 1)



###########################################
##Provide Logistic Regression
library(tidyverse)
library(caret)
# Split the data into training and test set
set.seed(123)
training.samples <- new_data2$HeartDiseaseorAttack.f %>% 
  createDataPartition(p = 0.8, list = FALSE)
train.data  <- new_data2[training.samples, ]
test.data <- new_data2[-training.samples, ]

library(MASS)
# Fit the full model and used step AIC for providing stepwise regression
model <-  glm(HeartDiseaseorAttack.f~AnyHealthcare.f+BMI.L+Education.f+HighBP.f+HighChol.f+Income.f+PhysActivity.f+Sex.f+Smoker.f,  data = train.data, family = binomial) %>%
  stepAIC(trace = FALSE)
sjPlot:: tab_model(model)
# Summarize the final selected model
summary(model)
# Make predictions
probabilities <- model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
# Model accuracy
mean(predicted.classes==test.data$HeartDiseaseorAttack.f)
##Check the full logistic regression model
##Full logistic regression model
full.model <- glm(HeartDiseaseorAttack.f~AnyHealthcare.f+BMI.L+Education.f+HighBP.f+HighChol.f+Income.f+PhysActivity.f+Sex.f+Smoker.f,  data = train.data, family = binomial) 
coef(full.model)
## Make predictions for full.model
probabilities <- full.model %>% predict(test.data, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "1", "0")
# Prediction accuracy
observed.classes <- test.data$HeartDiseaseorAttack.f
mean(predicted.classes == observed.classes)

##So, the stepwise selection reduced the complexity of the model without 
##compromising its accuracy.

##Check multicollinearity
library(car)
vif(model)

##Confusion matrix##Consider 0.1 as treshold, because less amount of category one in outcome
predicted<-ifelse(predict(model, test.data, type = "response")<.1, 0, 1)
confusion_matrix <- table(predicted, test.data$HeartDiseaseorAttack.f, dnn = c("Predicted outcome","Observed"))
confusion_matrix

# true positive rate
TPR<-confusion_matrix[1,1]/(confusion_matrix[1,1]+confusion_matrix[2,1])
TPR
# false positive rate:
FPR<-confusion_matrix[1,2]/(confusion_matrix[1,2]+confusion_matrix[2,2])
FPR

##Roc curve
library(pROC)
# Compute ROC
res.roc <- roc(test.data$HeartDiseaseorAttack.f,predicted)
plot.roc(res.roc, print.auc = TRUE,col="red")

############################################################
##KNN classification Method
## Fit the model on the training set/
##trControl, to set up 10-fold cross validation
##preProcess, to normalize the data
##tuneLength, to specify the number of possible k values to evaluate
set.seed(123)
model <- train(
  HeartDiseaseorAttack.f ~., data = train.data, method = "knn",
  trControl = trainControl("cv", number = 10),
  preProcess = c("center","scale"),
  tuneLength = 50
)
# Plot model accuracy vs different values of k
plot(model)

# Print the best tuning parameter k that
# maximizes model accuracy
model$bestTune
# Make predictions on the test data
predicted.classes <- model %>% predict(test.data)
head(predicted.classes)

# Compute model accuracy rate
mean(predicted.classes == test.data$HeartDiseaseorAttack.f)

k_43 <- knn(train.data, test.data, train_labels, k = 43)
table(k_43, test.data$HeartDiseaseorAttack)

##Clustering Analysis
new_data3 <- subset(new_data1, select= c("HeartDiseaseorAttack","AnyHealthcare","BMI","Education","HighBP","HighChol","Income","PhysActivity","Sex","Smoker"))

library(dplyr)
library(fpc)
require(factoextra)
standardize <- function(x){(x-min(x))/(max(x)-min(x))}
#norm_method is minmax or scalessss
clustering <- function(new_data3,distance_metric,num_clusts,norm_method){
  ifelse(norm_method=="minmax",new_dat <- standardize(new_data3),new_dat <- scale(new_data3))
  first.complete <- hclust(dist(new_data3,method=distance_metric),method="complete")
  plot(first.complete)
  hc <- cutree(first.complete,k=num_clusts)
  plot(hc)
  subs <- new_data1 %>% mutate(cluster=hc)
  show(fviz_cluster(list(data=new_data1,cluster=hc),labelsize=0,main=paste('Hierarchical Cluster, Distance =', distance_metric,sep=" ")))
  subs %>% group_by(cluster,HeartDiseaseorAttack) %>% summarise(count=n())
}
clustering(new_data3,"binary",2,"minmax")
cluster_1.prop <- 35/1435 #found through the table output of the function
cluster_2.prop <- 193/1035
cluster_1.prop
cluster_2.prop 
set.seed(0)
n=10000
sample_prop =  rep(NA,n)
for(i in 1:n){sample_prop[i]=mean(sample(new_data3$HeartDiseaseorAttack,size=1435))}
mean(sample_prop)
t.test(cluster_1.prop,sample_prop,var.equal=TRUE)
sample2_prop =  rep(NA,n)
for(i in 1:n){sample2_prop[i]=mean(sample(new_data3$HeartDiseaseorAttack,size=1035))}
mean(sample2_prop)
t.test(cluster_2.prop,sample2_prop,var.equal=TRUE)

clustering(new_data3,"euclidean",2,"minmax")
cluster_3.prop <- 225/2463 #the proportion for cluster 1 of hierarchical clustering with euclidean distance
cluster_4.prop <- 3/37 #the proportion of cluster 2 the of hierarchical clustering with binary distance
sample3_prop =  rep(NA,n)
for(i in 1:n){sample3_prop[i]=mean(sample(new_data3$HeartDiseaseorAttack,size=2463))}
mean(sample3_prop)
t.test(cluster_3.prop,sample3_prop,var.equal=TRUE)

sample4_prop =  rep(NA,n)
for(i in 1:n){sample4_prop[i]=mean(sample(new_data3$HeartDiseaseorAttack,size=37))}
mean(sample4_prop)
t.test(cluster_4.prop,sample4_prop,var.equal=TRUE)


km.res <- kmeans(new_data3,2,iter.max=10,nstart=1)
km.res$size
fviz_cluster(list(data=new_data3,cluster=km.res$cluster),ellipse=TRUE,labelsize=0,main="K-means clustering")
aggregate(new_data3,by=list(cluster=km.res$cluster),mean)
km.dat <- new_data3 %>% mutate(cluster=km.res$cluster)
km.dat %>% group_by(cluster,HeartDiseaseorAttack) %>% summarise(count=n())
kmcluster_1.prop <- 154/2110
kmcluster_2.prop <- 74/390

kmsample1_prop =  rep(NA,n)
for(i in 1:n){kmsample1_prop[i]=mean(sample(new_data3$HeartDiseaseorAttack,size=2110))}
mean(kmsample1_prop)
t.test(kmcluster_1.prop,kmsample1_prop,var.equal=TRUE)

kmsample1_prop =  rep(NA,n)
for(i in 1:n){kmsample1_prop[i]=mean(sample(new_data3$HeartDiseaseorAttack,size=2110))}
mean(kmsample1_prop)
t.test(kmcluster_1.prop,kmsample1_prop,var.equal=TRUE)


kmsample2_prop =  rep(NA,n)
for(i in 1:n){kmsample2_prop[i]=mean(sample(new_data3$HeartDiseaseorAttack,size=390))}
mean(kmsample2_prop)
t.test(kmcluster_2.prop,kmsample2_prop,var.equal=TRUE)
