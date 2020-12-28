


Zoo_data <- read.csv("C:/RAVI/Data science/Assignments/Module 18 KNN/KNN Assignment2 dataset/Zoo.csv/Zoo.csv")
View(Zoo_data)
attach(Zoo_data)

# drop the animal.name feature
Zoo_data1 <- Zoo_data[ ,2:18]
View(Zoo_data1)
str(Zoo_data1)

table(Zoo_data1$type)

summary(Zoo_data)

summary(Zoo_data[c("feathers","toothed","domestic","breathes","tail")])

head(Zoo_data)
str(Zoo_data)

#Data Visualization
install.packages('ggplot2')  #for Data Visualization
library(ggplot2)

plot(Zoo_data1)

install.packages('corrplot') #Correlation Plot
library(corrplot)
corrplot(cor(Zoo_data1))

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

Zoo_data1_n <- as.data.frame(lapply(Zoo_data1[1:16], normalize))
Zoo_data1_n

summary(Zoo_data1_n$aquatic)

# create training and test datasets

#random sampling
n  <- nrow(Zoo_data1_n)
n1 <- n*0.8
n1
n2 <- n-n1
n2

train_index  <- sample(1:n,n1)

zoo_train <- Zoo_data1[train_index, ]
zoo_test  <- Zoo_data1[-train_index, ]

  #Creating seperate dataframe for 'Type' feature which is our target.
  
  zoo_train_labels <- Zoo_data1[train_index,17]
  zoo_test_labels <- Zoo_data1[-train_index,17]

#---- Training a model on the data ----

#Find the number of observation
NROW(zoo_train_labels)
sqrt(80)    # k=9

# load the "class" library
install.packages("class")  ##KNN 
library(class)

zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                       cl = zoo_train_labels, k=1)
zoo_test_pred

#Error in prediction
error <- mean(zoo_test_pred!=zoo_test_labels)
error

install.packages('caret')
library(caret)


##--------Evaluating model performance ----


#Calculate the proportion of correct classification for k = 1
# Check prediction against actual value in tabular form for k=1
table(zoo_test_pred ,zoo_test_labels)
confusionMatrix(table(zoo_test_pred, zoo_test_labels))    
  

  
zoo_test_pred <- NULL
error_rate <- NULL

for (i in 1:15) {
  zoo_test_pred <- knn(train = zoo_train, test = zoo_test,cl = zoo_train_labels,k=i)
  error_rate[i] <- mean(zoo_test_pred!=zoo_test_labels)
}

knn_error <- as.data.frame(cbind(k=1:15,error_type =error_rate))

#K Value by Visualization
install.packages('ggplot2')  #for Data Visualization
library(ggplot2)
ggplot(knn_error,aes(k,error_type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:15)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')


zoo_test_pred <- knn(train = zoo_train, test = zoo_test,
                       cl = zoo_train_labels, k=2)
zoo_test_predOO

#Error in prediction
error <- mean(zoo_test_pred!=zoo_test_labels)
error
confusionMatrix(table(zoo_test_pred,zoo_test_labels))

#########################################
library(gmodels)
CrossTable(x = zoo_test_labels, y = zoo_test_pred,
           prop.chisq=FALSE)


