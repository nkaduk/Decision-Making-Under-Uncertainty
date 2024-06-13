##Nate Kaduk
#DMUU R Code HW 6
#Professor Sarver
#4/21/2022

#1
############################################################
install.packages("VGAM")
library("class")
library("VGAM")
setwd("C:\\Users\\natek\\OneDrive\\Desktop\\R Materials")

penData <- read.csv("penguins.csv")

#Scaling data for use in the future
scaledPenData <- penData
for(i in 4:7){
  scaledPenData[, i] <- scale(penData[, i])
  
}


#Randomize which values are chosen
penSplit <- sample(c(rep(0, 0.8*nrow(scaledPenData)),rep(1, 0.2*nrow(scaledPenData))))

#Create train(80%) and test(20%) data
train_pen <- scaledPenData[penSplit == 0,]
test_pen <- scaledPenData[penSplit == 1,]

#Create logistic model
logMod <- vglm(train_pen$species~ bill_length_mm + bill_depth_mm + flipper_length_mm + body_mass_g, family = "multinomial", data = train_pen)
#Output model
logMod

#Create a probability vector for the data
prob = predict(logMod, newdata = data.frame(test_pen[,4:7]), type='response')
#Make predictions for the data based on the probability that they match the species
predictions = apply(prob, 1, which.max)
#Output predictions
print("Predictions: ")
predictions

print ("Logistic regression table: ")
#Make table based on testing data and predictions
tab <-table(test_pen$species, predictions)
#Output table
tab

#Calculate and output accuracy
print("Accuracy: ")
correctPred <- sum(diag(tab))
totalPred <- sum(tab)
accuracy <- correctPred/totalPred
accuracy

#Calculate and output error rate
print("Error Rate: ")
errorRate <- 1 - accuracy
errorRate

#Calculate and output precision
print("Precision for A: ")
precA <- tab[1, 1] / sum(tab[, 1])
precA

print("Precision for B: ")
precB <- tab[2,2]/sum(tab[, 2])
precB

print("Precision for C: ")
precC <- tab[3, 3]/sum(tab[, 3])
precC

#Calculate and output recall
print("Recall for A: ")
recallA <- tab[1, 1]/sum(tab[1,])
recallA

print("Recall for B: ")
recallB <- tab[2, 2]/sum(tab[2,])
recallB

print("Recall for C: ")
recallC <- tab[3, 3]/sum(tab[3,])
recallC

#############################################################
#2

#Create train and test categories
pen_train_category <- penData[penSplit==0, "species"]
pen_test_category <- penData[penSplit==1, "species"]

#Previous scaling (I don't exactly know if I need to scale it again since now train_pen and test_pen come from 
#scaledPenData, which has already been scaled, but he is the code anyway)
# scaleTrain <- scale(train_pen[, 4:7])
# scaleTest <- scale(test_pen[, 4:7])
# 
# for(i in 4:7){
#   scaleTrain[, i-3] <- scale(train_pen[, i])
# }
# for (i in 4:7){
#   scaleTest[, i-3] <- scale(test_pen[, i])
# }

#Perform knn on train data
K = 1
#Since the train/test_pens are from scaled data, I can use it normally
Expected = knn(train_pen[, 4:7], test_pen[, 4:7], cl = pen_train_category, k=K, prob = TRUE)

#Print out table
print("K=1 Table: ")
tab = table(Expected, pen_test_category)
tab

#Calculate and output information for K=1
#Accuracy
print("Accuracy: ")
correctPred <- sum(diag(tab))
totalPred <- sum(tab)
accuracy <- correctPred/totalPred
accuracy

#Error rate
print("Error Rate: ")
errorRate <- 1 - accuracy
errorRate

#Precision
print("Precision for A: ")
precA <- tab[1, 1] / sum(tab[, 1])
precA

print("Precision for B: ")
precB <- tab[2,2]/sum(tab[, 2])
precB

print("Precision for C: ")
precC <- tab[3, 3]/sum(tab[, 3])
precC

#Recall
print("Recall for A: ")
recallA <- tab[1, 1]/sum(tab[1,])
recallA

print("Recall for B: ")
recallB <- tab[2, 2]/sum(tab[2,])
recallB

print("Recall for C: ")
recallC <- tab[3, 3]/sum(tab[3,])
recallC


print("K=5 Table: ")
K=5
Expected = knn(train_pen[, 4:7], test_pen[, 4:7], cl = pen_train_category, k=K, prob = TRUE)
tab = table(Expected, pen_test_category)
tab


#Calculate and output information for K=5
#Accuracy
print("Accuracy: ")
correctPred <- sum(diag(tab))
totalPred <- sum(tab)
accuracy <- correctPred/totalPred
accuracy

#Error rate
print("Error Rate: ")
errorRate <- 1 - accuracy
errorRate

#Precision
print("Precision for A: ")
precA <- tab[1, 1] / sum(tab[, 1])
precA

print("Precision for B: ")
precB <- tab[2,2]/sum(tab[, 2])
precB

print("Precision for C: ")
precC <- tab[3, 3]/sum(tab[, 3])
precC

#Recall
print("Recall for A: ")
recallA <- tab[1, 1]/sum(tab[1,])
recallA

print("Recall for B: ")
recallB <- tab[2, 2]/sum(tab[2,])
recallB

print("Recall for C: ")
recallC <- tab[3, 3]/sum(tab[3,])
recallC

print("K=11 Table: ")
K=11
#Create expected value
Expected = knn(train_pen[, 4:7], test_pen[, 4:7], cl = pen_train_category, k=K, prob = TRUE)
#Output table
tab = table(Expected, pen_test_category)
tab

#Calculate and output information for K=11
#Accuracy
print("Accuracy: ")
correctPred <- sum(diag(tab))
totalPred <- sum(tab)
accuracy <- correctPred/totalPred
accuracy

#Error rate
print("Error Rate: ")
errorRate <- 1 - accuracy
errorRate

#Precision
print("Precision for A: ")
precA <- tab[1, 1] / sum(tab[, 1])
precA

print("Precision for B: ")
precB <- tab[2,2]/sum(tab[, 2])
precB

print("Precision for C: ")
precC <- tab[3, 3]/sum(tab[, 3])
precC

#Recall
print("Recall for A: ")
recallA <- tab[1, 1]/sum(tab[1,])
recallA

print("Recall for B: ")
recallB <- tab[2, 2]/sum(tab[2,])
recallB

print("Recall for C: ")
recallC <- tab[3, 3]/sum(tab[3,])
recallC

#############################################################################################
#3

#I ran this several times, and it seems that knn and the logistic model are trading shots.  
#k=11 is the worst classifier, but not by much.  I have had times that it most correctly classified the data.  But in general, I think it performs just slightly worse than the others.
#As for the others one will have no errors just as the other will have one or two.  I have seen both all of the knns get it correct and the log model get it wrong and vice versa.
#I conclude that the tests for knn and the logistic model are similar.
