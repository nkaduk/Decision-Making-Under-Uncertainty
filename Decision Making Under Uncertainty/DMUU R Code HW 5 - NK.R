#Nate Kaduk
#DMUU HW 5
#Professor Sarver
#4/12/2022


############################################
#1
setwd("C:/Users/Nkadu/OneDrive/Desktop/R Programming")

#H0: To a fighter's knockout ratio, their stance is irrelevant (Since the means will be similar)
#Ha: A fighter's stance plays a role in their knockout ratio

#Read file
fighterStance <- read.csv("fighter_stance_tko.csv")

#Create an ANOVA for the fighter's tko win rate in relation to their stance
fsKO <- aov(tko_win_ratio~stance, data = fighterStance)
#Print out the summary for the fighter's stance
print("Summary of fighter's tko win rate to their stance: ")
summary(fsKO)

#Since the p-value is greater than 0.05, I can conclude that there is evidence
#to show that a fighter's stance does not play a role in their knockout ratio

#2
#H0: Diets A, B, and C do not play a significant role in weight loss.
#Ha: There exists a significant difference between the weight lost in diets A, B, or C
library("car")

#Read file
Diet.data <- read.csv("Diet.csv")
#Print out Summary of the data read
print("Summary of the Data:")
summary(Diet.data)

#Create a vector that details the amount of weight lost
weightLost = Diet.data[[3]] - Diet.data[[4]]


#Add the weight lost column values to Diet.data
Diet.data[5] <- weightLost
#Change the name of the weight loss column so it can be referenced later
colnames(Diet.data)[which(names(Diet.data)=="V5")]<-"weightLoss"
Diet.data

#Create an ANOVA for the weight loss compared to both diet and gender
#Use the main Diet data for the ANOVA's data
model <- aov(weightLoss ~Diet*gender, data = Diet.data)
#Print summary of the ANOVA
print("ANOVA Summary: ")
summary(model)



#Hypotheses for Levene Test
#H0: The variance of the weight lost between diet and gender are the same
#Ha: Weight lost with respect to diet and gender have different variances.

#Create Levene Variance Test based on the weight lost from the diet type
#Use the primary Diet.data
ltResult <- leveneTest(weightLoss ~ Diet*gender, Diet.data)
#Print out results
print("Levene's Test for Variance:")
ltResult
#Levene Conclusion
#Since the p-value is large, there exists evidence that the variance of the 
#weight lost between diets and gender is the same.




#Tukey Hypotheses
#H0: All diet and gender distributions have the same mean
#Ha: At least one distribution of diet and gender has a different mean

#Create and output a difference of means based on the original model
print("Tukey Difference of Means: ")
TukeyHSD(model, conf.level = .95)

#Since the distributions of C:F-A:F and C:F-B:F have an adjusted p-value less
#than 0.05, I can say that there is not enough evidence to conclude that
#the distributions have the same mean.  These distributions also correspond to
#the biggest mean differences of 2.83 and 3.27 respectively.



#General conclusion
#Taken from the ANOVA summary, diet has a p-value less than 0.05.  Using this
#result I can reject H0 and conclude that not evidence exists to show that the weight
#lost in diets A, B, and C are the same.  Evidence shows that there is a significant 
#difference in the weight lost for diets A, B, or C.  Using the Tukey test, the difference
#in means can most likely be attributed to C:F-A:F and C:F-B:F (Diet C for females).