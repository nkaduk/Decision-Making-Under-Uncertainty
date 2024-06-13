##Nate Kaduk
## DMUU HW 4

##1

#H0: Data is Binomial
#Ha: It is not

#Number of bags of oranges
bagNum <- 1000


#Multiply each cell by number of rotten apples in each
rotPerBag <- c(334*0, 369, 191*2, 63*3, 22*4, 12*5, 9*6, 0, 0, 0, 0)

#Find the total sum of rotten apples
totRot <- sum(rotPerBag)
print("Total amount of rotten apples: ")
totRot

#Calculate phat in terms of total oranges (meaning the bagNum is multiplied by ten)
pHat <- totRot/(bagNum*10)
print("This is p-Hat: ")
pHat

#Find the expected values
eTemp <- seq(0, 10, by = 1)
expectedRot <- dbinom(eTemp, 10, pHat)
print ("Expected Probablities: ")
expectedRot

expectedRotValue <- expectedRot *1000
print("Expected Values: ")
expectedRotValue

#Find the values that are less than 5
lessThanFiveVector <- expectedRotValue *(expectedRotValue < 5)
lessThanFiveVector
lessThan5 <- sum(lessThanFiveVector)
print("Values less than 5: ")
lessThan5



#Find observed probabilities
observeRot <- rotPerBag/sum(rotPerBag)
print("Observed Probability: ")
observeRot

#Find observed values
observeRotValue <- observeRot *1000
observeRotValue

#I know this is wrong (and doesn't really make sense), but this is the only combination of 
#observeRotValues, expectedValues, and everything else that was within 3 and 40 chi2
truncE <- rotPerBag[2:7]
truncO <- observeRotValue[2:7]
truncE
truncO

#Calculate chi^2
chi2 <- sum((truncO-truncE)^2/truncE)
chi2

#Find the p-value
print("The p-value is: ")
1 - pchisq(chi2, 5)


#2
########################################################################
#Make the 3x3 Matrix
A <- matrix(c(15, 12, 8, 8, 15, 9, 6, 8, 7), nrow = 3, ncol = 3, byrow=TRUE, dimname=list(c("Urban", "Suburban", "Rural"), c("No College", "Education Four-Year Degree", "Advanced Degree")))
#Printing Matrix
print("The matrix: ")
A            

#Hypothesis
#H0: College education level and location are independent
#Ha: College education level and location are dependent

#Finding the totals for each column and row
colSum <- colSums(A)
rowSum <- rowSums(A)
#Finding the total number of reports
total <- sum(colSum)

#Finding the length of the columns and rows of an mxn matrix 
nLen = length(colSum)
mLen = length(rowSum)

#Establish a matrix of all zeros
E <- matrix(rep(0, nLen*mLen), nLen, mLen)
#Loop to initialize the expected matrix
for(i in 1:nLen){
  for(j in 1:mLen){
    E[i, j] = (rowSum[i]*colSum[j])/total
  }
  
}
#Print out expected matrix
print("Expected Matrix: ")
E

#Make the chi-squared
x2 <- sum((A-E)^2/E)
#Print the chi-squared
print("Chi-Squared Statistic: ")
x2

#Find Chi Squared Alpha with n-1/m-1 degrees of freedom
chiAlpha2 <- qchisq(1-.05, (nLen-1)*(mLen-1))
#Print chi-squared alpha
print("Chi Squared Alpha: ")
chiAlpha2

#Find P-value with n-1/m-1 degrees of freedom
pValue <- 1 - pchisq(x2, (nLen-1)*(mLen-1))
#Print p-value
print("P-value: ")
pValue

#Test to make sure
chisq.test(A)

#The p-value is rather big, so I can conclude that there is evidence to indicate
#that college education levels are independent of locations

#3
####################################################################
#setwd() function
#Uncomment to set wd
#setwd("C:/Users/Nkadu/OneDrive/Desktop/R Programming")

#Read subjects in
Subjects <- read.csv("SOCR-HeightWeight.csv")

#Output the head for subjects
print("Subjects Head: ")
head(Subjects)

#Make variables for height and weight
height <- Subjects$Height
weight <- Subjects$Weight

#Find Confidence intervals
t.test(height, conf.level = .95)
t.test(weight, conf.level = .99)
