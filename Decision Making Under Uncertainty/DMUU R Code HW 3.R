##Nate Kaduk
##DMUU HW 3
##Professor Sarver

##Establish important variables
negativeSideProb <- 0.1
amountOfPatients <- 1:1000

#The mean is the expected value, and is found by the sum of the probability of
#Negative side effects for each patient.
currentExp <- 0
totalExp <- 0
for (i in amountOfPatients){
  currentExp <- 1 * negativeSideProb
  totalExp <- totalExp + currentExp
}
totalExp

sum <- 0
currentValue <- 0
i <- 0

for(i in amountOfPatients){
  currentProb <- choose(n=length(amountOfPatients), k = i) * (negativeSideProb^i)*(1-negativeSideProb)^(length(amountOfPatients)-i)
  currentValue <- ((i-totalExp)^2)*currentProb
  sum <- sum + currentValue
  
}
sum
print("Standard Deviation: ")
sqrt(sum)

print("Probablity that at most 100 people will experince side effects:")
pbinom(100, 1000, 0.1)

normApprox <- pnorm(100.5, mean =100, sd=sqrt(90))
normApprox



#2
#Establish variables
total = 20
red = 12
green = 8

#With replacement, the probability is constant
#As such, it is the probability of one trial being a success to the power
#of the number of trials
redProduct =(red/total)^5
#Output
print("Probability of red on the first try for five trials:")
redProduct

#The expected value of circumstance is 1 over over the probability of that circumstance
expectedRedTrials <- 1/redProduct
#Output
print("Expected number of trials for all five balls to be red: ")
expectedRedTrials


#Plugging in needed values for standard deviation
varRed <- (1-redProduct)/(redProduct^2)
sdRed <- sqrt(varRed)
#Output
print("Standard deviation for all five balls being red: ")
sdRed



#3
#Establish needed variables
mean <- 100
standardDev <- 15

#pnorm is P(x less than or equal to x)
#As such, x-1 is needed in the pnorm function
chanceOflt120 = pnorm(120,mean, standardDev)
chanceOflt120

#Between is inclusive
betweenIQ = pnorm(130, mean, standardDev) - pnorm(110, mean, standardDev)
betweenIQ

#Find the decimal percentage by subtracting all below 140 from 1 (100%)
atMostIQ = 1 - pnorm(140, mean, standardDev)
atMostIQ
#Multiply that answer by 100 to give percentage.
amIQPer = 100*atMostIQ
amIQPer

#4
#Establish n and the counter for the sum being less than 8.
n = 100000
sumCount <- 0
#Loop through n times
for (i in 1:n){
  #Find a newly-generated values and sum them
  currentValue <- sum(rgeom(5, .6))
  #If that sum is less than 8, increment the sum counter
  if(currentValue < 8){
    sumCount <- sumCount + 1
  }
}

#Output Sum
print("Total Sum:")
sumCount

#Output Ratio
print("Ratio:")
sumCount/n

#Output binomial being less than or equal to 7
print("Negative binomial for the sum being less than 7: ")
pnbinom(7, 5, .6)

#The negative binomial and the ratio of the geometric sum are very similar.
#Both list the same probability for the sum to the same three decimal places (.942)

#5
#Make variable for total population
totalPop <- 1000000
#Fill 32% of the total population with "graduate" and the rest with "not graduate"
gradRep <- c(rep("Graduate!", 0.32 * totalPop), rep("Not Graduate", 0.68 * totalPop))
gradRep

#Make a size for the sample
sampleSize <- 1000
#Take sample of size sampleSize(1000) from the main vector
gradSample <- sample(gradRep, sampleSize)
gradSample

#Find how many of the elements of the sample are graduates
pointEstimate <- sum(gradSample == "Graduate!")/sampleSize
pointEstimate

#Find alpha using the confidence of 99%
confidence <- .99
alpha <- 1- confidence
alphaDiv2 <- alpha/2

#Make needed variables
z<- qnorm(1-alphaDiv2, 0, 1)
z
conInt99 <- z
phat <- 0.32
#More variables for ease of use
conIntNum <- phat *(1-phat)
sE <- sqrt(conIntNum/sampleSize)
#Calculate the values to the right of the plus-minus
plusMinusNum <- conInt99 * sE
#Fill in the plus/minus values into the top/bottom portions respectively 
bottomPor <- phat - plusMinusNum
topPor <- phat + plusMinusNum

#Print out the proportions
print("The proportion is between: ")
bottomPor
print("and")
topPor

#For the first time it runs I need to include it
#library(epitools)

#Output the proportions using the epitools binom.approx function
binom.approx(totalPop*.32, totalPop, .99)

