##Nate Kaduk
##Decision-Making Under Uncertainty HW 2
##Dr. Sarver
##Due Feb 18

##1
##Set seed and n
set.seed(1)
n <- 1000
##Generate random numbers
x <- runif(n, 0, 1)

##Answer questions in order
xMean <- mean(x)
print("Mean of x: ")
xMean

xVar <- var(x)
print("Variance of x: ")
print(xVar)

cs <- cumsum(x)
print("Culmative sum of x: ")
print(cs)

##Plot
plot(1:n, cs/(1:n), type = "l")
abline(h=0.5)


##2
##Define the density function
funct <- function(xPara){
  if(xPara <= 1 && xPara >= 0){
    return (1)
  } else{
    return (0)
  }
  
}

##Create the section inside the integral as a function
meanGreek <- function(x){x*funct(x)}
##Integrate that function on the bounds of negative infinity to positive infinity
actualMean <- integrate(meanGreek,-Inf, Inf)
##Print result
print("The mean is: ")
actualMean

##Turn the value of the mean to a numeric so it can be used later
numMean <- as.numeric(actualMean[1])

##Function for standard deviation
sdTest <- function(x){funct(x)*(x-numMean)^2}
##Integrate on same boundary
standardDev <-integrate(sdTest, 0, 1)
##Get numeric value that can be square rooted
numericVarValue <- as.numeric(standardDev[1])

## Square root the numeric value and combine it with the absolute error
variance <- list(sqrt(numericVarValue), standardDev[2])
##print result
print("The variance is: ")
variance

##3
##Create needed variables
n <- 1000000
x <- runif(n, -1, 1)
y <- runif(n, -1, 1)

##Due to the equation having the form x^2 + y^2, it is a circle.

##Create vector of bool values corresponding to the area inside the circle
satPoints <-(x^2+y^2 < 1)
##Find the total number of points within the circle
satPointsArea <- sum(satPoints)
##Print out # of points
print("# of points: ")
satPointsArea

##Find the ratio
ratio <- satPointsArea / n
##Output
print("Number of points within circle vs total number of points: ")
ratio

##Multiply ratio by 4
partCAnswer <- ratio * 4
##Output result
print("Output: ")
partCAnswer
##This is also an answer similar to pi's 3.14


##4
##Establish variables
n<-100000
x<- runif(n, -1, 1)
y <- runif(n, -1, 1)
z <- runif(n, -1, 1)

##Bool vector for points that match the needed equation
boolInFigure <- (x^2 + y^4 + z^6 < 1)
##Find the total number of points
totalPointsInFigure <- sum(boolInFigure)
##Output
print("Total points: ")
totalPointsInFigure

##Find the volume by finding the ratio, then output
figureVolume <- totalPointsInFigure/n
print("Figure volume: ")
figureVolume


##5
##Establish n
n <- 10000

##Create segment variables that will be used to store lengths of each line
##part with each iteration
firstSegment <- 0
secondSegment <- 0
thirdSegment <- 0

##Create bool counter that will count the amount of iterations that could form a triangle
boolIter <- 0

##Main loop
for(t in 1:n){
  ##Generate two random points between 0 and 1
  x <- runif(2, 0, 1)
  ##If the first segment has a greater value than the second, arrange it one way,
  ##Otherwise, arrange it another
  if(x[1] > x[2]){
    ##Since x[2] is less, it is the first point.  The first segment must be
    ##Between 0 and the first point.
    firstSegment <- x[2]
    ##The larger point, in this case x[1] is from 0 to x[1].
    ##To find the value of just x[2] to x[1], we need to take the difference.
    secondSegment <- x[1] - firstSegment
    ##The third segment is the entire line minus the first two segments
    thirdSegment <- 1 - (secondSegment + firstSegment)
  } else{
    ##Similar logic here
    firstSegment <- x[1]
    secondSegment <- x[2] - firstSegment
    thirdSegment <- 1 - (secondSegment + firstSegment)
  }
  ##Note: If somehow x[1] == x[2], it wouldn't have the chance to be a triangle.
  ##This is because x[2] - firstSegment(which equals x[1]) would = 0,
  ##and 0+a is not greater than a.
  
  ##Sort the segments in ascending order.
  iterSort <- sort(c(firstSegment,secondSegment, thirdSegment))
  
  ##If the smaller two segments are greater than the largest, it is a triangle.
  ##If this holds true, increment boolIter to signify that this possibility forms a triangle.
  if((iterSort[1] + iterSort[2]) > iterSort[3]){
    boolIter <- boolIter + 1
  } 
}

##Print out the ratio.
boolIter
print("Chance that the line segments can form a triangle: ")
print(boolIter/n)

