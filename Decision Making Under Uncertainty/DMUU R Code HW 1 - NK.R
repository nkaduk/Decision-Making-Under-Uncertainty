##Nate Kaduk
##Decision-Making Under Uncertainty HW 1
##Dr. Sarver
##Due Feb 1, 2022

a <- seq(5, 200, by=5)
print("This is a:")
print (a)

aIncrement <- 5
totalA <- 0
while (aIncrement <= 200){
  aIncrement <- aIncrement + 5
  totalA <- totalA + 1
}

print("Total number of values in a:")
print(totalA)

q2Tenth <- a[10]
q2Nineteenth <- a[19]
q2TwentySecond <- a[22]
print("The tenth, nineteenth and twenty-second values in a (In that order):")
print(q2Tenth)
print(q2Nineteenth)
print(q2TwentySecond)


pointOneVector <-a * 0.1
print("Vector that is one-tenth of a:")
print(pointOneVector)


odd <- a %% 2 == 1
print("Only odd values of a: ")
a[odd]


onlyEven <- a%%2 == 0
print("Total sum of evens in a: ") 
sum(a[onlyEven])


divAThree <- a%%3 == 0
print("Only elements of A that are divisible by 3: ")
a[divAThree]

matrixVector <- c(5:13)
A <- matrix(matrixVector, nrow = 3)
print("This is A: ")
A
print("Second Row: ")
A[2,]
print("Third Column: ")
A[,3]

aTranspose <- matrix(matrixVector, nrow = 3)
aTranspose[1,] <- A[, 1]
aTranspose[2,] <- A[, 2]
aTranspose[3,] <- A[, 3]

A
aTranspose


vectorB <- diag(A)
B <- matrix(0, nrow = nrow(A), ncol = ncol(A))
i <- 1
while (i <= length(vectorB)){
  B[i,i] <- vectorB[i]
  i <- i + 1
}
print ("Matrix B: ")
B


solvedB <- solve(B)
solvedB


eMatrix1 <- cbind(A, c(2, 1, 5))
eMatrix2 <- rbind(A, c(0.3, -1.1, 3.5))
print("A followed by A with extra column followed by A with extra row: ")
A
eMatrix1
eMatrix2

