#Problem 3
library(MASS)
#Create Matrix
Z <- matrix(c(1, 5, 1, -3, 1, 2, 1, 4),
            nrow = 4,
            ncol = 2,
            byrow = T)
Y <- matrix(c(2, 1, -1, 3),
            nrow = 4,
            ncol = 1,
            byrow = T)
M <- matrix(c(20, 15, 0, 5, 25, 10, 0, 20, 5),
            nrow = 3,
            ncol = 3,
            byrow = T)
N <- matrix(c(-20, 5, 10, 0, -10, 10, 5, 20, -5),
            nrow = 3,
            ncol = 3,
            byrow = T)
v <- matrix(c(1, -1, 3),
            nrow = 3,
            ncol = 1,
            byrow = T)
w <- matrix(c(2, 1, -1),
            nrow = 3,
            ncol = 1,
            byrow = T)
#a. v.w
crossprod(v, w)

#b. -3*w
-3 * w

#c. M * v
M %*% v

#d. M + N
M + N

#e. M - N
M - N

#f. Z(T)
Zt <- t(Z)
Zt

#g. Z(T)Z
ZtZ = Zt %*% Z
ZtZ

#h. (Z(T)Z)^(-1)
inverse <- fractions(ginv(ZtZ))
inverse

#i. Z(T)Y
ZtY <- Zt %*% Y
ZtY

#j. Beta
beta <- fractions(inverse %*% ZtY)
beta

#k. det(Z(T)Z)
detZtZ <- det(ZtZ)
detZtZ

#dataset x y
x <- c(5, -3, 2, 4)
y <- c(2, 1, -1, 3)

dataSet <- data.frame(x, y)
fit <- lm(y ~ x, data = dataSet)
summary(fit)

#Problem 4
#a.
head(mtcars)
A <- mtcars[c("cyl", "disp", "hp", "wt", "carb")]
Y <- mtcars[c("mpg")]
Y
A

#b. 
A$count <- rep(1, nrow(A))
A <- A[c(6, 1, 2, 3, 4, 5)]
A

#c.
A <- as.matrix(A)
Y <- as.matrix(Y)
A
Y

#d.
At <- t(A)
At
inverse <- fractions(ginv(At %*% A))
AtY <- At %*% Y
beta <- inverse %*% AtY
beta

#e.
dataSet <- mtcars[c("cyl", "disp", "hp", "wt", "carb", "mpg")]
model <- lm(mpg ~ cyl + disp + hp + wt + carb, data = dataSet)
summary(model)

#Problem 5
londonOlympic <- read.csv2("/Users/Yiyang/Documents/CSC 424/Homework-1-DataFiles/olympics.csv", sep = ",", header = T)
print(londonOlympic)

londonOlympic$ratio <- (londonOlympic$Female.count + londonOlympic$Male.count)/londonOlympic$X2010.population
print(londonOlympic)

modelL <- lm(Gold.medals ~ ratio, data = londonOlympic)
summary(modelL)
plot(londonOlympic$ratio, londonOlympic$Gold.medals)

#Problem 6
maple <- read.table("/Users/Yiyang/Documents/CSC 424/Homework-1-DataFiles/maple.txt", header = TRUE)
print(maple)
X1 <- maple$Latitude
X2 <- maple$JulyTemp
Y <- maple$LeafIndex

#a.
m1 <- lm(Y ~ X1)
summary(m1)

#b.
m2 <- lm(Y ~ X2)
summary(m2)

#c.
m3 <- lm(Y ~ X1 + X2)
summary(m3)
pairs(maple)

#Problem 7
chicinsur <- read.table("/Users/Yiyang/Documents/CSC 424/Homework-1-DataFiles/chicinsur.txt", header = TRUE)
print(chicinsur)

#a.
c <- data.frame(pctmin = chicinsur$pctmin, 
                fires = chicinsur$fires, 
                thefts = chicinsur$thefts, 
                pctold = chicinsur$pctold, 
                income = chicinsur$income, 
                newpol = chicinsur$newpol)
C <- cor(c)
C
library(corrplot)
corrplot(C, method = "circle")

#b.
c6 <- lm(newpol ~ fires + pctmin + thefts + pctold + income, data = chicinsur)
summary(c6)

#d.
r = residuals(c6)
p = predict(c6)

plot(p, r, main = "Residual Plot",
           xlab = "Predicted",
           ylab = "Residuals")
abline(h = 0, lty = 2)


#Problem 8
housing <- read.table("/Users/Yiyang/Documents/CSC 424/housing.data", header = FALSE)
colnames(housing) <- c("CRIM", "ZN", "INDUS", "CHAS", "NOX", "RM", "AGE", "DIS", "RAD", "TAX", "PTRATIO", "B", "LSTAT", "MEDV")
print(housing)

h <- data.frame(CRIM = housing$CRIM,
                ZN = housing$ZN,
                INDUS = housing$INDUS,
                CHAS = housing$CHAS,
                NOX = housing$NOX,
                RM = housing$RM,
                AGE = housing$AGE,
                DIS = housing$DIS,
                RAD = housing$RAD,
                TAX = housing$TAX,
                PTRATIO = housing$PTRATIO,
                B = housing$B,
                LSTAT = housing$LSTAT,
                MEDV = housing$MEDV)
H <- cor(h)
H
corrplot(H, method = "circle")

modelH <- lm(CRIM ~ ZN + INDUS + CHAS + NOX + RM + AGE + DIS + RAD + TAX + PTRATIO + B + LSTAT + MEDV, data = housing)
summary(modelH)

modelH1 <- lm(CRIM ~ RAD + LSTAT + B + MEDV + ZN + DIS + NOX + PTRATIO, data = housing)
summary(modelH1)


fwd.model <- step(lm(CRIM ~ 1, data = housing), direction = 'forward', scope = ~ ZN + INDUS + CHAS + NOX + RM + AGE
                  + DIS + RAD + TAX + PTRATIO + B + LSTAT + MEDV)
bwd.model <- step(lm(CRIM ~ ZN + INDUS + CHAS + NOX + RM + AGE
                  + DIS + RAD + TAX + PTRATIO + B + LSTAT + MEDV, data = housing), direction = 'backward', scope = ~ 1)