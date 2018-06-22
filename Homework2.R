library(foreign) 
library(corrplot)
library(car)
library(QuantPsyc)
library(leaps)
library(ggplot2)

#Homework 2
#Problem 4
#a.
M <- matrix(c(1, 1, 1, 5/2),
            nrow = 2,
            ncol = 2,
            byrow = T)
eigen(M)

A <- matrix(c(1, 3, 4, 1),
            nrow = 2,
            ncol = 2,
            byrow = T)
A

eigen(A)

#b, c.
N <- matrix(c(0.4, 0.88, -0.28, 0.88, 1.1, -0.98, -0.28, -0.98, 2.26),
            nrow = 3,
            ncol = 3,
            byrow = T)
eigen(N)

#Problem 5
#a.
employment <- read.table("/Users/Yiyang/Documents/CSC 424/homeworkData/Employment.txt", sep = "\t", header = TRUE)
employment

employmentNumbric <- employment[, c(2:10)]
employmentNumbric

employment.pr <- prcomp(employmentNumbric)
employment.pr

summary(employment.pr)
screeplot(employment.pr)
abline(1,0)
biplot(employment.pr)

#b.
library(psych)
employment.pr2 <- principal(employmentNumbric, rotate = "varimax", nfactors = 2, score = TRUE)
print(employment.pr2$loadings, cutoff = 0.4, sort = TRUE)

biplot(employment.pr)

#c.
scores <- as.matrix(employmentNumbric) %*% as.matrix(employment.pr$rotation)
print(scores)
max(scores[, 2])
min(scores[, 2])

#d.
library(psych)
options("scipen" = 100, "digits" = 5)
round(cor(employmentNumbric), 2)
corrTest = corr.test(employmentNumbric, adjust = "none")
corrTest

E = corrTest$p
E

ETest = ifelse(E < 0.1, T, F)
ETest
colSums(ETest) - 1

employmentNumbric[2: 9]

employment.pr3 <- prcomp(employmentNumbric[2: 9], scale = TRUE)
employment.pr3
summary(employment.pr3)

employment.pr4 <- principal(employmentNumbric[2: 9], rotate = "varimax", nfactors = 5, score = TRUE)
print(employment.pr4$loadings, cutoff = 0.4, sort = TRUE)


#Problem 6
census <- read.csv("/Users/Yiyang/Documents/CSC 424/homeworkData/Census2.csv", sep = ",", header = TRUE)
census

census.pr <- prcomp(census)
census.pr

summary(census.pr)
screeplot(census.pr)
biplot(census.pr)

censusEdit <- census[, c(1:4)]
censusEdit$MedianHomeVal <- census$MedianHomeVal/(100000)
censusEdit

census.pr2 <- prcomp(censusEdit)
census.pr2
summary(census.pr2)

census.pr3 <- prcomp(census, scale = TRUE)
census.pr3
summary(census.pr3)

#Problem 7
track <- read.table("/Users/Yiyang/Documents/CSC 424/homeworkData/trackRecord.txt", header = TRUE)
trackNumberic <- track[2: 9]
trackNumberic

library(psych)
options("scipen" = 100, "digits" = 5)
round(cor(trackNumberic), 2)
tcorrTest = corr.test(trackNumberic, adjust = "none")
tcorrTest

Tr = tcorrTest$p
Tr

TTest = ifelse(Tr < 0.1, T, F)
TTest
colSums(TTest) - 1


track.pr <- prcomp(trackNumberic)
track.pr
summary(track.pr)

track.pr2 <- prcomp(trackNumberic, scale = TRUE)
summary(track.pr2)

trackEdit <- trackNumberic[1: 3]
trackEdit$m800_Sec <- trackNumberic$m800*(60)
trackEdit$m1500_Sec <- trackNumberic$m1500*(60)
trackEdit$m5000_Sec <- trackNumberic$m5000*(60)
trackEdit$m10000_Sec <- trackNumberic$m10000*(60)
trackEdit$Marathon <- trackNumberic$Marathon
trackEdit

trackEdit.pr <- prcomp(trackEdit)
summary(trackEdit.pr)

library(psych)
track.pr3 <- principal(trackNumberic, rotate = "varimax", nfactors = 8, score = TRUE)
print(track.pr3$loadings, cutoff = 0.4, sort = TRUE)

track.fc <- factanal(trackNumberic, 4)
print(track.fc$loadings, cutoff = 0.4, sort = TRUE)
