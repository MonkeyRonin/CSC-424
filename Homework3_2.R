ccaWilks = function(set1, set2, cca)
{
  ev = ((1 - cca$cor^2))
  ev
  
  n = dim(set1)[1]
  p = length(set1)
  q = length(set2)
  k = min(p, q)
  m = n - 3/2 - (p + q)/2
  m
  
  w = rev(cumprod(rev(ev)))
  
  # initialize
  d1 = d2 = f = vector("numeric", k)
  
  for (i in 1:k) 
  {
    s = sqrt((p^2 * q^2 - 4)/(p^2 + q^2 - 5))
    si = 1/s
    d1[i] = p * q
    d2[i] = m * s - p * q/2 + 1
    r = (1 - w[i]^si)/w[i]^si
    f[i] = r * d2[i]/d1[i]
    p = p - 1
    q = q - 1
  }
  
  pv = pf(f, d1, d2, lower.tail = FALSE)
  dmat = cbind(WilksL = w, F = f, df1 = d1, df2 = d2, p = pv)
}

library(CCA)
library(psych)
library(CCP)
#Homework 3
#Problem 3
#Two Dataset
ds <- read.csv("/Users/Yiyang/Documents/CSC 424/Datasets/data_marsh_cleaned_homework2.csv", sep = ",", header = TRUE)

#x: water, y: soil
X <- ds[-1, 2: 6]
x <- apply(X, 2, as.numeric)

Y <- ds[-1, 7: 9]
y <- apply(Y, 2, as.numeric)

#a. combine all
c <- matcor(x, y)
cross <- c$XYcor[1:5, 6:8]
round(cross, 2)

#Corr test
p <- corr.p(cross, nrow(x))
p

#Canonlical correlation
cc1 <- cc(x, y)
cc1
cc1$cor

wilks1 <- ccaWilks(X, Y, cc1)
wilks1
round(wilks1, 2)

#b
Y2 <- Y[, 2: 3]
y2 <- y[, 2: 3]
y2
cc2 <- cc(x, y2)
cc2$cor

wilks2 <- ccaWilks(X, Y2, cc2)
wilks2
round(wilks2, 2)

#c
Y3 <- data.frame(TPRSDFB = Y$TPRSDFB)
Y3
y3 <- as.matrix(y[, 3])
y3
cc3 <-cc(x, y3)
cc3$cor

wilks3 <- ccaWilks(X, Y3, cc3)
wilks3
round(wilks3, 2)


#4
library(ca)
smoking <- read.csv("/Users/Yiyang/Documents/CSC 424/Smoking.csv", sep = ',', header = TRUE)
colnames(smoking) <- c("Staff Group", "None", "Light", "Medium", "Heavy", "Row Total")
cSmoking = ca(smoking[1:5, 2:5])
cSmoking

plot(cSmoking, mass = T, contrib = "absolute", map = "rowgreen", arrows = c(F, T))



