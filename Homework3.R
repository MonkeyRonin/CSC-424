ds <- read.csv("/Users/Yiyang/Documents/CSC 424/Final Dataset/Dataset.csv", sep = ",", header = TRUE)

head(ds)

dm <- ds[, c(2: 9, 11)]
head(dm)

library(GMD)
dist.obj <- dist(dm)
hclust.obj <- hclust(dist.obj)
css.obj <- css.hclust(dist.obj,hclust.obj)
elbow.obj <- elbow.batch(css.obj)
print(elbow.obj)

#PCA
#princomp
dm.pr <- princomp(dm)
summary(dm.pr)

#prcomp
dm.pr1 <- prcomp(dm, center = TRUE)
dm.pr1
summary(dm.pr1)

library(MASS)
fit <- lm()

dm.pr2 <- prcomp(dm, center = TRUE, scale = TRUE)
dm.pr2
summary(dm.pr2)

pc <- principal(dm, nfactors = 2)
pc

pc1 <- principal(dm, nfactors = 2, rotate = "varimax")
pc1
#CFA
library(psych)
dm.cf <- principal(dm, rotate = "varimax", nfactors = 2, score = TRUE)
print(dm.cf$loadings, cutoff = 0.4, sort = TRUE)


