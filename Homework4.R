#4.
library(MASS)
library(gdata)

dt <- read.xls("/Users/Yiyang/Documents/CSC 424/BondRating.xls", sheet = "training", header = TRUE)
dt
head(dt)
names(dt) <- lapply(dt[1, ], as.character)
dt <- dt[-1,] 
dt
dt1 <- apply(dt[4: 13], 2, as.numeric)
dt1

df <- data.frame(dt[1: 3], dt1)
df

brLda <- lda(CODERTG ~ LOPMAR + LFIXCHAR + LGEARRAT + LTDCAP + LLEVER + LCASHLTD + LACIDRAT + LCURRAT + LRECTURN + LASSLTD, data = df)
brLda

p <- predict(brLda, newdata = df[,4:13])$class
p

table(p, df$CODERTG)

#5.
kdf <- read.table("/Users/Yiyang/Documents/CSC 424/kellog.dat", header = FALSE, skip = 2)
head(kdf)

d <- dist(kdf[, 2: 11])
d

#MDS
fit <- cmdscale(d, eig = TRUE, k = 2)
fit

x <- fit$points[, 1]
y <- fit$points[, 2]
plot(x, y, type="n")
text(x, y, labels = row.names(kdf), cex=.7)

#isoMDS
mds <- isoMDS(d)
mds
x1 <- mds$points[, 1]
y1 <- mds$points[, 2]
plot(x1, y1, type = "n")
text(x1, y1, labels = row.names(kdf), cex=.7)
