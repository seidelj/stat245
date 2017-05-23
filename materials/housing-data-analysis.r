# housing data analysis


#################
# download NewHaven.txt from chalk to your own computer



setwd("/Users/joseph/stat245/week8/q7")
# set up your own working directory, with NewHaven.txt in the same place

library(YaleToolkit)
# you may want to install this nice package

x <- dget("NewHaven.txt")
# read data

whatis(x)
dim(x)
names(x)
head(x)

plot(x$Lon, x$Lat, pch='.')

plot(x$LivingArea, x$CurVal, col="darkorange")
#three strange points


grep("13 PLEASANT", x[,2])
# find the row of the place where I lived!


plot(x$LivingArea, x$CurVal, col="darkorange")
# three obvious outliers

x <- x[x$house==TRUE,]
# only look at houses

plot(x$LivingArea, x$CurVal, col="darkorange")
fit1 <- lm(CurVal~LivingArea, data=x)
abline(fit1$coef, col="blue")





fit2 <- lm(CurVal ~ LivingArea+TotalBedrooms+TotalBathrooms+Depreciation+Year+Garage, data=x)
summary(fit2)

# check the number of bedrooms!!

fit3 <- lm(CurVal ~ TotalBedrooms+TotalBathrooms+Depreciation+Year+Garage, data=x)
summary(fit3)


fit4 <- lm(CurVal ~ TotalBedrooms+Depreciation+Year+Garage, data=x)
summary(fit4)


cook = cooks.distance(fit2)
plot(cook)
# outlier detection
x[cook > 0.04,]


# study condo
y <- dget("NewHaven.txt")
y <- y[y$condo==TRUE,]


fit5 <- lm(CurVal ~ LivingArea+TotalBedrooms+TotalBathrooms+Depreciation+Year+Garage, data=y)
summary(fit5)
cook = cooks.distance(fit5)
plot(cook)
y[cook > 1,]

y <- y[cook < 1,]
# exclude the outlier and refit the model
fit6 <- lm(CurVal ~ LivingArea+TotalBedrooms+TotalBathrooms+Depreciation+Year+Garage, data=y)
summary(fit6)

# Year is interesting, plot it
plot(y$Year, y$CurVal, col="darkorange")
# a two million condo?

y <- y[y$CurVal < 1000000,]
fit7 <- lm(CurVal ~ LivingArea+TotalBedrooms+TotalBathrooms+Depreciation+Year+Garage, data=y)
summary(fit7)



