# Anscombe's quartet

attach(anscombe)

par(mfrow=c(2,2))
plot(x1, y1, xlim=c(3,20), ylim=c(3,13), col="darkorange")
plot(x2, y2, xlim=c(3,20), ylim=c(3,13), col="darkorange")
plot(x3, y3, xlim=c(3,20), ylim=c(3,13), col="darkorange")
plot(x4, y4, xlim=c(3,20), ylim=c(3,13), col="darkorange")

fit1 <- lm(y1 ~ x1)
fit2 <- lm(y2 ~ x2)
fit3 <- lm(y3 ~ x3)
fit4 <- lm(y4 ~ x4)

summary(fit1)
names(fit1)

par(mfrow=c(2,2))
plot(x1, y1, xlim=c(3,20), ylim=c(3,13), col="darkorange")
abline(fit1$coef, col="blue")
plot(x2, y2, xlim=c(3,20), ylim=c(3,13), col="darkorange")
abline(fit2$coef, col="blue")
plot(x3, y3, xlim=c(3,20), ylim=c(3,13), col="darkorange")
abline(fit3$coef, col="blue")
plot(x4, y4, xlim=c(3,20), ylim=c(3,13), col="darkorange")
abline(fit4$coef, col="blue")

detach(anscombe)

