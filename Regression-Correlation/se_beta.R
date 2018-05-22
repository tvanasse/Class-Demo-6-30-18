#------generate one data set with epsilon ~ N(0, 0.25)------
seed <- 1152 #seed
n <- 100     #nb of observations
a <- 5       #intercept
b <- 2.7     #slope

set.seed(seed)
epsilon <- rnorm(n, mean=0, sd=sqrt(0.25))
x <- sample(x=c(0, 1), size=n, replace=TRUE)
y <- a + b * x + epsilon
#-----------------------------------------------------------

#------using lm------
mod <- lm(y ~ x)
plot(x,y)
abline(mod)
#--------------------

#------using the explicit formulas------
X <- cbind(1, x)
betaHat <- solve(t(X) %*% X) %*% t(X) %*% y
var_betaHat <- anova(mod)[[3]][2] * solve(t(X) %*% X)
#---------------------------------------

#------comparison------
#estimate
mod$coef

#BetaHat[1] = intercept, BetaHat[2] = slope
c(betaHat[1], betaHat[2])


#standard error
summary(mod)$coefficients[, 2]

sqrt(diag(var_betaHat))

#----------------------
