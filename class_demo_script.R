#Class Demo 6/30/18

#use Introductory Statistics with R (ISwR) package
library(ISwR)
library(MethComp)

attach(thuesen)

#show dataset 
#(It contains ventricular shortening velocity and blood 
# glucose for type 1 diabetic patients.)
thuesen

#scatter plot
plot(blood.glucose,short.velocity)

#Plot Line, meaning (a, b)-line, draws lines based on the 
#intercept and slope, a and b, respectively.
abline(lm.velo)

#creates "model object"
lm(short.velocity~blood.glucose)

#Extractor function
summary(lm(short.velocity~blood.glucose))

#Exclude missing data point so it doesn't cause us problems!
options(na.action=na.exclude)
lm.velo<-lm(short.velocity~blood.glucose)

#Plot line with residuals
segments(blood.glucose,fitted(lm.velo),
         + blood.glucose,short.velocity)

#Independence of errors, first plot
plot(lm.velo) 

#Are residuals normal?
hist(resid(lm.velo))
qqnorm(resid(lm.velo))

#Checking Homoscedacity function (plot 3)
plot(lm.velo)

#Checking Cook's Distance (plot 4)
plot(lm.velo)


#data frame of x-values, which will be used to predict y-values
pred.frame<-data.frame(blood.glucose=4:20)


#predict.lm produces predicted values, obtained by evaluating the regression 
#function in the frame. 
pp<- predict(lm.velo, interval='prediction', newdata=pred.frame)
pp
pc<- predict(lm.velo, interval='confidence', newdata=pred.frame)
pc

plot(blood.glucose,short.velocity,
     ylim=range(short.velocity,pp,na.rm=T))

pred.gluc<- pred.frame$blood.glucose

matlines(pred.gluc,pc,lty= c(1,2,2),
         col='blue')
matlines(pred.gluc,pp,lty= c(1,3,3),
         col='red')

#Generate random dataset
x<-runif(100,0,5)+rnorm(100)
y<-2+3*x+rnorm(100,sd=2)
Deming(x,y)
?Deming

#Ration of variances is 2, i.e. residual variance in x is 
#two times larger than residual variance in y
#vr - variance ratio

# 'True' values 
M <- runif(100,0,5)

# Measurements:
x <-         M + rnorm(100)
y <- 2 + 3 * M + rnorm(100,sd=2)
# Deming regression with equal variances,  resp. variance ratio 2.
Deming(x,y)
Deming(x,y,vr=2)
Deming(x,y,boot=TRUE)
bb <- Deming(x,y,boot=TRUE,keep.boot=TRUE)
str(bb)
# Plot data with the two classical regression lines
plot(x,y)
abline(lm(y~x))
ir <- coef(lm(x~y))
#(line that passes through origin)
abline(-ir[1]/ir[2],1/ir[2])
abline(Deming(x,y,sdr=2)[1:2],col="red")
abline(Deming(x,y,sdr=10)[1:2],col="blue")
# Comparing classical regression and "Deming extreme"
summary(lm(y~x))
Deming(x,y,vr=1000000)

thuesen

cor(blood.glucose,short.velocity)

#remove data point
cor(blood.glucose,short.velocity,use='complete.obs')

#correlation matrix
cor(thuesen,use='complete.obs')
cor.test(blood.glucose, short.velocity)

#spearman correlation
cor.test(blood.glucose,short.velocity,method="spearman")
