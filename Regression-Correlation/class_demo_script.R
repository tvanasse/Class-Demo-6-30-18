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

#creates "model object"
lm.velo <- lm(short.velocity~blood.glucose)

#Plot Line, meaning (a, b)-line, draws lines based on the 
#intercept and slope, a and b, respectively.
abline(lm.velo)

#Extractor function
summary(lm(short.velocity~blood.glucose))
#STOP

#START
#Exclude missing data point so it doesn't cause us problems!
options(na.action=na.exclude)
lm.velo<-lm(short.velocity~blood.glucose)

#Plot line with residuals
segments(blood.glucose,fitted(lm.velo),
         + blood.glucose,short.velocity)

#Independence of errors, first plot
plot(lm.velo) 

#Let's look at a different dataset, lh, in the R datasets package
#A regular time series giving the luteinizing hormone (LH) in 
#blood samples at 10 mins intervals from a human female, 48 samples.
lh
plot(lh)
acf(lh)


#now back to thuesun, are residuals normal?
hist(resid(lm.velo))
qqnorm(resid(lm.velo))

#Checking Homoscedacity function and CD (plot 3 & 4)
plot(lm.velo)

#STOP

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

#Ratio of variances is 2, i.e. residual variance in x is 
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

#Research Example
library("ggplot2")

#Create data_frame and order levels
cwd = "/Users/thomasvanasse/Google Drive/RESEARCH/PTSD_RS_ICA/Graphs/"

#get self-report measures from csv
self_report = read.csv(file=sprintf("%s/subject_scores.csv", cwd))
self_report_df = data.frame(self_report)
#create factors based on subject group
self_report_df$Group <- factor(self_report_df$Group, levels=c('cc','tec','ptsd'))

#GET z-score data
SDR_names = read.csv(file=sprintf("%s/SDR_names.csv", cwd), header = TRUE)
i <- 14
mydata = read.table(sprintf("%s/SDR_mean_z/MEANZ_OUTPUT_SDR%d.txt", cwd, i))
  
#add the mean z-score to data
self_report_df$meanz <- mydata[1:105,]
    
plot <- ggplot(self_report_df, aes(x=PCL_V1, y=meanz, color=Group))
    
plot + geom_point(aes(shape=Group, color=Group)) + xlab("PCL scores") + ylab("Mean z-score") +
      ggtitle(sprintf("%s, r=%g (p=%.2e)", SDR_names$SDR[i], round(cor(self_report_df$meanz,self_report_df$PCL_V1),2), cor.test(self_report_df$meanz,self_report_df$PCL_V1)$p.value)) + 
      theme(plot.title= element_text(hjust = 0.5)) + 
      theme_gray(base_size = 18) + guides(fill=FALSE) +
      theme(axis.text.x = element_text(colour="black"), axis.text.y = element_text(colour="black")) + 
      theme(axis.text=element_text(size=18), axis.title=element_text(size=15,face="bold"), plot.title = element_text(size = 15, face = "bold"))

ggsave(sprintf("%s/ptsd_tec_cc_graphs/PCL_correlation_%s.png",cwd,SDR_names$SDR[i]), width = 5, height = 5, dpi = 300)
  

