#Non-parametric Statistics Lecture(6/1/18)
#Tom Vanasse 

heartatk = read.table("http://statland.org/R/RC/heartatk4R.txt",header=TRUE) 
heartatk

attach(heartatk)

#create contingency table
table(SEX,DIED)

#create mosaic plot
a =table(SEX,DIED) 
mosaicplot(a, color = T) 

#A test of independence, dof = 1
chisq.test(table(SEX,DIED))

#Plot X^2 distribution
x <- rchisq(12844, 1)
hist(x, prob=TRUE, ylim=c(0, 1), main='pdf of Chi-square (df = 1)')
lines(density(x), col='orange')

#Plot a hypothetical observed chi-square:
abline(v=5)

#Plot our own chi-square:
hist(x, prob=TRUE, xlim=c(0, 150), ylim=c(0, 1), main='pdf of Chi-square (df = 1)')
lines(density(x), col='orange')
abline(v=147)

#Fisher Test
fisher.test(table(SEX,DIED))

#McNemar Test
mcnemar.test(table(SEX,DIED))

library(ISwR)
attach(energy)
energy

#wilcox test
wilcox.test(expend~stature)

#wilcox signed rank test
#n is the number of pairs, nn is number of observations
daily.intake<-c(5260,5470,5640,
                6180, 6390,6515,6805,7515,7515,
                8230,8770)
mean(daily.intake)
wilcox.test(daily.intake,mu=7725)

#Sign Test
binom.test(5, 18) 
binom.test(15, 18)

#Kruskal-Wallis
attach(red.cell.folate)
red.cell.folate

xbar <- tapply(folate, ventilation, mean)
s <- tapply(folate, ventilation, sd)
n <- tapply(folate, ventilation, length)
sem <- s/sqrt(n)
stripchart(folate~ventilation, method='jitter', jitter=0.05, pch=16, vert=T)
arrows(1:3,xbar+sem,1:3,xbar-sem,angle=90,code=3,length=.1)
lines(1:3,xbar,pch=4,type="b",cex=2)

summary(red.cell.folate)
kruskal.test(folate~ventilation)


