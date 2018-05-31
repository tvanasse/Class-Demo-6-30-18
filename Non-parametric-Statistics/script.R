#Non-parametric Statistics Lecture(6/1/18)
#Tom Vanasse 
library(ggplot2)
data("cars")

#Spearman Rank Correlation
plot(cars$speed,cars$dist)
corr <- cor.test(x=cars$speed, y=cars$dist, method = 'spearman')
corr


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

# FISHER EXACT TEST in R
#
# Example data:
# Area     With rabies      Without rabies
# ----     -----------      --------------
#  Earl          10                5 
#  Will           6                17
# Read the 2x2 contingency table, r=2 rows (X values), c=2 columns (Y values):
data<-scan()

tab <- t(matrix(data, nrow=2,ncol=2))  # transpose, as usual
tab         # view the table to make sure it is right


fisher.test(tab)   # performs a test of independence for matrix input.

#McNemar Test
## Agresti (1990), p. 350.
## Presidential Approval Ratings.
##  Approval of the President's performance in office in two surveys,
##  one month apart, for a random sample of 1600 voting-age Americans.
Performance <-
  matrix(c(794, 86, 150, 570),
         nrow = 2,
         dimnames = list("1st Survey" = c("Approve", "Disapprove"),
                         "2nd Survey" = c("Approve", "Disapprove")))
Performance
mcnemar.test(Performance)
## => significant change (in fact, drop) in approval ratings

library(ISwR)
attach(energy)
energy

#wilcox test
wilcox.test(expend~stature)

#wilcox signed rank test
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


