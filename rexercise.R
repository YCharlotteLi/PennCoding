### This script will demonstrate basic familiarity with R
###
### Charlotte Li
### Apr 20 2021


# Load the R library, ggplot2, and no others (i.e., you should use base R for most
# of this exercise)
library(ggplot2)
set.seed(2021)
# Create a dataframe with NAs in every cell, 100 rows, and columns with the
# following names: bblid,sex,age,cognition.
dat<-data.frame(matrix(data=NA,nrow=100,ncol=4))
colnames(dat)<-c("bblid","sex","age","cognition")
# Sample 100 random integers, without replacement, between 10,000 and 20,000
# to serve as bblids (our lab's subject identifier). Put these integers into the
# bblid column.
dat["bblid"]<-sample(seq(10000,20000),100,replace = F)

# Simulate 100 ages and 100 cognition scores from a bivariate normal distribution
# with rho=.7, and put these values into their respective columns in the dataframe.
rho=0.7
## simulate age, mean of 20, with a sd 2
age<-rnorm(100,20,2)
s.age=sd(age)
mean.age=mean(age)
## simulate cognition, mean score of 25 with a sd of 3
cognition<-rnorm(100,25,3)
s.cognition=sd(cognition)
mean.cognition=mean(cognition)
covar=rho*s.age*s.cognition
sigma=matrix(ncol=2,nrow=2,c(s.age^2,covar,covar,s.cognition^2))
temp=eigen(sigma)
SqrtSigma = temp$vectors%*%diag(sqrt(temp$values))%*%t(temp$vectors)
XYvec = c(mean.age,mean.cognition) + SqrtSigma%*%rnorm(2)
XYvec

for(i in 1:100){
  XYvec=c(mean.age,mean.cognition)+ SqrtSigma%*%rnorm(2)
  dat$age[i]=XYvec[1]
  dat$cognition[i]=XYvec[2]
}
# Scale the age values so mean=15 and variance=6.
mean_target <- 15 #desired mean/sd of the data
sd_target   <- 6
dat$age <- mean_target + (dat$age - mean(dat$age)) * sd_target/sd(dat$age) #according to the given formula following the link you provided
##check
mean(dat$age)
sd(dat$age)
# Check if any age values are below zero. How many are there?
which(dat$age<0)
## no value was below 0

# For any age values that are below zero, replace that age with zero. Write this
# code whether or not there are ages below zero.

# Create two ggplots: a histogram of ages, and a histogram of cognition. Use
# non-default themes and colors (explore!).
p1<-ggplot(data = dat,aes(x=age))+geom_histogram(color="black", fill="coral",alpha=0.8)+
  theme_bw()+ggtitle("Age Histogram")+xlab("Age")+ylab("Count")
p2<-ggplot(data = dat,aes(x=cognition))+geom_histogram(color="black", fill="royalblue",alpha=0.8)+
  theme_bw()+ggtitle("Cognition Histogram")+xlab("Cognition")+ylab("Count")
# Calculate the correlation between age and cognition. Does it look familiar?
cor(dat$age,dat$cognition)
# Now assign the first 50 rows in your dataframe to be female, and the latter
# 50 to be male. Make sure your sex variable is coded as a factor.
dat[1:50,2]<-"F"
dat[51:100,2]<-"M"
dat$sex<-as.factor(dat$sex)
# Create a scatterplot with age on the x-axis, and cognition on the y-axis.
# Color your points by sex, and plot a regression line per sex.
p3<-ggplot(data=dat,aes(x=age,y=cognition,color=sex))+geom_point()+
  geom_smooth(method='lm',se=F)+theme_bw()
# Export all of your plots to one pdf, with each plot on a different page.
p<-list(p1,p2,p3)
pdf("rexercise_plots.pdf",onefile=T)
for (i in 1:length(p)){
  print(p[i])
}
dev.off()