#--------------Factor Analysis and Regression - Hair dataset------------------------

#Setting up working directory
setwd("C:/Users/mchoudhu/Documents/*******")
getwd()

#Importing required Hair dataset
mydata = read.csv("Factor-Hair-Revised.csv")
attach(mydata)

install.packages("ggplot2")
library(ggplot2)
install.packages("psych")
install.packages("car")
install.packages("foreign")
install.packages("MASS")
install.packages("lattice")
library(psych)
library(car)
library(foreign)
library(MASS)
library(lattice)
install.packages("nortest")
library(nortest)
install.packages("corrplot")
library(corrplot)

#Understanding the dataset using the following commands
head(mydata)
dim(mydata)
names(mydata)
str(mydata)
summary(mydata)


mydata1 = mydata[,-1] #removing the unnecessary column

variablenames = c("Product Quality","E-Commerce","Technical Support","Complaint Resolution","Advertising", 
                  "Product Line","Salesforce Image","Competitive Pricing","Warranty & Claims","Order & Billing",
                  "Delivery Speed","Customer Satisfaction")
colnames(mydata1) = variablenames
mydata1
attach(mydata1)

#Checking for missing values and outliers in the dataset
sum(is.na(mydata1))
cat("Number of missing values =", sum(is.na(mydata1)))
cat("Outliers in Data:",boxplot(mydata1,plot=FALSE)$Out)

par("mar")
(par(mar=c(1,1,1,1)))

#Histogram of the dependent Variable which is Customer Satisfaction
hist (`Customer Satisfaction`, breaks = c(0:11), labels =TRUE,
      include.lowest=TRUE, right=TRUE, 
      col="light blue", border=1, 
      main = "Histogram of Customer Satisfaction",
      xlab= "Customer Satisfaction", ylab="COUNT", 
      xlim = c(0,11), ylim = c(0,35))

#BoxPlot of the Dependent Variable which is Customer Satisfaction
boxplot(`Customer Satisfaction`,horizontal=TRUE,col="light blue", 
        xlab="Customer Satisfaction",ylim=c(0,11))



#Histogram of all independent Variables
par(mfrow = c(3,4)) #Converting Plotting space into 3 rows and 4 columns
for (i in (1:11)) 
  {
    h = round(max(mydata1[,i]),0)+1
    l = round(min(mydata1[,i]),0)-1
    n = variablenames[i]
    hist (mydata1[,i],breaks=seq(l,h,((h-l)/6)),labels=TRUE,
        include.lowest=TRUE,right=TRUE, 
        col="pink",border=1, 
        main = NULL, xlab= n, ylab=NULL, 
        cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,
        xlim = c(0,11), ylim = c(0,70))
}

#Exploring the dataset using boxplot
par(mfrow = c(2,1))
boxplot(mydata1[,-12],las = 2,names=variablenames[-12],cex.axis=1)

#Bivariate Analysis#
##Scatter and Line Plot of independent variables against the dependent variable##
par(mfrow = c(3,4))
for (i in c(1:11)) {
  plot(mydata1[,i],`Customer Satisfaction`, 
       xlab = variablenames[i], ylab = NULL, col = "dark green", 
       cex.lab=1, cex.axis=1, cex.main=1, cex.sub=1,
       xlim = c(0,10),ylim = c(0,10))
  abline(lm(formula=`Customer Satisfaction`~mydata1[,i]),col = "blue")
}
dev.off()

## correlation matrix and correlation plot
corMtrx=cor(mydata1[,-12])
corMtrx


# Checking multicollinearity

# Kaiser-Meyer-Olkin Test (KMO) is a measure of how appropriate the 
# given dataset is for Factor Analysis
KMO(corMtrx)

#The Overall MSA value is more than 0.5, hence it is an evidence of 
#the presence of multicollinearity

SLRPQ=lm(`Customer Satisfaction`~`Product Quality`)
summary(SLRPQ)

SLREC=lm(`Customer Satisfaction`~`E-Commerce`)
summary(SLREC)

SLRTS=lm(`Customer Satisfaction`~`Technical Support`)
summary(SLRTS)

SLRCR=lm(`Customer Satisfaction`~`Complaint Resolution`)
summary(SLRCR)

SLRA=lm(`Customer Satisfaction`~`Advertising`)
summary(SLRA)

SLRPL=lm(`Customer Satisfaction`~`Product Line`)
summary(SLRPL)

SLRSI=lm(`Customer Satisfaction`~`Salesforce Image`)
summary(SLRSI)

SLRCP=lm(`Customer Satisfaction`~`Competitive Pricing`)
summary(SLRCP)

SLRWC=lm(`Customer Satisfaction`~`Warranty & Claims`)
summary(SLRWC)

SLROB=lm(`Customer Satisfaction`~`Order & Billing`)
summary(SLROB)

SLRDS=lm(`Customer Satisfaction`~`Delivery Speed`)
summary(SLRDS)


#Factor Analysis
ev=eigen(corMtrx)
eigenvalues=ev$values
eigenvalues

Factor=c(1,2,3,4,5,6,7,8,9,10,11)
scree=data.frame(Factor,eigenvalues)
#Scree Plot
par("mar")
(par(mar=c(1,1,1,1)))
plot(scree,main="Scree Plot of eigen values",xlab="Factors",
     ylab="Eigen values",col="dark green",ylim=c(0,4),pch=20)
lines(scree,col="red")
abline(h=1,col="green",lty=2)

#We will consider Components with eigenvalues > 1 unit,i.e.,PC1 to PC4
eigenvectors=ev$vectors
eigenvectors

# Getting the loadings and Communality
#Unrotated
pc1=principal(mydata1,nfactors = 4,rotate = "none")
pc1
#SS loadings = Eigenvalues
#%Variance = Proportion Var 0.34 0.21 0.14 0.10
print(pc1,digits = 5)

#Rotated
pc2=principal(mydata1,nfactors = 4,rotate = "varimax")
pc2
print(pc2,digits = 5)


pc2$scores
Loading1=print(pc2$loadings,cutoff = 0.3)


# RC1 : Customer/Client/User Interaction
# RC2 : Marketing Strategy
# RC3 : Value for Money
# RC4 : Quality of Customer Service

write.csv(Loading1, "Loadings.csv")

loadingdata=read.csv("Loadings.csv")
loadingdata
attach(loadingdata)
loadingdata1=loadingdata[,-1]
loadingdata1

class(loadingdata)

mydata2=cbind(mydata1[,12],pc2$score)
head(mydata2)

# Dependent variable (V1) = Customer Satisfaction
# RC1 = Customer Interaction
# RC2 = Marketing Strategy
# RC4 = Value for Money
# RC3 = Quality of Customer Service


class(mydata2)

mydata2=as.data.frame(mydata2)
names(mydata2)

MLReg = lm(V1 ~., data = mydata2)
print(summary(MLReg))

anova(MLReg)


confint(MLReg,"RC1")
confint(MLReg,"RC2")
confint(MLReg,"RC4")
confint(MLReg,"RC3")

newdata=data.frame(RC1=0.72,RC2=0.67,RC4=0.78,RC3=0.15)
newdata

prediction=predict(MLReg,newdata)
prediction

prediction=predict(MLReg,newdata, interval = "confidence")
prediction


Predicted=predict(MLReg)
Actual=mydata$Satisfaction
Backtrack=data.frame(Actual,Predicted)
Backtrack

plot(Actual,col="Dark Green")
lines(Actual,col="Dark Green")
plot(Predicted,col="Blue")
lines(Predicted,col="Blue")
lines(Actual,col="Dark Green")


#-------------------------------THE END----------------------------------







