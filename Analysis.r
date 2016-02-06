# Impact factor vs rejection rate
# Written by Jon Tennant, ScienceOpen

# Input data from Figshare (https://figshare.com/s/d81dcba0b9c5400a9c46) after saving as a .csv file
setwd("file pathway")

data<-read.csv("filename.csv",header=TRUE)

# Create two new vectors
IF<-data[,2]
Rej.rate<-data[,1]

# Perform a simple linear regression
Fit.IF<-lm(Rej.rate~IF)
summary(Fit.IF)
plot(Fit.IF)

# Perform bivariate correlation tests
cor.test(Rej.rate,IF,method="pearson")
cor.test(Rej.rate,IF,method="spearman")
cor.test(Rej.rate,IF,method="kendall")

# Results
# Pearson's: cor=0.0453, p=0.0005
# Spearman's: cor=0.0344, p=0.412
# Kendall's: cor=0.0204, p=0.4706

# Plot the results up and export the plot as a PDF
pdf(file='If vs Rej Rate.pdf', width=14, height=8)
plot(Rej.rate,IF,type="p",pch=1,ylab="Journal impact factor (2014)",xlab="Journal rejection rate")
abline(lm(IF~Rej.rate),lwd=2,col="red",lty=2)
legend("topleft",c("Pearson's: cor=0.453, p=0.0005","Spearman's: cor=0.0344, p=0.412","Kendall's: cor=0.0204, p=0.4706"),border="NA")
dev.off()

# Repeat but with a rejection rate threshold of 0.6
# Note that you can input data with any threshold range you like
data2<-read.csv("modified filename.csv",header=TRUE)

IF.2<-data2[,2]
Rej.rate.2<-data2[,1]

Fit.IF.2<-lm(Rej.rate.2~IF.2)
summary(Fit.IF.2)
plot(Fit.IF.2)

cor.test(Rej.rate.2,IF.2,method="pearson") #cor=0.343, p=1.745^-11
cor.test(Rej.rate.2,IF.2,method="spearman") #cor=0.189, p=0.0003
cor.test(Rej.rate.2,IF.2,method="kendall") #cor=0.131, p=0.0003

pdf(file='If vs Rej Rate, reduced.pdf', width=14, height=8)
plot(Rej.rate.2,IF.2,type="p",pch=1,ylab="Journal impact factor (2014)",xlab="Journal rejection rate")
abline(lm(IF.2~Rej.rate.2),lwd=2,col="red",lty=2)
legend("topleft",c("cor=0.343, p=1.745^-11","cor=0.189, p=0.0003","cor=0.131, p=0.0003"))
dev.off()

# Repeat but with a modified rejection rate threshold of 0.9
data3<-read.csv("modified filename_2.csv",header=TRUE)

IF.3<-data3[,2]
Rej.rate.3<-data3[,1]

Fit.IF.3<-lm(Rej.rate.3~IF.3)
summary(Fit.IF.3)
plot(Fit.IF.3)

cor.test(Rej.rate.3,IF.3,method="pearson") #cor=0.09, p=0.732
cor.test(Rej.rate.3,IF.3,method="spearman") #cor=0.094, p=0.72
cor.test(Rej.rate.3,IF.3,method="kendall") #cor=0.096, p=0.612

pdf(file='If vs Rej Rate, reduced2.pdf', width=14, height=8)
plot(Rej.rate.3,IF.3,type="p",pch=1,ylab="Journal impact factor (2014)",xlab="Journal rejection rate")
abline(lm(IF.3~Rej.rate.3),lwd=2,col="red",lty=2)
legend("topleft",c("cor=0.09, p=0.732","cor=0.094, p=0.72","cor=0.096, p=0.612"))
dev.off()
