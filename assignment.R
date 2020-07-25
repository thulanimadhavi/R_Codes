#BT 4019 - Statistics for Bioinformatics 
#Take-home Assignment  
#S12764
#2015s15420
#to get the path of working directory
getwd()

#Read csv file
data=read.csv("breast-cancer-data.csv")

#Exploring the data
#Head of data
head(data)
#Tail of data
tail(data)
#Extract classes to explore about data
data$diagnosis
#Table
table(data$diagnosis)
#Pie chart
pie(table(data$diagnosis))

#Extract only continous data. Remove categorical data (variable 2) and data which do not affect the 
#process directly(example: variable 1 - ID and variable 33-NA data)
#So variable 1,2 and 33 are removed

#Preprocessed dataset
data1=data[3:32]
data1

?prcomp
#Implementation of PCA using prcomp
#Standardization of data-center and scale
PCA=prcomp(data1,center = T, scale=T)
#Investigating important PC's
#Summary of PCA
summary(PCA)
#Screeplot
screeplot(PCA, main = "Screeplot of first 10 PC's")
#abline of eigenvalue=1 - to select PC's cut by the abline
abline(h = 1, col="red")
#Elbow plot
plot(PCA, type="l", main = "Elbow plot of first 10 PC's")
#abline of eigenvalue=1 - to select PC's above the abline
abline(h = 1, col="red")
#Loading matrix
PCA$rotation
#Principal scores
head(PCA$x)

?plot
#############
#Plotting PC's
#Plot btw PC1 and PC2
plot(PCA$x[,1], PCA$x[,2], xlab="PC1", ylab="PC2", main = "PC2 Vs PC1 - plot")
#Plot btw PC1 and PC3
plot(PCA$x[,1], PCA$x[,3])
#Plot btw PC2 and PC3
plot(PCA$x[,2], PCA$x[,3])

#Column bind of original dataset and scoring matrix of PCA
data2=cbind(data,PCA$x)
data2

#ggplot between PC1 and PC2
library(ggplot2)
cancerdata=ggplot(data2,aes(PC1,PC2, col=diagnosis,fill=diagnosis))+
  stat_ellipse(geom = "polygon",col="black",alpha=0.5)+
  geom_point(shape=25,col="black")+ 
  ggtitle("PC2 Vs PC1-ggplot")
cancerdata

##############
#Implementation of PCA without using prcomp

#Scale the data - bcz data are in different ranges (not centralized) and in different units.
#So need to scale data
scaleddata = scale(data1)
scaleddata
#Build up variance covariance matrix
cov.matrix=var(scaleddata)
cov.matrix
#Build up eigen vectors and eigen values of covariance matrix
Ei=eigen(cov.matrix)
#Extract eigen vectors-loading matrix
Eivec=Ei$vectors

#Build up the PCA score matrix
#Multiplication of TEivec and dataset (Tdata1)
PCAscore=scaleddata%*%Eivec
head(PCAscore)




