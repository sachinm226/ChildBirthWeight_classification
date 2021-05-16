rm(list=ls())
df <- read.csv("C:\\Users\\sachi\\Downloads\\NCI\\Statics\\CA2\\Childbirths.csv")
attach(df)

class(df)

library(tidyverse)
library(corrplot)
library(ggplot2)
library(lubridate)
library(gridExtra)
library(caTools)
library(GGally)

df$smoker <? as.factor(df$smoker)
df$lowbwt <- as.factor(df$lowbwt)
df$mage35 <- as.factor(df$mage35)
str(df)


dim(df)

library("dplyr")

#Correlation plot
correlation=cor(select_if(df, is.numeric))
par(mfrow=c(1, 1))
corrplot(correlation,method="color")#hypothesis t?sting

ggpairs(select_if(df, is.numeric))


#Box plot
par(mfrow=c(4, 3))
boxplot(Length~lowbwt,data=df,main="Length of baby (cm) w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Length of baby (cm)",col="green",border="brown")
boxplot(Headcirc~lowbwt?data=df,main="Headcirc w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Head Circumference",col="yellow",border="brown")
boxplot(mage~lowbwt,data=df,main="mage w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Maternal age",col="orange",border="?rown")
boxplot(mnocig~lowbwt,data=df,main="mnocig w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Number of cigarettes smoked per day by mother",col="blue",border="brown")
boxplot(mheight~lowbwt,data=df,main="mheight w.r.t Low Birth Weight", xlab="L?w Birth weight",ylab="Mothers height (cm)",col="purple",border="brown")
boxplot(mppwt~lowbwt,data=df,main="mppwt w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Mothers pre-pregnancy weight (kg)",col="red",border="brown")
boxplot(fage~lowbwt,data=df?main="fage w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Father's age",col="grey",border="brown")
boxplot(fedyrs~lowbwt,data=df,main="fedyrs w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Father's years in education",col="pink",border="bro?n")
boxplot(fnocig~lowbwt,data=df,main="fnocig w.r.t Low Birth Weight", xlab="Low Birth weight",ylab="Number of cigarettes smoked per day by father",col="black",border="brown")
boxplot(fheight~lowbwt,data=df,main="fheight w.r.t Low Birth Weight", xlab="Low?Birth weight",ylab="Father's height (kg)",col="orange",border="brown")
?boxplot
library(car)
print("From Here")
# Compute the analysis of variance
leveneTest(Length ~ lowbwt, data = df)
res.aov <- aov(Length ~ lowbwt, data = df)
# Summary of the analysis
s?mmary(res.aov)
# Compute the analysis of variance
leveneTest(Headcirc ~ lowbwt, data = df)
res1.aov <- aov(Headcirc ~ lowbwt, data = df)
# Summary of the analysis
summary(res1.aov)
leveneTest(mage ~ lowbwt, data = df)
res2.aov <- aov(mage ~ lowbwt, data = ?f)
# Summary of the analysis
summary(res2.aov)
leveneTest(mnocig ~ lowbwt, data = df)
res3.aov <- aov(mnocig ~ lowbwt, data = df)
# Summary of the analysis
summary(res3.aov)
leveneTest(mheight ~ lowbwt, data = df)
res4.aov <- aov(mheight ~ lowbwt, data = d?)
# Summary of the analysis
summary(res4.aov)
leveneTest(mppwt ~ lowbwt, data = df)
res5.aov <- aov(mppwt ~ lowbwt, data = df)
# Summary of the analysis
summary(res5.aov)
leveneTest(fage ~ lowbwt, data = df)
res6.aov <- aov(fage ~ lowbwt, data = df)
# Summ?ry of the analysis
summary(res6.aov)
leveneTest(fedyrs ~ lowbwt, data = df)
res7.aov <- aov(fedyrs ~ lowbwt, data = df)
# Summary of the analysis
summary(res7.aov)
leveneTest(fnocig ~ lowbwt, data = df)
res8.aov <- aov(fnocig ~ lowbwt, data = df)
# Summary?of the analysis
summary(res8.aov)
leveneTest(fheight ~ lowbwt, data = df)
res9.aov <- aov(fheight ~ lowbwt, data = df)
# Summary of the analysis
summary(res9.aov)



#Model building
library(tidyverse)
library(caret)
library(e1071)

#logistic regression mod?l1
lg_1 <- glm(lowbwt ~ Length:Headcirc, data = df, family = binomial)
coef(lg_1)
pred <- ifelse(lg_1$fitted.values > 0.4, "1", "0")
confusionMatrix(as.factor(pred), as.factor(lowbwt))
#df$lowbwt == "0"
#pred == "0"
summary(lg_1)
#ROC curve
library(pROC)
r? = roc(as.numeric(df$lowbwt) ~ lg_1$fitted.values, plot = TRUE, print.auc = TRUE)
as.numeric(rc$auc)



newdat <- data.frame(hp=seq(min(mtcars$hp), max(mtcars$hp),len=100))
newdat = predict(lg_1, newdata=data.frame(Length,Headcirc,mppwt), type="response")
?lot(lowbwt ~ Length+Headcirc+mppwt, data = df, col="red4")
lines(lowbwt ~ Length+Headcirc+mppwt, df, col="green4", lwd=2)
#confusionMatrix(predict, subset(df,select = -c(lowbwt)),lowbwt, type = 'response')
#pred <- predict(lg, subset(df,select = -c(lowbwt)?)
#pred <- ifelse(pred > 0.7, "1", "0")
#lowbwt <- as.factor(lowbwt)
#pred<- as.factor(pred)
#table(pred, lowbwt)
#confusionMatrix(pred, lowbwt)

#PCA code

##PCA
library(psych)

#check for number of components
num_df <- select_if(df, is.numeric)
num_df <-?subset(num_df, select = -c(Birthweight))
fa.parallel(num_df ,fa="pc",n.iter = 10)

#Extract components
pc.df<-principal(num_df ,nfactors = 10,rotate = "none")
pc.df

#Rotate components
rc.df<-principal(num_df,nfactors = 5,rotate="varimax")
rc_comp <- as.da?a.frame(rc.df$scores)

#Interpret rotated components
fa.diagram(rc.df)

#Factor Scores
rc.df$score

df_1 <- df
df_1$RC1 <-rc_comp$RC1
df_1$RC2 <-rc_comp$RC2
df_1$RC3 <-rc_comp$RC3
df_1$RC4 <-rc_comp$RC4
df_1$RC5 <-rc_comp$RC5

#logistic regression model1
l?_2 <- glm(lowbwt ~ df_1$RC1+df_1$RC2+df_1$RC3, data = df, family = binomial)
coef(lg_2)
pred <- ifelse(lg_2$fitted.values > 0.3, "1", "0")
confusionMatrix(as.factor(pred), as.factor(lowbwt))
#df$lowbwt == "0"
pred == "0"
summary(lg_2)
#ROC curve
library(pR?C)
rc = roc(as.numeric(df$lowbwt) ~ lg_2$fitted.values, plot = TRUE, print.auc = TRUE)
as.numeric(rc$auc)
####Adity codes
train_set<-df[-15]
head(train_set)


#By default, it centers the variable to have mean equals to zero. 
#With parameter scale. = T, no?malize the variables to have standard deviation equals to 1.
pca <- prcomp(num_df, scale. = T)
#center and scale refers to mean and standard deviation of the variables
names(pca)



#Each column of rotation matrix contains the principal component loading v?ctor.
#4 principal components and first 3 rows
pca$rotation[1:3,1:4]



#the matrix x has the principal component score vectors in a 42  × 15 dimension.
dim(pca$x)


#standard deviation of each principal component
std_dev <- pca$sdev

#variance
variance <-?std_dev^2
#divide the variance by sum of total variance -> to compute the proportion of variance explained by each component
variance_prop <- variance/sum(variance)
#first principal component explains 6.98% of the variance, second 3.2%, third 2.5% 
varianc?_prop[1:10]


#scree plot - the percentage of variance explained by each principal component
plot(variance_prop, xlab = "Principal Component", ylab = "Proportion of Variance Explained", type = "b", xlim=c(0, 20))




#cumulative variance plot
plot(cumsum(v?riance_prop), xlab = "Principal Component", ylab = "Cumulative Proportion of Variance Explained", type = "b", xlim=c(0, 20))








