#Multiple Linear Regression Analysis
data(seatpos,package = "faraway") #loading the data 
library(faraway)
#attach(seatpos)
head(seatpos)
summary(seatpos)
#dim(seatpos)
library(car)

#install.packages("faraway")

#fitting the model

model<-lm(hipcenter~., data = seatpos)

summary(model)

#1. testing for multicollinearity

#i) condition numbers

c <- model.matrix(model)[,-1]  #eigendecomposition of the predictor space excluding the intercept
e <- eigen(t(c) %*% c)
e$val
sqrt(e$val[1]/e$val)

#ii) correlation matrix to determine the strength of the relationships 
#between predictors
round(cor(seatpos),2)



#iv) variance inflation factor
vif(model)
#based on the results HtShoes and Ht are correlated with all other variables
#And Leg and seated variables have a strong correlation to other variables

set.seed(133)
#adding random noise to test the effect of collinearity
noise = rnorm(n = nrow(seatpos), mean = 0, sd = 10)
model_noise = lm(hipcenter + noise ~ ., data = seatpos)
summary(model_noise)
coef(model)
coef(model_noise)
# no significant change based on the coefficient comparison

#2. Checking unusual observations
par(mfrow = c(1, 1))
#i) leverage
hatv <- hatvalues(model)
head(hatv)
sum(hatv)    #sum of all leverages equal number of parameters in the model
drivers<- row.names(seatpos)
halfnorm(hatv,labs=drivers,ylab="Leverages")
abline(h=0.474, col="blue") # cutoff point = 2*p/n, any point > h, is a leverage
#22 and 31 stick out most as influential points


#ii) outliers

stud <- rstudent(model)
stud[which.max(abs(stud))]
qt(.05/(38*2),28)              #Bonferroni critical value
#since 2.39 < |-3.57|, observation 31 is not an outlier

#iii) influential points - use Cook's distance
cook <- cooks.distance(model)
halfnorm(cook,2,labs=drivers,ylab="Cook’s distances")
abline(h=0.11)
#the plot suggests that age 31 may significantly affect regression results and
#potentially distort conclusions drawn

#removing the observation with the largest's cooks distance to check how it influences the fit
modcook <- lm(hipcenter~.,seatpos,subset=(cook < max(cook)))
sumary(modcook)

coef(model)# coeffiecients related to each predictor before removing largest
#cooks distance
coef(modcook)# coeffiecients related to each predictor after removing largest
#cooks distance
#coefficients significantly change which suggests the 31 does in fact
# distort results

require(lmtest)
shapiro.test(residuals(model))  #test for normality
#p-value=0.4341>0.05 therefore it suggests the residuals 
#likey Normally distributed
bptest(model)                   #test for homoscedasticity Breusch-Pagan
#p-value=0.081>0.05 therefore it suggests the variance of the errors are
#likey constant
dwtest (model)               #test for correlated errors
#p-value=0.2441>0.05 therefore it suggests there is
#likey no significant autocorrelation


#5. Selection of the "best" model
#i) AIC                        #AIC = nlog(RSS/n) + 2p
require(leaps)
par(mfrow = c(1, 1)) # 
b <- regsubsets(hipcenter~., data = seatpos)
rs <- summary(b)
rs$which
AIC <- 38*log(rs$rss/38) + (2:9)*2
AIC
plot(AIC ~ I(1:8), ylab="AIC", xlab="Number of Predictors")
# since the model 3 has the lowest AIC value we can assume its the best model 
# i.e. HtShoes is the best predictor 

model2<-lm(hipcenter~Age+Ht+Leg, data = seatpos)
summary(model2)
#significance of regression coefficients using CI #if B_j = 0 falls within a CI
confint(model2)
#Zero falls within the confidence region for all predictors
# i.e.there is insufficient evidence to conclude the predictors
#significantly affect the outcome
