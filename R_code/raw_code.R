## ----------------------------------------
# Dataset data x.csv contains data on variables y, x1, x2, x3 
# Goal Build a linear regression model to predict y in terms of some of the other three variables. 
# dataset contains outliers: For these reasons, it makes sense to build a robust linear regression model (rlm) 
# and use the bootstrap to study the significance of the regressors.
## ----------------------------------------

rm(list = ls())

# Load libraries/ data

library(MASS)
library(bootstrap)

data3 <-  read.csv("data/data_3.csv", header = TRUE)

# ---- Part 0: lm model ---------------------------
# The dataset contains outliers and after building any OLS model it can be checked that the error residuals
# are not normally distributed. 

colnames(data3)
n <- nrow(data3)

boxplot(data3$y ~ data3$x3)

model.lm <- lm(y ~  x1 + x2 + x3, data = data3)
summary(model.lm)$coef
round(coef(model.lm),2)

#Diagnostics plots
par(mfrow=c(2,2))
plot(model.lm)
par(mfrow = c(1,1))
rm(model.lm)


# ---- Part 1: rlm model ---------------------------
# 1. Build a robust linear regression model with the three covariates 
# use (95%) bootstrap confidence intervals on the regressors' coefficients to study their signifficance.

model.rlm <- rlm(y ~  x1 + x2 + x3, data = data3)
coef(summary(model.rlm))

#@! Study the coefficients!

res_rob_rg <- function(x, beta, xdata){
  y_fit  <- beta[1] + beta[2]*xdata$x1 + beta[3]*xdata$x2 + beta[4]*xdata$x3 + x
  bmodel <- rlm(y_fit ~ x1 + x2 + x3, data = xdata)
  return(coef(bmodel))
}

rres    <- model.rlm$residuals
rbeta.h <- coef(model.rlm)

B <- 100
set.seed(1)
coeff.res <- bootstrap(rres, B, res_rob_rg, beta = rbeta.h, xdata = data3)$thetastar

# Real coefficients
rbeta.h

# Basic Bootstrap

basic_beta1 <- 2*rbeta.h[1] - quantile(coeff.res[1,], c(0.975,0.025))
basic_beta2 <- 2*rbeta.h[2] - quantile(coeff.res[2,], c(0.975,0.025))
basic_beta3 <- 2*rbeta.h[3] - quantile(coeff.res[3,], c(0.975,0.025))
basic_beta4 <- 2*rbeta.h[4] - quantile(coeff.res[4,], c(0.975,0.025))
rbind(basic_beta1, basic_beta2, basic_beta3, basic_beta4)

knitr::kable(codemess)

par(mfrow=c(2,2))
hist(coeff.res[1,], main = "Bootstrap beta1", probability = T, xlab = "beta1")
abline(v = rbeta.h[1], col ="blue")
hist(coeff.res[2,], main = "Bootstrap beta2", probability = T, xlab = "beta2")
abline(v = rbeta.h[2], col ="blue")
hist(coeff.res[3,], main = "Bootstrap beta3", probability = T, xlab = "beta3")
abline(v = rbeta.h[3], col ="blue")
hist(coeff.res[4,], main = "Bootstrap beta4", probability = T, xlab = "beta4")
abline(v = 0, col ="red")
abline(v = rbeta.h[4], col ="blue")
par(mfrow=c(1,1))

# ---- Part 2: Backward elimination ---------------------------
# 2. Use backward elimination to select the relevant covariates and provide the chosen regression model.

# From the part, beta4 have zero inside the interval (related to x3), so it can be delete:
# We study the Bca intervals

y_predict <-  predict(model.rlm)


## x1

res_rob_rg_x1 <- function(x, beta, xdata){
  
  y_bt <-  y_predict + x
  y_bt <-  y_bt - (beta[1] + beta[3]*xdata$x2 + beta[4]*xdata$x3)
  data1_BT <-  data.frame(x1 = data3$x1,
                          y = y_bt)
  fit_BT <- rlm(y ~ x1 -1, data = data1_BT)$coefficients
  return(fit_BT)
}

B = 1000
bcanon(rres, B, res_rob_rg_x1, beta = rbeta.h, xdata = data3,alpha = c(0.025, 0.975))$confpoints

## x2

res_rob_rg_x2 <- function(x, beta, xdata){
  
  y_bt <-  y_predict + x
  y_bt <-  y_bt - (beta[1] + beta[2]*xdata$x1 + beta[4]*xdata$x3)
  data1_BT <-  data.frame(x2 = data3$x2,
                          y = y_bt)
  fit_BT <- rlm(y ~ x2 -1, data = data1_BT)$coefficients
  return(fit_BT)
}

B = 1000
bcanon(rres, B, res_rob_rg_x2, beta = rbeta.h, xdata = data3,alpha = c(0.025, 0.975))$confpoints


## x3

res_rob_rg_x3 <- function(x, beta, xdata){
  
  y_bt <-  y_predict + x
  y_bt <-  y_bt - (beta[1] + beta[2]*xdata$x1 + beta[3]*xdata$x2)
  data1_BT <-  data.frame(x3 = data3$x3,
                          y = y_bt)
  fit_BT <- rlm(y ~ x3 -1, data = data1_BT)$coefficients
  return(fit_BT)
}

B = 1000
bcanon(rres, B, res_rob_rg_x3, beta = rbeta.h, xdata = data3,alpha = c(0.025, 0.975))$confpoints





model.rlm <- rlm(y ~  x1 + x2, data = data3)
coef(summary(model.rlm))

#@! Study the coefficients!

res_rob_rg <- function(x,beta,xdata){
  y_fit  <- beta[1] + beta[2]*xdata$x1 + beta[3]*xdata$x2 + x
  bmodel <- rlm(y_fit ~ x1 + x2, data = xdata)
  return(coef(bmodel))
}

rres    <- model.rlm$residuals
rbeta.h <- coef(model.rlm)

B <- 100
set.seed(1)
coeff.res <- bootstrap(rres, B, res_rob_rg, beta = rbeta.h, xdata = data3)$thetastar
head(coeff.res)

# Real coefficients
rbeta.h

# Basic Bootstrap

2*rbeta.h[1] - quantile(coeff.res[1,], c(0.975,0.025))
2*rbeta.h[2] - quantile(coeff.res[2,], c(0.975,0.025))
2*rbeta.h[3] - quantile(coeff.res[3,], c(0.975,0.025))

# All positive, x1 and x2 are relevant

# ---- Part 3 Confidence Intervals ---------------------------
# 3. Provide 95% confidence intervals on the regression coefficients.


# ---- Part 4 Mean Response ---------------------------
## 4. Build a 95% confidence interval on the mean response when (x1; x2; x3) = (14; 14; 14).


# Theoretical model:

model.rlm <- rlm(y ~  x1 + x2 + x3, data = data3)
coef(summary(model.rlm))
fit_value <- predict(model.rlm, newdata = data.frame(cbind(x1 = 14, x2 = 14, x3 = 14)))
fit_value

model.rlm <- rlm(y ~  x1 + x2, data = data3)
coef(summary(model.rlm2))
fit_value <- predict(model.rlm, newdata = data.frame(cbind(x1 = 14, x2 = 14, x3 = 14)))
fit_value


res_rob_mean <- function(x,beta,xdata){
  y_fit  <- beta[1] + beta[2]*xdata$x1 + beta[3]*xdata$x2 + x
  bmodel <- rlm(y_fit ~ x1 + x2, data = xdata)
  return(predict(bmodel, newdata = data.frame(cbind(x1 = 14, x2 = 14, x3 = 14))))
}

#take the best model in 3 (change this)
rres    <- model.rlm$residuals
rbeta.h <- coef(model.rlm)

B <- 100
set.seed(1)
mean.response <- bootstrap(rres, B, res_rob_mean, beta = rbeta.h, xdata = data3)$thetastar
coeff.res

hist(coeff.res, main = "Mean Response")

# Basic Bootstrap

2*fit_value - quantile(mean.response, c(0.975,0.025))
