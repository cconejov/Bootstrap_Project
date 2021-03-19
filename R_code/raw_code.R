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

datax <-  read.csv("data/data_3.csv", header = TRUE)

# ---- Part 0: lm model ---------------------------
# The dataset contains outliers and after building any OLS model it can be checked that the error residuals
# are not normally distributed. 

colnames(datax)
n <- nrow(datax)

model.lm <- lm(y ~  x1 + x2 + x3, data = datax)
summary(model.lm)$coef

#Diagnostics plots
par(mfrow=c(2,2))
plot(model.lm)
par(mfrow = c(1,1))
rm(model.lm)


# ---- Part 1: rlm model ---------------------------
# 1. Build a robust linear regression model with the three covariates 
# use (95%) bootstrap confidence intervals on the regressors' coefficients to study their signifficance.

model.rlm <- rlm(y ~  x1 + x2 + x3, data = datax)
coef(summary(model.rlm))

#@! Study the coefficients!

res_rob_rg <- function(x,beta,xdata){
  y_fit  <- beta[1] + beta[2]*xdata$x1 + beta[3]*xdata$x2 + beta[4]*xdata$x3 + x
  bmodel <- rlm(y_fit ~ x1 + x2 + x3, data = xdata)
  return(coef(bmodel))
}

rres    <- model.rlm$residuals
rbeta.h <- coef(model.rlm)

B <- 100
set.seed(1)
coeff.res <- bootstrap(rres, B, res_rob_rg, beta = rbeta.h, xdata = datax)$thetastar
head(coeff.res)

# Real coefficients
rbeta.h

# Basic Bootstrap

2*rbeta.h[1] - quantile(coeff.res[1,], c(0.975,0.025))
2*rbeta.h[2] - quantile(coeff.res[2,], c(0.975,0.025))
2*rbeta.h[3] - quantile(coeff.res[3,], c(0.975,0.025))
2*rbeta.h[4] - quantile(coeff.res[4,], c(0.975,0.025))


# ---- Part 2: Backward elimination ---------------------------
# 2. Use backward elimination to select the relevant covariates and provide the chosen regression model.

# From the part, beta3 have cero inside the interval, so it can be delete:


model.rlm <- rlm(y ~  x1 + x2, data = datax)
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
coeff.res <- bootstrap(rres, B, res_rob_rg, beta = rbeta.h, xdata = datax)$thetastar
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

res_rob_rg <- function(x,beta,xdata){
  y_fit  <- beta[1] + beta[2]*xdata$x1 + beta[3]*xdata$x2 + x
  bmodel <- rlm(y_fit ~ x1 + x2, data = xdata)
  return(predict(bmodel, data = c(14,14,14)))
}

#take the best model in 3 (change this)

fit <- rlm(y~ x1 + x2, data = datax)
rres    <- fit$residuals
rbeta.h <- coef(fit)

B <- 100
set.seed(1)
coeff.res <- bootstrap(rres, B, res_rob_rg, beta = rbeta.h, xdata = datax)$thetastar
coeff.res



# Basic Bootstrap

2*rbeta.h[1] - quantile(coeff.res[1,], c(0.975,0.025))
2*rbeta.h[2] - quantile(coeff.res[2,], c(0.975,0.025))
2*rbeta.h[3] - quantile(coeff.res[3,], c(0.975,0.025))
2*rbeta.h[4] - quantile(coeff.res[4,], c(0.975,0.025))