##------------------
## illustrate some features of splines
## CM: 19 July 2018
## 
##------------------

## Simulate some data, we'd like to model
## x 
years <- seq(1, 10)

## f(x)
fymean <- 1 + 0.5 * cos(2 * pi / 12 * years)

## create some observations around curve
fy <- sapply(fymean, FUN = function(z){rnorm(10, mean = z, sd = 0.2)})

fy.df <- data.frame(year = rep(years, each = nrow(fy)),
                    fy = c(fy))
## plot
with(fy.df, plot(year, fy, pch = 1, col = "slategrey", bty = "L"))

## fit a model with a mean per year 
factor.year.lm <- lm(fy ~ -1 + factor(year), data = fy.df)

## predict from this model
pred.df <- data.frame(year = years)
pred.df$pred.factor <- predict(factor.year.lm, newdata = pred.df)
    
with(pred.df, points(year, pred.factor, pch = 19, col = "red", cex = 1.4))

## number of parameters used 
length(coef(factor.year.lm))

## a quadratic model
quadratic.year.lm <- lm(fy ~ year + I(year^2), data = fy.df)

## predict from this model
pred.df$pred.quadratic <- predict(quadratic.year.lm, newdata = pred.df)

with(pred.df, lines(year, pred.quadratic, pch = 19, col = "blue", lwd = 1.5))
## number of coefficients
length(coef(quadratic.year.lm))

##plot(predict(quadratic.year.lm), resid(quadratic.year.lm), pch = 19, col = "purple")

## a gam model (uses splines to smooth the function)
library(mgcv)
gam.year.gam <- gam(fy ~ s(year), data = fy.df)
summary(gam.year.gam) ## note edf (expected degrees of freedon of the spline)

## predict from the gam
pred.df$pred.gam <- predict(gam.year.gam, newdata = pred.df)

with(pred.df, lines(year, pred.gam, pch = 19, col = "forestgreen", lwd = 1.5))

## get Akaike Information Criterion (AIC = -2 x log-likelihood + 2 npar)
AIC(factor.year.lm)
AIC(quadratic.year.lm)
AIC(gam.year.gam)

## which is fitting better?
## please also look at the residuals!
