#####################################################################
# SCA
# Statistical catch-at-age framework for stock assessment
#####################################################################


# fit types = "MP" (Management Procedures) or "assessment"

# fit method = "sca"  

?sca

#====================================================================
# Load
#====================================================================
# install.packages("rgl")
# install.packages("diagram")
rm(list=ls())

library(rgl)
library(FLa4a)
library(diagram)
library(ggplotFL)

# Load the Hake stock for the combined GSA 9-10-11

load("HKE_09_10_11_EWG15_11.RData")
load("HKE_09_10_11_idx.Rdata")

# rename the stocks
hke <- HKE_09_10_11_EWG15_11
hke.idx <- flq.idx 
plot(hke)
# Adjust Fbar range
xyplot(data~age,groups=year,data=hke@catch.n,type=c("l","p"))
range(hke)["minfbar"]
range(hke)["maxfbar"]

units(harvest(hke)) <-"f"
range(hke)["minfbar"] <- 0   
range(hke)["maxfbar"] <- 3

#====================================================================
# Quick and dirty
#====================================================================
# To fit a simple default a4a model, use the function sca()

fit <- sca(hke, hke.idx)

# diagnostics
res <- residuals(fit, hke, hke.idx)
plot(res)
bubbles(res)
qqmath(res)

# update stock object with assessment results
stkqd <- hke + fit
wireframe(data~year+age, data=harvest(stkqd))
plot(stkqd)+ ggtitle("Stock summary")

jet.colors <-  colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan","#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
z <- as.matrix(harvest(stkqd)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))
plot3d(surface3d(z = z,  y = y , x = x, type = "n"))
surface3d(z = z,  y = y , x = x, col = jet.colors(100))


# N 3D
wireframe(data ~ age + year, data = as.data.frame(stock.n(stkqd)), drape = TRUE, main="Population", screen = list(x = -60, y= - 45))

# C 3D
wireframe(data ~ age + year, data = as.data.frame(catch.n(stkqd)), drape = TRUE, main="Catches", screen = list(x = -60, y= - 45))

# F 3D
wireframe(data ~ age + year, data = as.data.frame(harvest(stkqd)), drape = TRUE, main="Harvest", screen = list(x = -60, y= - 45))

xyplot(data~age,groups=year,type=c("l","p"),data=stkqd@harvest)
# xyplot(data~age,groups=year,type=c("l","p"),data=stkqd@catch.n[,age=c(5:7)])

# Explore how well the model is predicitng the catches
plot(fit, hke)

# Explore how well the model is predicitng survey abundances
plot(fit, hke.idx)

# Individual indexes can be called with
plot(fit, hke.idx[1])
plot(fit, hke.idx[2])
plot(fit, hke.idx[3])
plot(fit, hke.idx[4])

#To get information about the likelihood fit the method fitSumm() will extract information about likelihood, number of parameters, etc, and the methods AIC() and BIC() will compute the information criteria.

#Get the fit parameters
fitSumm(fit)

AIC(fit) # Akike Information Criterion, the smaller the better, but be careful that the model is fitting something sensible!
BIC(fit) # Bayesian Information Criterion

#====================================================================
# The sca method - statistical catch-at-age
#====================================================================

# fishing mortality submodel
# separable Fay = Fa * Fy (age and year are dummy variable a simple slope over age and year)
fmodel <- ~ factor(age) + factor(year)

# fix catchability model (qmodel)
# The qmodel is a list where a catchability model needs to be set up for each index, hence here we have 3 Medits and one commercial CPUE.
qmodel <- list(~ factor(age), # q of MEDITS 10
               ~ factor(age), # q of CPUE LLS 10
               ~ factor(age), # q of MEDITS 9
               ~ factor(age)) # q of MEDITS 11


fit0 <- sca(stock = hke, indices = hke.idx, fmodel=fmodel, qmodel=qmodel)

#fit <- sca(hke, hke.idx, fmodel, qmodel)
hke.sep <- hke + fit0
plot(hke.sep)
z <- as.matrix(harvest(hke.sep)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

# diagnostics
res0 <- residuals(fit0, hke, hke.idx)
plot(res0)
bubbles(res0)
qqmath(res0)

# update stock object with assessment results
stk0 <- hke + fit0

wireframe(data~year+age, data=harvest(stk0))
plot(stk0) + ggtitle("Stock summary")


AIC(fit0)
BIC(fit0)

#  Next we may make things a bit more interesting by using an (unpenalised) thin plate spline, where we'll borrow the smoothing splines method (s()) provided by package mgcv. We're using the Hake data again, and since it has 6 ages we will use a simple rule of thumb that the spline should have fewer than 6/2 = 3 degrees of freedom, and so we opt for 3-4 degrees of freedom. We will also do the same for year and model the change in F through time as a smoother with 5 degrees of freedom.
?mgcv  
  
# smooth separable Fay = smooth Fa * smooth Fy (F at specific age to be dependent on F on the other ages. Using a smoother with k degrees of freedom)
fmodel <- ~ s(age, k = 4) + s(year, k = 5)

fit1 <- sca(hke, hke.idx, fmodel, qmodel)

hke1 <- hke + fit1

z <- as.matrix(harvest(hke1)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


# diagnostics
res1 <- residuals(fit1, hke, hke.idx)
plot(res1)
bubbles(res1)
qqmath(res1)
# update stock object with assessment results
stk1 <- hke + fit1

wireframe(data~year+age, data=harvest(stk1))
plot(stk1) + ggtitle("Stock summary")

AIC(fit1)
BIC(fit1)
#================================================================================

# interaction Fa * Fy (F is a process evolves with age and year including an interaction between the two effects)
fmodel <- ~ te(age, year, k = c(4,5))
fit2 <- sca(hke, hke.idx, fmodel, qmodel)
stk2 <- hke + fit2

z <- as.matrix(harvest(stk2)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

# diagnostics
res2 <- residuals(fit2, hke, hke.idx)
plot(res2)
bubbles(res2)
qqmath(res2)


wireframe(data~year+age, data=harvest(stk2))
plot(stk2) + ggtitle("Stock summary")

AIC(fit2)
BIC(fit2)

#================================================================================

# smooth separable + interaction Fa,Fy
fmodel <- ~ s(age, k=4) + s(year, k = 5) + te(age, year, k = c(3,3))

fit3 <- sca(hke, hke.idx, fmodel, qmodel)
stk3 <- hke + fit3

# diagnostics
res3 <- residuals(fit3, hke, hke.idx)
plot(res3)
bubbles(res3)
qqmath(res3)

z <- as.matrix(harvest(stk3)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

wireframe(data~year+age, data=harvest(stk3))
plot(stk3) + ggtitle("Stock summary")
AIC(fit3)
BIC(fit3)

#================================================================================

# interaction Fa * Fy + recruitment F extra smooth 
fmodel <- ~ te(age, year, k = c(4,5)) + s(year, k = 6, by = as.numeric(age==0))

fit4 <- sca(hke, hke.idx, fmodel, qmodel)
stk4 <- hke + fit4

# diagnostics
res4 <- residuals(fit4, hke, hke.idx)
plot(res4)
bubbles(res4)
qqmath(res4)

z <- as.matrix(harvest(stk4)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit4, hke)
plot(fit4, hke.idx[1])

AIC(fit4)
BIC(fit4)

# What happens if we compare the AIC's and BIC's of all the fitted models?

# Which model fits best? 

AIC(fit, fit1, fit2, fit3, fit4)
BIC(fit, fit1, fit2, fit3, fit4)


# EXERCISE 1
# fit a linear F model with a quadratic term at age, using in the fmodel a command like 
fmodel <- ~ age + I(age^2) + factor(year)

#  - is it improving the fit? What do the diagnostic support?

# EXERCISE 2
# fit other linear models that use transformations of age or year


# EXERCISE 3
# fit different smoothers and different degrees of freedom (see ?s)
# - does it crash when you have higher K that your data can support?


#######################
# catchability submodel
#######################
#The catchability submodel is set up the same way as the F submodel and the tools available are the same. The only difference is that the submodel is set up as a list of formulas, where each formula relates to one abundance index. For Hake in GSA 9-10-11 we have been running the model with 4 tuning indexes, so we need to set up the catchability of each tuning index.
#We'll start by fixing the F and R models and compute the fraction of the year the index relates to, which will allow us to compute catchability at age and year.


# fix fmodel, remember this is the simplest model we used before
fmodel <- ~ factor(age) + factor(year)

# The Q model allows now one catchability coefficient for each age
qmodel <- list(~ factor(age), ~ factor(age), ~ factor(age), ~ factor(age)) 

fit5 <- sca(hke, hke.idx, fmodel, qmodel)
stk5 <- hke + fit5

res5 <- residuals(fit5, hke, hke.idx)
plot(res5)

bubbles(res5)
qqmath(res5)

z <- as.matrix(harvest(stk5)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit5, hke)
plot(fit5, hke.idx)


AIC(fit5)
BIC(fit5)

#==============================================================================

# smooth age catchability
qmodel <- list(~ s(age, k=4),~ s(age, k=4),~ s(age, k=4),~ s(age, k=4))

fit6 <- sca(hke, hke.idx, fmodel, qmodel)
stk6 <- hke + fit6

res6 <- residuals(fit6, hke, hke.idx)
plot(res6)
bubbles(res6)
qqmath(res6)

z <- as.matrix(harvest(stk6)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fit6, hke)
plot(fit6, hke.idx)


AIC(fit6)
BIC(fit6)

#----------------------------------------------------------------------------------

# age-year interaction

qmodel <- list(~ te(age, year, k = c(3,5)),~ te(age, year, k = c(3,5)),~ te(age, year, k = c(3,5)),~ te(age, year, k = c(3,5)))
fit7 <- sca(hke, hke.idx, fmodel, qmodel)
stk7 <- hke + fit7

res7 <- residuals(fit7, hke, hke.idx)
plot(res7)
bubbles(res7)
qqmath(res7)

z <- as.matrix(harvest(stk7)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

plot(fit7, hke)
plot(fit7, hke.idx)


AIC(fit7)
BIC(fit7)

#-------------------------------------------------------------------------------

# smooth age catchability + year linear effect

qmodel <- list( ~ s(age, k=4) + year, ~ s(age, k=4) + year, ~ s(age, k=4) + year, ~ s(age, k=4) + year)

fit8 <- sca(hke, hke.idx, fmodel, qmodel)
stk8 <- hke + fit8

res8 <- residuals(fit8, hke, hke.idx)
plot(res8)
bubbles(res8)
qqmath(res8)

z <- as.matrix(harvest(stk8)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

plot(fit8, hke)
plot(fit8, hke.idx)

AIC(fit8)
BIC(fit8)

#
AIC(fit, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)
BIC(fit, fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8)

#-------------------------------------------------------------------------
# EXERCISE 4
# Fit an a4a sca to hake by trying the best combination of fmodel and qmodel based on AIC and residuals

# EXERCISE 5
# Compare your best model fit with the best fit from the XSA of Day 3



# If you are not happy with the results, what would you do to improve the assessment?

# 1) Try to change the plusgroup it look like the internal consistency of the cohorts is bad with more than age 4. We are going to use only trawl survey (trim LLS)

hkeplus <- setPlusGroup(hke, 4)
hke.idxplus <- hke.idx[c(1,3:4)]
hke.idxplus[[1]] <- FLIndex(index=setPlusGroup(index(hke.idxplus[[1]]), 4))
hke.idxplus[[2]] <- FLIndex(index=setPlusGroup(index(hke.idxplus[[2]]), 4))
hke.idxplus[[3]] <- FLIndex(index=setPlusGroup(index(hke.idxplus[[3]]), 4))
names(hke.idxplus)
range(hke.idxplus[[1]], c('startf', 'endf')) <- c( 0.5, 0.75)
range(hke.idxplus[[2]], c('startf', 'endf')) <- c( 0.5, 0.75)
range(hke.idxplus[[3]], c('startf', 'endf')) <- c( 0.5, 0.75)
#
#
fmodel <- ~ factor(age) + factor(year)
qmodel <- list( ~ s(age, k=4) + year, ~ s(age, k=4) + year, ~ s(age, k=4) + year)
fitplus4 <- sca(hkeplus, hke.idxplus, fmodel, qmodel)
res9 <- residuals(fitplus4, hkeplus, hke.idxplus)
plot(res9)
bubbles(res9)
qqmath(res9)
plot(fitplus4, hkeplus)
plot(fitplus4, hke.idxplus)
hkeplus <- hkeplus + fitplus4
plot(FLStocks(XSA=hke,PlusG4=hkeplus))



# 
# # 2) Try to remove the worst fitting tuning index
# # Trim the FLIndices of hke.idx to remove the MEDITS GSA 11 (we assume it is the worst).
# 
hke.idxTRIM <- hke.idx[1:3]
# interaction Fa * Fy + recruitment F extra smooth
fmodel <- ~ te(age, year, k = c(4,5)) + s(year, k = 5, by = as.numeric(age==0))
qmodel <- list( ~ factor(age), ~ factor(age), ~ factor(age))

fitTRIM <- sca(hke, hke.idxTRIM, fmodel, qmodel)
hkeTRIM <- hke + fitTRIM
# diagnostics
resT <- residuals(fitTRIM, hke, hke.idxTRIM)
plot(resT)
bubbles(resT)
qqmath(resT)

z <- as.matrix(harvest(hkeTRIM)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

plot(fitTRIM, hke)
plot(fitTRIM, hke.idxTRIM)
plot(FLStocks(XSA=hke,PlusG4=hkeplus,Trim11=hkeTRIM))

AIC(fitTRIM)
BIC(fitTRIM)

plot(hkeTRIM)


##################################################################
# stock-recruitment submodel
fmodel <- ~ s(age, k=4) + s(year, k = 5)
qmodel <- list(~ s(age, k=4))

srmodel <- ~ factor(year)
fit11 <- sca(hke, hke.idx[1], fmodel=fmodel, qmodel=qmodel, srmodel=srmodel) 
stk11 <- hke+fit11

srmodel <- ~ s(year, k=5)
fit12 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
stk12 <- hke+fit12

srmodel <- ~ ricker(CV=0.05)
fit13 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
stk13 <- hke+fit13

srmodel <- ~ bevholt(CV=0.05)
fit14 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
stk14 <- hke+fit14

srmodel <- ~ hockey(CV=0.05)
fit15 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
stk15 <- hke+fit15

srmodel <- ~ geomean(CV=0.05)
fit16 <- sca(hke, hke.idx[1], fmodel, qmodel, srmodel) 
stk16 <- hke+fit16

plot(stk16)

flqs <- FLQuants(fac=stock.n(fit11)[1], bh=stock.n(fit14)[1])

xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment models",auto.key=T)

plot(FLQuants(fac=stock.n(fit11)[1], bh=stock.n(fit14)[1]))


#====================================================================
# Predict and simulate
# To simulate we need a fit="assessment" so that the hessian/vcov is computed
#====================================================================

# To predict and simulate R uses the methods predict() and simulate(), which were implemented in FLa4a in the same fashion.

fmodel <- ~ te(age, year, k = c(3,5)) + s(year, k = 5, by = as.numeric(age==0))
qmodel <- list( ~ s(age, k=3) + year, ~ s(age, k=3) + year, ~ s(age, k=3) + year, ~ s(age, k=3) + year)

fit.sim <- sca(hke, hke.idx, fmodel, qmodel) 

fit.pred <- predict(fit.sim)
# lapply(fit.pred, names)
#--------------------------------------------------------------------
# Simulate
#--------------------------------------------------------------------
?simulate
# Simulate uses the variance-covariance matrix computed from the Hessian returned by ADMB and the fitted parameters, to parametrize a multivariate normal distribution
fits <- simulate(fit.sim, 100)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit.sim))

xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")), auto.key=T)

stks <- hke + fits
plot(stks)

#--------------------------------------------------------------------
# More models
#--------------------------------------------------------------------

# constant fishing mortality for ages older than 5
fmodel <- ~ s(replace(age, age>3, 3), k=4) + s(year, k=6)
fit <- sca(hke, hke.idx, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

# the same model with to two periods
fmodel <- ~s(age, k = 3, by = breakpts(year, 2008))
fit <- sca(hke, hke.idx, fmodel=fmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))



# #--------------------------------------------------------------------
# # Working with covariates
# #--------------------------------------------------------------------
library(FLCore)
data(ple4)
data(ple4.indices)
fit <- sca(ple4, ple4.indices)

# In linear model one can use covariates to explain part of the variance observed on the data that the ’core’ model does not explain. The same can be done in the a4a framework. The example below uses the North Atlantic Oscillation (NAO) index to model recruitment

nao <- read.table("https://www.esrl.noaa.gov/psd/data/correlation/nao.data",
                  skip = 1, nrow = 62, na.strings = "-99.90")
dnms <- list(quant = "nao", year = 1948:2017, unit = "unique", season = 1:12,
             area = "unique")
nao <- FLQuant(unlist(nao[, -1]), dimnames = dnms, units = "nao")
nao <- seasonMeans(trim(nao, year = dimnames(stock.n(ple4))$year))
nao <- as.numeric(nao)
srmod <- ~ nao
fit2 <- sca(ple4, ple4.indices[1], qmodel = list(~s(age, k = 4)), srmodel = srmod)
flqs <- FLQuants(simple = stock.n(fit)[1], covar = stock.n(fit2)[1])
xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates", auto.key=T)

# In a second model we’re using the NAO index not to model recruitment directly but to model one of the parameters of the S/R function

srmod <- ~ricker(a = ~nao, CV = 0.1)
fit3 <- sca(ple4, ple4.indices[1], qmodel = list(~s(age, k = 4)), srmodel = srmod)
flqs <- FLQuants(simple = stock.n(fit)[1], covar = stock.n(fit3)[1])
xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment model with covariates", auto.key=T)

#--------------------------------------------------------------------
# External weigthing of likelihood components
#--------------------------------------------------------------------

# data(hke)
# data(hke.idx)
# 
# stk <- hke
# idx <- hke.idx[1]
# # variance of observed catches
# varslt <- catch.n(stk)
# varslt[] <- 1
# catch.n(stk) <- FLQuantDistr(catch.n(stk), varslt) # show: remove var
# # variance of observed indices
# varslt <- index(idx[[1]])
# varslt[] <- 0.05
# index.var(idx[[1]]) <- varslt
# 
# # run
# fit <- a4aSCA(hke, hke.idx[1], vmodel=list(~1, ~1))
# fit1 <- a4aSCA(stk, idx, vmodel=list(~1, ~1)) 
# 
# flqs <- FLQuants(nowgt=stock.n(fit), extwgt=stock.n(fit1))
# 
# xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Likelihood weighting", scales=list(y=list(relation="free")), auto.key=keylst)
# 
# #--------------------------------------------------------------------
# # Assessing ADMB files
# #--------------------------------------------------------------------
# 
# fit1 <- a4aSCA(stk, idx, fmodel, qmodel, srmodel, n1model, vmodel=list(~1, ~1), wkdir="mytest") 
# 

#--------------------------------------------------------------------
# WKSAM exercise
#--------------------------------------------------------------------
# fits <- simulate(fit, 25)
# stk <- hke + fits
# 
# fits2 <- a4aSCA(stk, hke.idx[1], fmodel, qmodel, srmodel, fit="MP")  
# flqs <- FLQuants(fit=stock.n(fit), repl=stock.n(fits2))
# xyplot(data~year|age, groups=qname, data=flqs, type="l", scales=list(y=list(relation="free")), auto.key=keylst)
# 
#--------------------------------------------------------------------
# Exercise 06
#--------------------------------------------------------------------

# Assess your stock or hke (don't forget to simulate)

# Fit a periodic function to recruitment

# Fit a logistic to F