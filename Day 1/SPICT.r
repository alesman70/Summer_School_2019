#
# Day 2 Author Chato Osio

#  Surplus Production in Continuous-Time (SPICT)
library(spict)

# Data Structure

# Build spict stock

# Let's work with a good time series for anchovy in GSA Ane6.
# What is available: 
  # catch since 1945
  # biomass index since 2002
  # effort index for Purse seine in kw*Days and fishing days


# Load the .csv file, but first set working directory to where your file is

setwd("C:/Users/danai/Dropbox/FAO/Module II/DAY 1")
ane <- read.csv("ANE_06_nonAge.csv")

# look at the file, what do we have there?
View(ane)

# Assemble stock

ane6 <- vector("list")

# Import Catch data (Landings plus Discards)

ane6$obsC  <- ane$catch  #catch observations
ane6$timeC <- ane$year   # time of catch observations

ane6$obsC <- c(ane6$obsC, 17830.4) # adding an extra value to update the stock
ane6$timeC <- c(ane6$timeC, 2016)  # adding an extra value to update the year


# TUNING INDEXES
# Pick surveys index, in this case an acoustic biomass index

ane6$timeI <- ane$year[59:71] # Index 1
ane6$obsI <- ane$index[59:71]

ane6$obsI <- c(ane6$obsI, 67910.3)
ane6$timeI <- c(ane6$timeI, 2016)


# Inspect the file
ane6

# Plot your data

# x11()
plotspict.data(ane6) # Notice color coding of the month! 



#Plot inital guesses on the model initial values

plotspict.ci(ane6)

#The two top plots come from plotspict.data, with the dashed horizontal line representing a guess of MSY.
#This guess comes from a linear regression between the index and the catch divided by the index (middle row,left). This regression is expected to have a negative slope. A similar plot can be made showing catch versus catch/index (middle row, right) to approximately find the optimal effort (or effort proxy). The proportional increase in the index as a function of catch (bottom row, right) should show primarily positive increases in index at low catches and vice versa. Positive increases in index at large catches could indicate model violations (Source SPICT Vignette)

# Fit base model
ane6fit <- fit.spict(ane6)


# Now what ? Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

# Explore convergence
capture.output(summary(ane6fit))[1:4]

# Model converged, seems ok we can proceed with further diagnostics.

# There are various objects that are stored in the fitted object

names(ane6fit)

# Calculate residuals and main diagnostics

ane6fit_diagn <- calc.osa.resid(ane6fit)

plotspict.diagnostic(ane6fit_diagn)

# Retrospective analysis
# Diagnostics, run it by taking away the last 3 years, one at a time

ane6fit_retro <- retro(ane6fit, nretroyear = 3)

# now plot it!
plotspict.retro(ane6fit_retro)

# So the model fits well, diagnostics are good, we can have a look at the final results.

# Fit Summary
summary(ane6fit)

# x11()
plot(ane6fit) 

# To explore in more details
par(mfrow=c(3, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(ane6fit)
plotspict.ffmsy(ane6fit, qlegend=FALSE)
plotspict.catch(ane6fit, qlegend=FALSE)
plotspict.fb(ane6fit, man.legend=FALSE)
plotspict.tc(ane6fit)


#########################################################################################
# Exercise 1

# 1 Trim catch time series to start the year when the index starts, fit the best model you can
#  - does it converge?
#  - if yes, does it change the paramters and the perception of the stocks ?

############################################################################################################################

# re-run the assessment by assigning the correct timing of the surveys.

# First reload the stock, we truncated it in the prior exercise
ane6 <- vector("list")

# Import Catch data (Landings plus Discards)
ane6$obsC  <- ane$catch  #catch observations
ane6$timeC <- ane$year   # time of catch observations
ane6$obsC <- c(ane6$obsC, 17830.4) # adding an extra value to update the stock
ane6$timeC <- c(ane6$timeC, 2016)  # adding an extra value to update the year

# TUNING INDEXES
ane6$timeI <- ane$year[59:71] # Index 1
ane6$obsI <- ane$index[59:71]
ane6$obsI <- c(ane6$obsI, 67910.3)
ane6$timeI <- c(ane6$timeI, 2016)



# Check the timing of when your survey was performed. 
# In this case the early part of the survey was in month 11-12, the last years in 6-7
# Since the model needs to account for growth, the weight of the fish in the survey needs to be acounted for
# Add to the year the fraction of the month e.g. 11.5/12 = 0.95 and 6.5/12 = 0.54

ane6$timeI[1:5] <- ane6$timeI[1:5] + 0.95
ane6$timeI[6:14] <- ane6$timeI[6:14] + 0.54

ane6$eulertype <- "soft"

# how does it look? 

plotspict.data(ane6)

ne6fitTA <- fit.spict(ane6)


# Explore convergence
capture.output(summary(ne6fitTA))[1:4]

#Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

ane6fitTA_diagn <- calc.osa.resid(ne6fitTA)

plotspict.diagnostic(ane6fitTA_diagn)


# Diagnostics, run it by taking away the last 3 years, one at a time
###### D: Again it throws an error if you put 4 years in the retro
# ane6fitTA_retro <- retro(ne6fitTA, nretroyear = 3) does not converge


plotspict.retro(ane6fitTA_retro)


# Fit Summary
summary(ne6fitTA)

# x11()
plot(ne6fitTA)


# So what do you think, is this a better fit than the previous one?


####################################################
# SPICT ASSESSMENT NEP 17-18                    ####
####################################################
library(ggplot2)
library(plyr)
library(spict)
library(reshape2)
library(data.table)

# Data Structure

# Let's work with a good time series for norway lobster in GSAs1718.
# What is available: 
# catch since 1970
# biomass index since 1994

# Load data ####

# LANDINGS
lands17_18 <- fread("landings17_18.csv")
names(lands17_18) <- c("year","landings")

# look at the file, what do we have there?
View(lands17_18)

# Tuning Index MEDITS
MEDITS <- fread("MEDITS17_18.csv")
names(MEDITS) <- c("index","year")

# look at the file, what do we have there?
View(MEDITS)

# Tuning from older periods
nep_pomo <- fread("nep_pomo.csv")
View(nep_pomo)
froglia <- fread("froglia.csv")
View(froglia)


## Build spict stock
# In this case things are more complicated as we want 3 indexes
nep1718 <- vector("list")

# Import Catch data (basically only landings because there isn't discard for this species)

nep1718$obsC  <- lands17_18$landings #catch observations
nep1718$timeC <- lands17_18$year # time of catch observations

# TUNING INDEXES
# Pick surveys index, in this case a trawl survey biomass index (MEDITS)
nep1718$obsI <- list()
nep1718$obsI[[1]] <- froglia$kh_hour_m
nep1718$obsI[[2]] <- nep_pomo$CPUE_NEP_Combined
nep1718$obsI[[3]] <- MEDITS$index[MEDITS$year > 1993]

nep1718$timeI <- list()
nep1718$timeI[[1]]= as.numeric(froglia$year)
nep1718$timeI[[2]]=nep_pomo$year
nep1718$timeI[[3]]=MEDITS$year[MEDITS$year > 1993]

# Inspect the file
nep1718

# Plot your data
plotspict.data(nep1718) # Notice color coding of the month! 

#Plot inital guesses on the model initial values
plotspict.ci(nep1718)


# Fit base model
nep1718fit <- fit.spict(nep1718)

# Now what ? Check list:
#  - did it converge?
#  - how is it fitting?

# Let's have a look at the Diagnostics First

# Explore convergence
capture.output(summary(nep1718fit))[1:4]

# Calculate residuals and main diagnostics
nep1718fit_diagn <- calc.osa.resid(nep1718fit)

plotspict.diagnostic(nep1718fit_diagn)


# Retrospective analysis
# Diagnostics, run it by taking away the last 4 years, one at a time
nep1718fit_retro <- retro(nep1718fit, nretroyear = 4)


# now plot it!
plotspict.retro(nep1718fit_retro)



# x11()
plot(nep1718fit)
par(mfrow=c(1,1))
plotspict.f(nep1718fit,rel.axes = F,rel.ci = F,stamp ="")
plotspict.bbmsy(nep1718fit)
plotspict.ffmsy(nep1718fit)
plotspict.tc(nep1718fit)
plotspict.biomass(nep1718fit)
plotspict.catch(nep1718fit)
plotspict.fb(nep1718fit)

summary(nep1718fit)
capture.output(summary(nep1718fit))


# So the model fits quite well, diagnostics are good, we can have a look at the final results.

# Fit Summary
summary(nep1718fit)

# x11()
plot(nep1718fit)

par(mfrow=c(3, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(nep1718fit)
plotspict.ffmsy(nep1718fit, qlegend=FALSE)
plotspict.catch(nep1718fit, qlegend=FALSE)
plotspict.fb(nep1718fit, man.legend=FALSE)
plotspict.tc(nep1718fit)
# x11()
plotspict.ffmsy(nep1718fit, qlegend=FALSE)



############################################################################################################################





####################################################################
# Exercise 2
# Production models can take also time series of effort, but many caveats should be made when using fishing effort! 

# Run the model with also effort
# Take the prior list generated and add effort to a new slot of the list
ane6effort <- ane6

ane6effort$obsE <- ane$nominal_effort[59:71]
ane6effort$timeE <- ane$year[59:71]

# EXERCIsE 2

# 1) Run a spict model with fishing effort

# 2) Is it converging? 

# 3) How are the diagnostics? 

# 4) Is the fit better with effort or with the biomass index from acoustic surveys?


########################################################################################

# EXERCIsE 3


# Robust estimation for effort (or catch), helps reduce influence of idividual data points on model fit or CIs.

# We think that there might be some problems with the effort data, in particular with the estimate of 2007

# 1) Turn on robust estimation
ane6effort$robflage <- 1 # here we turn on robust estimation on effort
# Rerun the assessment, does the fit improve?





# 2) remove the value of fishing effort in 2007 and rerun the assessment, is it better?




