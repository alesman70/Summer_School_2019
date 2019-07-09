#--------------------------------------------------------------------------------------------------------------------
# HOW TO RUN A CATCH FORECAST IN SPICT

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


# Short term forecast
#To make a catch forecast a forecast interval needs to be specified. This is done by specifying the start of the interval (inp$timepredc) and the length of the interval in years (inp$dtpredc). 

#For example, if a forecast of the annual catch of 2018 is of interest, then inp$timepredc = 2018 and inp$dtpredc = 1. 

#In addition to the forecast interval a fishing scenario needs to be specified. This is done by specifying a factor (inp$ffac) to multiply the current fishing mortality by (i.e. the F at the last time point of the time period where data are available) and the time that management should start (inp$manstart). 

#The time point of the reported forecast of biomass and fishing mortality can be controlled by setting inp$timepredi. Producing short-term forecasts entails minimal additional computing time.

ane6$manstart <- 2016  # When management will start
ane6$timepredc <- 2020 # Time when we want predicted catch
ane6$dtpredc <- 1 # Time interval in years for prediction
ane6$timepredi <- 2020
ane6$ffac <- 0.75 # Specify the fishing scenario for the forecast, in this case use a factor to  


anespict <- fit.spict(ane6)
summary(anespict)
plot(anespict)

sumspict.predictions(anespict)

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(anespict)
plotspict.ffmsy(anespict, qlegend=FALSE)
plotspict.catch(anespict, qlegend=FALSE)
plotspict.fb(anespict, man.legend=FALSE)


# BUILD MANAGEMENT SCENARIOS
# The package has a function that runs several predefined management scenarios, which can be presented in a forecast table.

res <- manage(anespict)
df <- mansummary(res)

df

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(res)
plotspict.ffmsy(res, qlegend=FALSE)
plotspict.catch(res, qlegend=FALSE)
plotspict.fb(res, man.legend=FALSE)

# table of forecast for  +1 year
mansummary(res, ypred=1, include.unc = FALSE)

# table of forecast for  +4 year, in our case the period 2019-2020
mansummary(res, ypred=4, include.unc = FALSE)



##################################################################################
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



ane6$manstart <- 2016  # When management will start
ane6$timepredc <- 2020 # Time when we want predicted catch
ane6$dtpredc <- 1 # Time interval in years for prediction
ane6$timepredi <- 2020
#ane6$ffac <- 0.75 # Specify the fishing scenario for the forecast, in this case use a factor to  multiply F F 0.25


anespict2 <- fit.spict(ane6)
summary(anespict2)
plot(anespict2)

sumspict.predictions(anespict2)

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(anespict2)
plotspict.ffmsy(anespict2, qlegend=FALSE)
plotspict.catch(anespict2, qlegend=FALSE)
plotspict.fb(anespict2, man.legend=FALSE)


# BUILD MANAGEMENT SCENARIOS
res2 <- manage(anespict2)
df2 <- mansummary(res2)

df2

par(mfrow=c(2, 2), mar=c(4, 4.5, 3, 3.5))
plotspict.bbmsy(res2)
plotspict.ffmsy(res2, qlegend=FALSE)
plotspict.catch(res2, qlegend=FALSE)
plotspict.fb(res2, man.legend=FALSE)

# table of forecast for  +1 year
mansummary(res2, ypred=1, include.unc = FALSE)

# table of forecast for  +4 year, in our case the period 2019-2020
mansummary(res2, ypred=4, include.unc = FALSE)



