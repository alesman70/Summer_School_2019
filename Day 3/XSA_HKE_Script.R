#' ---
#' title: "XSA in FLR"
#' author: "Alessandro Ligas & Alessandro Orio"
#' date: "July 15th, 2018"
#' ---


# This document is based on:
# Stock Assessment Using Tuned VPA. Rob Scott & Clara Ulrich (December 2010) and
# Virtual Population analysis using eXtended Survivor Analysis.
#Dorleta Garcia & Alessandro MANNINI (March 2017)

# Removing previous objects and clean environment
rm(list=ls())

# Use underneath code to open FLR installation window
# utils::install.packages(repos="http://flr-project.org/R")

#### Load libraries ####
library(FLCore)
library(FLAssess)
library(FLXSA)
library(ggplotFL)
#### Load data ####
load("HKE_09_10_11_stk.RData")
HKE <- stk
load("HKE_09_10_11_idx.RData")
HKE.idx <- flq.idx

################################
#### Explore data structure ####
################################

#### Exploring commercial data ####

landings(HKE)#landings
units(landings(HKE)) <- "t"

catch(HKE)#catch
units(catch(HKE)) <- "t"

catch.n(HKE) #catch number by age
units(catch.n(HKE)) <- "1000"

catch.wt(HKE) #catch weight by age
units(catch.wt(HKE)) <- "kg"

stock.n(HKE)  #stock number by age - Not filled in yet
units(stock.n(HKE)) <- "1000"

stock.wt(HKE)#stock weight by age
units(stock.wt(HKE)) <- "kg"

mat(HKE)#maturity by age
units(mat(HKE)) <- ""

m(HKE)#natural mortality by age
units(m(HKE)) <- "m"

harvest(HKE)#fishing mortality by age  - Not filled in yet
units(harvest(HKE)) <- "f"

range(HKE)
range(HKE)[c("minfbar","maxfbar")]



############################
### Exploring data loaded ##
############################

plot(catch(HKE)) + xlab("Year") + ylab("Catch (t)")

catches<-as.data.frame(catch.n(HKE), cohort=TRUE)

ggplot(catches, aes(age,data,color=factor(year))) + geom_line(size=1.1) + ggtitle("Age structure catches") + xlab("Age") + ylab("N (thousands)") + scale_colour_discrete(name  ="Year")


ggplot(catches, aes(year,data,color=factor(age))) + geom_line(size=1.1) + ggtitle("Catches HKE GSA9-11") + xlab("Year") + ylab("N (thousands)") + scale_colour_discrete(name  = "Age")

# To plot cohorts in time
ggplot(catches, aes(year,data,color=factor(cohort))) + geom_line(size=1.1) + ggtitle("Cohorts HKE GSA9-11") + xlab("Year") + ylab("N (thousands)") + scale_colour_discrete(name  = "Cohort")

ggplot(catches, aes(age,data,color=factor(cohort))) + geom_line(size=1.1) + ggtitle("Cohorts HKE GSA9-11") + xlab("Age") + ylab("N (thousands)") + scale_colour_discrete(name  = "Cohort")

ggplot(catches, aes(age,data)) + geom_line(size=1.1) + ggtitle("Cohorts HKE GSA9-11") + xlab("Age") + ylab("N (thousands)") + facet_wrap(~cohort)


# Set fbar in the stock 

range(HKE)["minfbar"] <- 0
range(HKE)["maxfbar"] <- 3


# Set the plus group in the stock
HKE <- setPlusGroup(HKE, 6)

# What happens if we set a lower plus group?

HKE4 <- setPlusGroup(HKE, 4)

catch.n(HKE4)
catch.n(HKE)



ggplot(as.data.frame(catch.n(HKE4)), aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Age structure catches")+xlab("Age") +ylab("N (thousands)")+ scale_colour_discrete(name  ="Year")

# and now let's remove our HKE4 object
rm(HKE4)


#### Exploring survey index ####

names(HKE.idx)

index(HKE.idx[[1]])
index(HKE.idx[[2]])
index(HKE.idx[[3]])
index(HKE.idx[[4]])

# Let's name the four surveys:
name(HKE.idx[[1]]) <- "MEDITS_SA10"
name(HKE.idx[[2]]) <- "CPUE_LLS_SA10"
name(HKE.idx[[3]]) <- "MEDITS_SA9"
name(HKE.idx[[4]]) <- "MEDITS_SA11"



Med_SA10 <- as.data.frame(index(HKE.idx[[1]]))

ggplot(Med_SA10, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Age structure Med_SA10")+xlab("Age") +ylab("N/km2")+ scale_colour_discrete(name  ="Year")

LLS_SA10 <- as.data.frame(index(HKE.idx[[2]]))

ggplot(LLS_SA10, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Age structure LLS_SA10")+xlab("Age") +ylab("CPUE")+ scale_colour_discrete(name  ="Year")

Med_SA9 <- as.data.frame(index(HKE.idx[[3]]))

ggplot(Med_SA9, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Age structure Med_SA9")+xlab("Age") +ylab("N/km2")+ scale_colour_discrete(name  ="Year")

Med_SA11 <- as.data.frame(index(HKE.idx[[4]]))

ggplot(Med_SA11, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Age structure Med_SA11")+xlab("Age") +ylab("N/km2")+ scale_colour_discrete(name  ="Year")


# Set the plus group in the index (if needed!)
HKE.idx[[1]] <- FLIndex(index=setPlusGroup(index(HKE.idx[[1]]), 5))


################################################
### Cohorts consistence in the tuning fleets ###
################################################

ggplot(Med_SA10, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("abundance in the MEDITS SA10 survey by age")+xlab("Year") +ylab("N (thousands)")+ scale_colour_discrete(name  ="Age")

ggplot(Med_SA10, aes(year,data))+geom_line(size=1.1)+facet_wrap(~age, scales = "free_y")+ggtitle("abundance in the MEDITS SA10 survey by age")+xlab("Year") +ylab("N (thousands)")+ scale_colour_discrete(name  ="Age")


plot(HKE.idx[[1]], type="internal",main="Cohorts consistence in the MEDITS SA10 survey")


#if you have more than one index


ggplot(Med_SA9, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("abundance in the MEDITS SA9 survey by age")+xlab("Year") +ylab("N/km2")+ scale_colour_discrete(name  ="Age")

ggplot(Med_SA9, aes(year,data))+geom_line(size=1.1)+facet_wrap(~age, scales = "free_y")+ggtitle("abundance in the MEDITS SA9 survey by age")+xlab("Year") +ylab("N/km2")+ scale_colour_discrete(name  ="Age")


plot(HKE.idx[[3]], type="internal",main="Cohorts consistence in the MEDITS SA9 survey")


# Exercise 01

# 1. Explore the weight at age data in our FLStock and plot them by using ggplot

# 2. Explore the abundance by age and the internal consistency of the remaining two survey indices


## We can also plot the cohort consistence of the catches. To do so we need to treat the catches as an index

catchn.fli <- FLIndex(FLQuant(NA, dimnames=list(year = dimnames(catch.n(HKE))$year,
                                                age = dimnames(catch.n(HKE))$age)))
index(catchn.fli) <- catch.n(HKE)
plot(catchn.fli, type="internal",main="Cohorts consistence in the catch")



####################
#### Tuned VPA  ####
####################

# The VPA method estimates population numbers and fishing mortalities at age by back-calculating values down each cohort. To do this, the method requires initial values of harvest for the terminal age and terminal year. These terminal values must be specified by the user prior to running the VPA. This dependency limits the usefulness of the method since the most recent, terminal, estimates are usually of main concern to fishery managers.

# Additional catch at age and effort information, derived either from a sub component of the fishery or from a fishery independent source, such as a research survey, can be used to 'tune' the assessment, as described above, and thereby obtain better estimates of fishing mortality and stock numbers in the most recent years. Several so-called ad hoc techniques for tuning a VPA have been developed. Extended Survivor Analysis (XSA) is one of the most common.

###############
#### FLXSA ####
###############

# XSA is a more sophisticated method that uses information on individual cohort sizes to estimate survivors at each age in the terminal population.
# This method requires as input catch numbers at age and indices of abundance and it retains at its core the basic VPA method. The details of the XSA method are too complex to show here, or to code individually. 

# The function FLXSA requires several arguments

# FLXSA(stock, indices, control)

# stock = An object of class FLStock.

# indices = An object of class FLIndices that contains one or more indices of catch at age and effort.

# control = An object of class FLXSA.control that specifies the model parameters.


##################################
#### The FLXSA control object ####
##################################

# The FLXSA.control object contains the user defined model settings for running an XSA analysis. It can be created in several different ways. 
# The simplest method is to accept all of the default settings by calling the FLXSA.control function without any extra arguments:

FLXSA.control()

#FLXSA.control(x=NULL, tol=1e-09, maxit=30, min.nse=0.3, fse=0.5, rage=0, qage=10, shk.n=TRUE, shk.f=TRUE, shk.yrs=5, shk.ages=5, window=100, tsrange=20, tspower=3, vpa=FALSE)

# As you can see there are a lot of different parameters you can change. Today we will have a look only at some of them.

# rage: the first age at which catchability is considered to be independent of year class strength. If the youngest age, for which tuning data is available, is chosen, all ages will be treated as having catchability independent of year class strenght. 

# qage: the age after which catchability is no longer estimated (i.e. constant)

# shk.yrs: mortality of last n years is dependent by the previous year mortality. It should be less than or equivalent to the longest time period over which the exploitation pattern is considered to have been constant.

# shk.ages: the number of ages used for the shrinkage mean. They should be customised to the dimension of the data set studied.

# fse: user defined standard error when shrinking the mean F; the lower the shrinkage the higher the fse; the higher the fse the higher the tuning signal (weight of the survey).



# Alternatively the default settings can be overwritten by specifying values at the point of creation or by overwriting them afterwards.

FLXSA.control1 <- FLXSA.control(maxit = 50, qage = 8)
FLXSA.control1 <- FLXSA.control()


################################################################################
# FLXSA Control used as best run in the HKE_9-11 official assessment EWG15-11  #
################################################################################

FLXSA.control1 <- FLXSA.control(fse=2, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

####################################################################

# Once the control object has been created, the XSA analysis can be run as a simple one-line command. The FLXSA function returns an object of class FLXSA which extends the FLAssess class. The FLXSA object contains all of the information in the FLAssess class plus additional information specific to the XSA assessment method, such as the survivors estimates and their internal and external standard errors. The control object used for the assessment is also stored in the returned FLXSA object to provide a record of what settings were used for that particular run. All of the settings in the returned control object will remain the same except for the "maxit" slot that contains the maximum number of iterations for the analysis. This value will be overwritten with the actual number of iterations taken to reach convergence, if indeed the model had converged before the maximum number initially specified.



#####################
###### Run XSA ######
#####################

#### TUNING using all indices #####
HKE.xsa <- FLXSA(HKE, HKE.idx, FLXSA.control1)

### what happened?

range(HKE.idx[[1]])

range(HKE.idx[[1]], c("startf","endf")) <- c(0.66,0.75)

### Try again
HKE.xsa <- FLXSA(HKE, HKE.idx, FLXSA.control1)

### Check that it has converged i.e. it did not hit our maxit parameter

HKE.xsa@control #it converged after 122 runs



HKE.new_xsa <- HKE + HKE.xsa


#### Plotting main results #####

# Let's have a look at the FINAL OUTPUTS (this is just for better understanding the results; before looking at the outputs, we should look at the diagnostics!!!)

plot(HKE.new_xsa)
ggsave("HKE.new_xsa.png",last_plot())

# Fbar(0-3)
fbar(HKE.new_xsa)

# Spawning stock biomass
ssb(HKE.new_xsa)

# Recruitment
rec(HKE.new_xsa)

# Fishing mortality (F) by age
harvest(HKE.new_xsa)
F_hke <- as.data.frame(harvest(HKE.new_xsa))
write.csv(F_hke, file="F HKE.csv")

# Before concentrating too much on the results of the assessment it is advisable to first investigate some of the model diagnostics.


#####################
#### Diagnostics ####
#####################

# There are many diagnostic checks that one might be interested in conducting to examine the model fit. 

diagnostics(HKE.xsa)


#### RESIDUALS BUBBLE PLOTS ####

surv_names<-index.res(HKE.xsa)
names(surv_names)<-c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names , main = "Log catchability residuals")




#############################
### Sensitivity analysis ####
#############################

# Sensitivity analysis on rage and qage
# rage from -1 to 1 and qage from 4 to 5

FLXSA.control.HKE <- FLXSA.control(fse=3.0, rage=1, qage=4, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.HKE1 <- FLXSA.control(fse=3.0, rage=1, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.HKE2 <- FLXSA.control(fse=3.0, rage=0, qage=4, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.HKE3 <- FLXSA.control(fse=3.0, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.HKE4 <- FLXSA.control(fse=3.0, rage=-1, qage=4, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.HKE5 <- FLXSA.control(fse=3.0, rage=-1, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)


#Running the assessments with different settings
HKE.xsa <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE)
HKE.xsa1 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE1)
HKE.xsa2 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE2)
HKE.xsa3 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE3)
HKE.xsa4 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE4)
HKE.xsa5 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE5)


#Add the results to the stock files
HKE.stk <- HKE+HKE.xsa
HKE.stk1 <- HKE+HKE.xsa1
HKE.stk2 <- HKE+HKE.xsa2
HKE.stk3 <- HKE+HKE.xsa3
HKE.stk4 <- HKE+HKE.xsa4
HKE.stk5 <- HKE+HKE.xsa5


stocks <- FLStocks(HKE.stk,HKE.stk1,HKE.stk2,HKE.stk3,HKE.stk4,HKE.stk5)
names(stocks) <- c("r1q4","r1q5","r0q4","r0q5","r-1q4","r-1q5")
plot(stocks)

# in case you want to save the plot
#ggsave("RageQage.png",last_plot())

# The different runs are not that different so it is hard from this plot to decide the best parameters to use. Let's check the residuals

###Residuals by fleet
surv_names <- index.res(HKE.xsa)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r1q4")

surv_names <- index.res(HKE.xsa1)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r1q5")

surv_names <- index.res(HKE.xsa2)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r0q4")

surv_names <- index.res(HKE.xsa3)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r0q5")

surv_names <- index.res(HKE.xsa4)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r-1q4")

surv_names <- index.res(HKE.xsa5)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r-1q5")




# Let's keep the best combination for rage and qage (rage=0 and qage=5), and do a sensitivity analysis on shrinkage age.
# Fix qage and rage and change the values for shk.ages from 1 to 3

FLXSA.control.stk <- FLXSA.control(fse=3.0, rage=0, qage=5, shk.yrs=3, shk.ages=1, maxit = 2000)

FLXSA.control.stk1 <- FLXSA.control(fse=3.0, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.stk2 <- FLXSA.control(fse=3.0, rage=0, qage=5, shk.yrs=3, shk.ages=3, maxit = 2000)

#Running the assessments with different settings

HKE.xsa <- FLXSA(HKE.stk, HKE.idx, FLXSA.control.stk)
HKE.xsa1 <- FLXSA(HKE.stk, HKE.idx, FLXSA.control.stk1)
HKE.xsa2 <- FLXSA(HKE.stk, HKE.idx, FLXSA.control.stk2)


#Add the results to the stock files
HKE.stk <- HKE+HKE.xsa
HKE.stk1 <- HKE+HKE.xsa1
HKE.stk2 <- HKE+HKE.xsa2

stocks1 <- FLStocks(HKE.stk,HKE.stk1,HKE.stk2)
names(stocks1) <- c("shk.ages=1","shk.ages=2","shk.ages=3")
plot(stocks1)

# to save the plot
#ggsave("ShAge.png",last_plot())

# Again the different runs look similar

###Residuals by fleet 
surv_names <- index.res(HKE.xsa)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year shk.ages=1")

surv_names <- index.res(HKE.xsa1)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year shk.ages=2")

surv_names <- index.res(HKE.xsa2)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year shk.ages=3")

# since they really look similar we can decide which one we want to use. Let's use shk.ages = 2
# Finally a sensitivity analysis on fse with values from 0.5 to 2 with a step of 0.5

### fse 0.5
FLXSA.control.HKE <- FLXSA.control(fse=0.5, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 1.0
FLXSA.control.HKE1 <- FLXSA.control(fse=1.0, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 1.5
FLXSA.control.HKE2 <- FLXSA.control(fse=1.5, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 2.0
FLXSA.control.HKE3 <- FLXSA.control(fse=2.0, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)


#Running the assessments with different settings
HKE.xsa <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE)
HKE.xsa1 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE1)
HKE.xsa2 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE2)
HKE.xsa3 <- FLXSA(HKE, HKE.idx, FLXSA.control.HKE3)

#Add the results to the stock files
HKE.stk <- HKE+HKE.xsa
HKE.stk1 <- HKE+HKE.xsa1
HKE.stk2 <- HKE+HKE.xsa2
HKE.stk3 <- HKE+HKE.xsa3


stocks <- FLStocks(HKE.stk,HKE.stk1,HKE.stk2,HKE.stk3)
names(stocks) <- c("fse0.5","fse1.0","fse1.5","fse2.0")
plot(stocks)
# ggsave("sens_fse.png",last_plot())


###Residuals by fleet 
surv_names <- index.res(HKE.xsa)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse0.5")

surv_names <- index.res(HKE.xsa1)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse1.0")

surv_names <- index.res(HKE.xsa2)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse1.5")

surv_names <- index.res(HKE.xsa3)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse2.0")

# The residuals for fse = 0.5 looks a bit better but before deciding the final value let's have a look at the retrospective analysis

###############################
### Retrospective analysis ####
###############################

# An important diagnostic check is to see how the estimated values vary as the time series of the input data changes.
# We can make use of existing R functions to apply the same assessment model to truncated time series of input data. In this example we are using window to truncate the FLStock object to the specified year range, the + operator to pass the results of the XSA into the FLStock object and the tapply function to perform this action over the year range 2012:2014.
# Note that the resulting object, called stk.retro, is of class FLStocks i.e. a list of FLStock objects, each one having a separate year range.

# A retrospective analysis should be done any time you perform a sensitivity analysis on a different parameter. In this example script, we do a retrospective analysis with the last sensitivity analysis only.


## Retrospective analysis ##
retro.years <- 2012:2014 # retro years range

HKE.stk.retro <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk,end=x)+FLXSA(window(HKE.stk,end=x),HKE.idx, FLXSA.control.HKE)))

HKE.stk.retro <- FLStocks(HKE.stk.retro)
names(HKE.stk.retro) <- as.character(unique(retro.years))
plot(HKE.stk.retro)


HKE.stk.retro1 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk1,end=x)+FLXSA(window(HKE.stk1,end=x),HKE.idx, FLXSA.control.HKE1)))

HKE.stk.retro1 <- FLStocks(HKE.stk.retro1)
names(HKE.stk.retro1) <- as.character(unique(retro.years))
plot(HKE.stk.retro1)


HKE.stk.retro2 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk2,end=x)+FLXSA(window(HKE.stk2,end=x),HKE.idx, FLXSA.control.HKE2)))

HKE.stk.retro2 <- FLStocks(HKE.stk.retro2)
names(HKE.stk.retro2) <- as.character(unique(retro.years))
plot(HKE.stk.retro2)


HKE.stk.retro3 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk3,end=x)+FLXSA(window(HKE.stk3,end=x),HKE.idx, FLXSA.control.HKE3)))

HKE.stk.retro3 <- FLStocks(HKE.stk.retro3)
names(HKE.stk.retro3) <- as.character(unique(retro.years))
plot(HKE.stk.retro3)


# By looking at the retrospective we decide to choose as final fse 0.5


############################## SUPER IMPORTANT!!!!!!! #########################
# In theory now we should start again the sensitivity analysis on rage and qage by setting our final values for fse and shk.age in it and check that the resulting best rage and qage are not different from before. We should do the same for shk.age. If we see that the final parameters still provides the best results then it is probably OK (it would be advisable to run anyway your sensitivity analyses on all the possible parameters combinations). If the best results comes from different parameters we should keep running sensitivity analyses until we find the best one. Because we have limited time we will not do that but PLEASE BE VERY THOROUGH WHEN DOING YOUR SENSITIVITY ANALYSES!! 




###### Choose the final model and plot it

plot(HKE.stk)
#ggsave("final.png",last_plot())

# Let's have a look at the FINAL OUTPUTS:

# Fbar(0-3)
fbar(HKE.stk)

# Total biomass
quantSums(stock.n(HKE.stk)*stock.wt(HKE.stk))

# Spawning stock biomass
ssb(HKE.stk)

# Recruitment
rec(HKE.stk)

# Fishing mortality (F) by age
harvest(HKE.stk)

## Total mortality
z(HKE.stk)

# Save the stock object of the BEST MODEL
save(HKE.stk, file="HKE9_10_11.xsa.Rdata")

## remember to change the file name output on the basis of your stock

# Save also the index you used to run the model
save(HKE.idx, file="HKE9_10_11xsa_idx.Rdata")



# Exercise 02

# 1. Run XSA with different Fcontrol settings (i.e. fse from 0.5 to 3, with 0.5 steps) and compare results in terms of residuals and retrospective


# Exercise 03

# 1. Change the Fbar settings (i.e. 0-2, instead of 0-3) and compare results with the original assessment


# Exercise 04

# 1. Run XSA with different  natural mortality values (costant with M = 0.3; and vectorial M = c(1.0, 0.8, 0.7, 0.5, 0.35, 0.3, 0.3)) and compare results with the original assessment 


# Exercise 05

# 1. Load the FLStock called DPS09stk and the FLIndex called DPS09idx. TIP: use the function load()

# 2. Run a sensitivity analysis on rage and qage using values ranging between -1 and 1 for rage and between 2 and 3 for qage. On shk.age ranging between 1 to 3. On fse ranging between 0.5 and 3 with steps of 0.5. Remember to check residuals and retrospective to decide the best possible model.

