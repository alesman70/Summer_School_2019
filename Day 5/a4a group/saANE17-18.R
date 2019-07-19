#run library

library(rgl)
library(FLa4a)
library(diagram)
library(ggplotFL)
library(FLCore)
library(FLAssess)
library(FLash)
library(FLXSA)
library(plyr)
library(FLBRP)

#load data

ane <- ANCHOVY
ane.idx <- ANCHOVY.tun 
plot(ane)


#put units and missing things
harvest.spwn(ane)<-0.5
m.spwn(ane)<-0.5
harvest(ane)

landings(ane)+discards(ane)
catch.n(ane)<-landings(ane)+discards(ane)

landings(ane)#landings
units(landings(ane)) <- "t"

catch(ane)#catch
units(catch(ane)) <- "t"

catch.n(ane) #catch number by age
units(catch.n(ane)) <- "1000"

catch.wt(ane) #catch weight by age
units(catch.wt(ane)) <- "kg"

stock.n(ane)  #stock number by age - Not filled in yet
units(stock.n(ane)) <- "1000"

stock.wt(ane)#stock weight by age
units(stock.wt(ane)) <- "kg"

mat(ane)#maturity by age
units(mat(ane)) <- ""

m(ane)#natural mortality by age
units(m(ane)) <- "m"

harvest(ane)#fishing mortality by age  - Not filled in yet
units(harvest(ane)) <- "f"

range(ane)
range(ane)[c("minfbar","maxfbar")]

#first plot

plot(catch(ane)) + xlab("Year") + ylab("Catch (t)")

catches<-as.data.frame(catch.n(anetrim), cohort=TRUE)

ggplot(catches, aes(age,data,color=factor(year))) + geom_line(size=1.1) + ggtitle("Age structure catches 2009-2015") + xlab("Age") + ylab("N (thousands)") + scale_colour_discrete(name  ="Year")


ggplot(catches, aes(year,data,color=factor(age))) + geom_line(size=1.1) + ggtitle("Catches ANE") + xlab("Year") + ylab("N (thousands)") + scale_colour_discrete(name  = "Age")

ggplot(catches, aes(year,data,color=factor(cohort))) + geom_line(size=1.1) + ggtitle("Cohorts ANE") + xlab("Year") + ylab("N (thousands)") + scale_colour_discrete(name  = "Cohort")

ggplot(catches, aes(age,data,color=factor(cohort))) + geom_line(size=1.1) + ggtitle("Cohorts ANE") + xlab("Age") + ylab("N (thousands)") + scale_colour_discrete(name  = "Cohort")

ggplot(catches, aes(age,data)) + geom_line(size=1.1) + ggtitle("Cohorts ANE") + xlab("Age") + ylab("N (thousands)") + facet_wrap(~cohort)

index(ane.idx[[1]])


#running the model

anetrim<-window(ane,start=2009,end=2015)
range(anetrim)["minfbar"]<-1
range(anetrim)["maxfbar"]<-3


catch(anetrim)
fit <- sca(anetrim, ane.idx)

fmodel<- ~ factor(age) +  factor(year)

qmodel <- list(~factor(age)) 

fit0 <- sca(stock = anetrim, indices = ane.idx, fmodel=fmodel, qmodel=qmodel, fit="MP")

harvest(fit0)

#fit <- sca(ane, ane.idx, fmodel, qmodel)
ane.sep <- anetrim + fit0
plot(ane.sep)
z <- as.matrix(harvest(ane.sep)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

# diagnostics
res0 <- residuals(fit0, anetrim, ane.idx)
plot(res0)
bubbles(res0)
qqmath(res0)

# update stock object with assessment results
stk0 <- anetrim + fit0

wireframe(data~year+age, data=harvest(stk0), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))

plot(stk0) + ggtitle("Stock summary")

# Explore how well the model is predicitng the catches
plot(fit0, anetrim)

AIC(fitnew)
BIC(fitnew)


# here we are setting a rmodel 
rmodel <- ~ bevholt(CV=0.05)

fit0 <- sca(stock = anetrim, indices = ane.idx, fmodel=fmodel, qmodel=qmodel, srmodel=rmodel, fit="MP")

ane.sep <- anetrim + fit0
plot(ane.sep)
z <- as.matrix(harvest(ane.sep)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

# diagnostics
res0 <- residuals(fit0, anetrim, ane.idx)
plot(res0)
bubbles(res0)
qqmath(res0)

#####STF

max_yr_stk <- range(ane.sep )["maxyear"]
stf_years <- c(max_yr_stk+1,max_yr_stk+2,max_yr_stk+3)
no_stf_years <- length(stf_years)

stk_brp <- brp(FLBRP(ane.sep ))
ref<-refpts(stk_brp)
f01 <- c(refpts(stk_brp)["f0.1","harvest"])
f01

write.csv(ref, file="refpoint.csv")

no_stk_years <- dim(rec(ane.sep))[2]
no_fbar_years <- 3 # Or set your own as appropriate
fbars <- fbar(ane.sep)[,(no_stk_years - no_fbar_years + 1):no_stk_years]
fbar_status_quo <- exp(mean(log(c(fbars))))
fbar_status_quo
ssb(ane.sep)
catch(ane.sep)

stf_stk <- stf(ane.sep, nyears = no_stf_years, wts.nyears = 3)


no_rec_years <- 3 
recs <- rec(ane.sep)[,(no_stk_years - no_rec_years + 1):no_stk_years]
mean_rec <- exp(mean(log(c(recs))))


fbar_multiplier <- seq(from = 0, to = 2, by = 0.1)
length(fbar_multiplier)

fbar_scenarios <- cbind(rep(fbar_status_quo,length(fbar_multiplier)),
                        fbar_multiplier*fbar_status_quo,
                        fbar_multiplier*fbar_status_quo)

fbar_scenarios <- rbind(fbar_scenarios, c(fbar_status_quo,f01,f01))

stf_results <- matrix(NA,nrow = nrow(fbar_scenarios),ncol = 10) 

colnames(stf_results) <- c('Ffactor','Fbar',
                           paste0('Catch',max_yr_stk),
                           paste0('Catch',max_yr_stk+1), 
                           paste0('Catch',max_yr_stk+2),
                           paste0('Catch',max_yr_stk+3),
                           paste0('SSB',max_yr_stk+2),
                           paste0('SSB',max_yr_stk+3),
                           paste0('SSB_change_',max_yr_stk+2,'-',max_yr_stk+3,'(%)'),
                           paste0('Catch_change_',max_yr_stk,'-',max_yr_stk+2,'(%)'))

# Store the FLStock each time
stk_stf <- FLStocks()
# Loop over the scenarios
for (scenario in 1:nrow(fbar_scenarios)) {
  cat("Scenario: ", scenario, "\n")
  # Make a target object withe F values for that scenario
  ctrl_target <- data.frame(year = stf_years,
                            quantity = "f",
                            val = fbar_scenarios[scenario,])
  # Set the control object - year, quantity and value for the moment
  ctrl_f <- fwdControl(ctrl_target)
  # Run the forward projection. We include an additional argument, maxF.
  # By default the value of maxF is 2.0
  # Here we increase it to 10.0 so that F is not limited
  stk_stf_fwd <- fwd(stf_stk, ctrl = ctrl_f, sr = list(model="mean", params=FLPar(a = mean_rec)), maxF = 10.0)
  ## Check it has worked - uncomment out to check scenario by scenario
  #plot(stk_stf_fwd)
  # Store the result - if you want to, comment out if unnecessary
  stk_stf[[scenario]] <- stk_stf_fwd
  
  # Fill results table
  stf_results[scenario,1] <- fbar_scenarios[scenario,2] / fbar_scenarios[scenario,1] # fbar status quo ratio
  stf_results[scenario,2] <- fbar(stk_stf_fwd)[,ac(max_yr_stk+3)] # final stf year
  stf_results[scenario,3] <- catch(stk_stf_fwd)[,ac(max_yr_stk)] # last 'true' year
  stf_results[scenario,4] <- catch(stk_stf_fwd)[,ac(max_yr_stk+1)] # 1st stf year
  stf_results[scenario,5] <- catch(stk_stf_fwd)[,ac(max_yr_stk+2)] # 2nd stf year
  stf_results[scenario,6] <- catch(stk_stf_fwd)[,ac(max_yr_stk+3)] # final stf year
  stf_results[scenario,7] <- ssb(stk_stf_fwd)[,ac(max_yr_stk+2)] # 2nd stf year
  stf_results[scenario,8] <- ssb(stk_stf_fwd)[,ac(max_yr_stk+3)] # final stf year
  # Change in SSB
  stf_results[scenario,9] <- (ssb(stk_stf_fwd)[,ac(max_yr_stk+3)]-ssb(stk_stf_fwd)[,ac(max_yr_stk+2)])/ssb(stk_stf_fwd)[,ac(max_yr_stk+2)]*100 # change in ssb in last two stf years
  stf_results[scenario,10] <- (catch(stk_stf_fwd)[,ac(max_yr_stk+2)]-catch(stk_stf_fwd)[,ac(max_yr_stk)])/catch(stk_stf_fwd)[,ac(max_yr_stk)]*100 # change in catch from true year, to 2nd to last stf year
}

#}
# Look at the table of results
stf_results
# export this if necessary
write.csv(stf_results, file="stf_results2015_2017.csv")

# Plotting
# Plotting is not necessary for the report but here is a crude one anyway
names(stk_stf) <- c("0","0.1", "0.2", "0.3", "0.4", "0.5", "0.6", "0.7", "0.8", "0.9", "1.0", "1.1", "1.2", "1.3", "1.4", "1.5", "1.6", "1.7", "1.8", "1.9", "2.0", "F01" )
plot(window(stk_stf, start=max_yr_stk, end=max_yr_stk+3))

#reference point




