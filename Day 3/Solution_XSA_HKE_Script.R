# Exercise 01

# 1. Explore the weight at age data in our FLStock and plot them by using ggplot

weights <- as.data.frame(catch.wt(HKE))

ggplot(weights, aes(age,data,color=factor(year)))+geom_line(size=1.1)+ggtitle("Mean weight at age of catches")+xlab("Age") +ylab("Kg")+ scale_colour_discrete(name  ="Year")

# This plot is not really useful. Let's plot it in a different way

ggplot(weights, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("Mean weight at age of catches")+xlab("Year") +ylab("Kg")+ scale_colour_discrete(name  ="Age")

# This type of plot is much more informative

# 2. Explore the abundance by age and the internal consistency of the remaining two survey indices

#### MEDITS SA11

ggplot(Med_SA11, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("abundance in the MEDITS SA11 survey by age")+xlab("Year") +ylab("N (thousands)")+ scale_colour_discrete(name  ="Age")

ggplot(Med_SA11, aes(year,data))+geom_line(size=1.1)+facet_wrap(~age, scales = "free_y")+ggtitle("abundance in the MEDITS SA11 survey by age")+xlab("Year") +ylab("N (thousands)")+ scale_colour_discrete(name  ="Age")


plot(HKE.idx[[4]], type="internal",main="Cohorts consistence in the MEDITS SA11 survey")


#### LLS SA10

ggplot(LLS_SA10, aes(year,data,color=factor(age)))+geom_line(size=1.1)+ggtitle("CPUE in the LLS SA10 by age")+xlab("Year") +ylab("CPUE")+ scale_colour_discrete(name  ="Age")

ggplot(LLS_SA10, aes(year,data))+geom_line(size=1.1)+facet_wrap(~age, scales = "free_y")+ggtitle("CPUE in the LLS SA10 by age")+xlab("Year") +ylab("CPUE")+ scale_colour_discrete(name  ="Age")


plot(HKE.idx[[2]], type="internal",main="Cohorts consistence in the LLS SA10")




# Exercise 02

# 1. Run XSA with different Fcontrol settings (i.e. fse from 0.5 to 3, with 0.5 steps) and compare results in terms of residuals and retrospective

### fse 0.5
FLXSA.control.HKE <- FLXSA.control(fse=0.5, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 1.0
FLXSA.control.HKE1 <- FLXSA.control(fse=1.0, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 1.5
FLXSA.control.HKE2 <- FLXSA.control(fse=1.5, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 2.0
FLXSA.control.HKE3 <- FLXSA.control(fse=2.0, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 2.5
FLXSA.control.HKE4 <- FLXSA.control(fse=2.5, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 3.0
FLXSA.control.HKE5 <- FLXSA.control(fse=3.0, rage=0, qage=5, shk.yrs=3, shk.ages=2, maxit = 2000)


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

stocks <- FLStocks(HKE.stk, HKE.stk1, HKE.stk2, HKE.stk3, HKE.stk4, HKE.stk5)
names(stocks) <- c("fse0.5","fse1.0","fse1.5","fse2.0", "fse2.5", "fse3.0")
plot(stocks)


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

surv_names <- index.res(HKE.xsa4)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse2.5")

surv_names <- index.res(HKE.xsa5)
names(surv_names) <- c("Medits_SA10", "LLS_CPUE_SA10", "Medits_SA9", "Medits_SA11")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse3.0")



## Retrospective analysis ##
retro.years <- 2012:2014 # retro years range

HKE.stk.retro <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk,end=x)+FLXSA(window(HKE.stk,end=x),HKE.idx, FLXSA.control.HKE)))

HKE.stk.retro <- FLStocks(HKE.stk.retro)
names(HKE.stk.retro) <- as.character(unique(retro.years))
plot(HKE.stk.retro)


HKE.stk.retro1 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk1,end=x)+FLXSA(window(HKE.stk1,end=x),HKE.idx, FLXSA.control.HKE1)))

HKE.stk.retro1<- FLStocks(HKE.stk.retro1)
names(HKE.stk.retro1) <- as.character(unique(retro.years))
plot(HKE.stk.retro1)


HKE.stk.retro2 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk2,end=x)+FLXSA(window(HKE.stk2,end=x),HKE.idx, FLXSA.control.HKE2)))

HKE.stk.retro2<- FLStocks(HKE.stk.retro2)
names(HKE.stk.retro2) <- as.character(unique(retro.years))
plot(HKE.stk.retro2)


HKE.stk.retro3 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk3,end=x)+FLXSA(window(HKE.stk3,end=x),HKE.idx, FLXSA.control.HKE3)))

HKE.stk.retro3<- FLStocks(HKE.stk.retro3)
names(HKE.stk.retro3) <- as.character(unique(retro.years))
plot(HKE.stk.retro3)


HKE.stk.retro4 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk4,end=x)+FLXSA(window(HKE.stk4,end=x),HKE.idx, FLXSA.control.HKE4)))

HKE.stk.retro4<- FLStocks(HKE.stk.retro4)
names(HKE.stk.retro4) <- as.character(unique(retro.years))
plot(HKE.stk.retro4)


HKE.stk.retro5 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk5,end=x)+FLXSA(window(HKE.stk5,end=x),HKE.idx, FLXSA.control.HKE5)))

HKE.stk.retro5<- FLStocks(HKE.stk.retro5)
names(HKE.stk.retro5) <- as.character(unique(retro.years))
plot(HKE.stk.retro5)



# Exercise 03

# 1. Change the Fbar settings (i.e. 0-2, instead of 0-3) and compare results with the original assessment

# Now load the stock object from the assessment run with Fbar0-3 
load("HKE9_10_11.xsa.Rdata")
HKE0_3 <- HKE.stk

#Let's rename the stock object to avoid confusion with the previous assessment
HKE0_2 <- HKE.stk


# Set fbar in the stock 
range(HKE0_2)["minfbar"] <- 0
range(HKE0_2)["maxfbar"] <- 2


# And compare the results of the assessments run with Fbar0_3 and Fbar0_2
stocks <- FLStocks(HKE0_3,HKE0_2)
names(stocks)<- c("fbar0_3","fbar0_2")
plot(stocks)


# Exercise 04

# 1. Run XSA with different natural mortality values (costant with M = 0.3; and vectorial M = c(1.0, 0.8, 0.7, 0.5, 0.35, 0.3, 0.3)) and compare results with the original assessment 


# Let's look at the natural mortality in the HKE stock object
m(HKE)

# Let's try with constant M
m(HKE) <- 0.3
# if we were interested in a vector:
#m(HKE) <- c(1.0, 0.8, 0.7, 0.5, 0.35, 0.3, 0.3)
#m(HKE)

# and run again the full assessment with sensitivity analysis and retrospective:

# sensitivity on rage and qage
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




# Let's keep the best combination for rage and qage (rage=0 and qage=5), and do a sensitivity analysis on shrinkage age

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

# and finally a sensitivity analysis on fse

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


## Retrospective analysis ##
retro.years <- 2012:2014 # retro years range

HKE.stk.retro <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk,end=x)+FLXSA(window(HKE.stk,end=x),HKE.idx, FLXSA.control.HKE)))

HKE.stk.retro <- FLStocks(HKE.stk.retro)
names(HKE.stk.retro) <- as.character(unique(retro.years))
plot(HKE.stk.retro)


HKE.stk.retro1 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk1,end=x)+FLXSA(window(HKE.stk1,end=x),HKE.idx, FLXSA.control.HKE1)))

HKE.stk.retro1<- FLStocks(HKE.stk.retro1)
names(HKE.stk.retro1) <- as.character(unique(retro.years))
plot(HKE.stk.retro1)


HKE.stk.retro2 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk2,end=x)+FLXSA(window(HKE.stk2,end=x),HKE.idx, FLXSA.control.HKE2)))

HKE.stk.retro2<- FLStocks(HKE.stk.retro2)
names(HKE.stk.retro2) <- as.character(unique(retro.years))
plot(HKE.stk.retro2)


HKE.stk.retro3 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(HKE.stk3,end=x)+FLXSA(window(HKE.stk3,end=x),HKE.idx, FLXSA.control.HKE3)))

HKE.stk.retro3<- FLStocks(HKE.stk.retro3)
names(HKE.stk.retro3) <- as.character(unique(retro.years))
plot(HKE.stk.retro3)


###### Choose the final model, plot it

plot(HKE.stk)


# And compare the results of the assessments run with constant natural mortality with the final one we saved before
stocks <- FLStocks(HKE0_3,HKE.stk)
names(stocks)<- c("Original assessment","M0.3")
plot(stocks)

### Vectorial M

m(HKE) <- FLQuant(c(1.0, 0.8, 0.7, 0.5, 0.35, 0.3, 0.3),dim=c(7,9), dimnames=list(age=0:6,year=2006:2014))

### I'm not going to rerun the assessment with the vectorial m. You can do it and compare it with the previous ones.

# Exercise 05

# 1. Load the FLStock called DPS09stk and the FLIndex called DPS09idx. TIP: use the function load()

load("DPS09stk.Rdata")
load("DPS09idx.Rdata")

# 2. Run a sensitivity analysis on rage and qage using values ranging between -1 and 1 for rage and between 2 and 3 for qage. On shk.age ranging between 1 to 3. On fse ranging between 0.5 and 3 with steps of 0.5. Remember to check residuals and retrospective to decide the best possible model.


# Sensitivity analysis on rage and qage
# rage from -1 to 1 and qage from 2 to 3

FLXSA.control.dps <- FLXSA.control(fse=2.0, rage=1, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.dps1 <- FLXSA.control(fse=2.0, rage=1, qage=3, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.dps2 <- FLXSA.control(fse=2.0, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.dps3 <- FLXSA.control(fse=2.0, rage=0, qage=3, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.dps4 <- FLXSA.control(fse=2.0, rage=-1, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.dps5 <- FLXSA.control(fse=2.0, rage=-1, qage=3, shk.yrs=3, shk.ages=2, maxit = 2000)


#Running the assessments with different settings
dps.xsa <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps)
dps.xsa1 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps1)
dps.xsa2 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps2)
dps.xsa3 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps3)
dps.xsa4 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps4)
dps.xsa5 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps5)


#Add the results to the stock files
dps.stk0 <- dps.stk+dps.xsa
dps.stk1 <- dps.stk+dps.xsa1
dps.stk2 <- dps.stk+dps.xsa2
dps.stk3 <- dps.stk+dps.xsa3
dps.stk4 <- dps.stk+dps.xsa4
dps.stk5 <- dps.stk+dps.xsa5


stocks <- FLStocks(dps.stk0,dps.stk1,dps.stk2,dps.stk3,dps.stk4,dps.stk5)
names(stocks)<- c("r1q2","r1q3","r0q2","r0q3","r-1q2","r-1q3")
plot(stocks)


# The different runs are not that different, only in the last years, so it is hard from this plot to decide the best parameters to use. Let's check the residuals

###Residuals by fleet
surv_names <- index.res(dps.xsa)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r1q2")

surv_names <- index.res(dps.xsa1)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r1q3")

surv_names <- index.res(dps.xsa2)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r0q2")

surv_names <- index.res(dps.xsa3)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r0q3")

surv_names <- index.res(dps.xsa4)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r-1q2")

surv_names <- index.res(dps.xsa5)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year r-1q3")




# Let's keep the best combination for rage and qage (rage=0 and qage=2), and do a sensitivity analysis on shrinkage age.
# Fix qage and rage and change the values for shk.ages from 1 to 3

FLXSA.control.stk <- FLXSA.control(fse=2.0, rage=0, qage=2, shk.yrs=3, shk.ages=1, maxit = 2000)

FLXSA.control.stk1 <- FLXSA.control(fse=2.0, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

FLXSA.control.stk2 <- FLXSA.control(fse=2.0, rage=0, qage=2, shk.yrs=3, shk.ages=3, maxit = 2000)

#Running the assessments with different settings

dps.xsa <- FLXSA(dps.stk, dps.idx, FLXSA.control.stk)
dps.xsa1 <- FLXSA(dps.stk, dps.idx, FLXSA.control.stk1)
dps.xsa2 <- FLXSA(dps.stk, dps.idx, FLXSA.control.stk2)


#Add the results to the stock files
dps.stk0 <- dps.stk+dps.xsa
dps.stk1 <- dps.stk+dps.xsa1
dps.stk2 <- dps.stk+dps.xsa2

stocks1 <- FLStocks(dps.stk0,dps.stk1,dps.stk2)
names(stocks1) <- c("shk.ages=1","shk.ages=2","shk.ages=3")
plot(stocks1)


###Residuals by fleet 
surv_names <- index.res(dps.xsa)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year shk.ages=1")

surv_names <- index.res(dps.xsa1)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year shk.ages=2")

surv_names <- index.res(dps.xsa2)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year shk.ages=3")


# since shk.ages 2 and 3 look really similar we can decide which one we want to use. Let's use shk.ages = 2
# Finally a sensitivity analysis on fse with values from 0.5 to 3 with a step of 0.5

### fse 0.5
FLXSA.control.dps <- FLXSA.control(fse=0.5, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 1.0
FLXSA.control.dps1 <- FLXSA.control(fse=1.0, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 1.5
FLXSA.control.dps2 <- FLXSA.control(fse=1.5, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 2.0
FLXSA.control.dps3 <- FLXSA.control(fse=2.0, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 2.5
FLXSA.control.dps4 <- FLXSA.control(fse=2.5, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)

### fse 3.0
FLXSA.control.dps5 <- FLXSA.control(fse=3.0, rage=0, qage=2, shk.yrs=3, shk.ages=2, maxit = 2000)


#Running the assessments with different settings
dps.xsa <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps)
dps.xsa1 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps1)
dps.xsa2 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps2)
dps.xsa3 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps3)
dps.xsa4 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps4)
dps.xsa5 <- FLXSA(dps.stk, dps.idx, FLXSA.control.dps5)

#Add the results to the stock files
dps.stk0 <- dps.stk+dps.xsa
dps.stk1 <- dps.stk+dps.xsa1
dps.stk2 <- dps.stk+dps.xsa2
dps.stk3 <- dps.stk+dps.xsa3
dps.stk4 <- dps.stk+dps.xsa4
dps.stk5 <- dps.stk+dps.xsa5


stocks <- FLStocks(dps.stk0,dps.stk1,dps.stk2,dps.stk3,dps.stk4,dps.stk5)
names(stocks) <- c("fse0.5","fse1.0","fse1.5","fse2.0","fse2.5","fse3.0")
plot(stocks)


###Residuals by fleet 
surv_names <- index.res(dps.xsa)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse0.5")

surv_names <- index.res(dps.xsa1)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse1.0")

surv_names <- index.res(dps.xsa2)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse1.5")

surv_names <- index.res(dps.xsa3)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse2.0")

surv_names <- index.res(dps.xsa4)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse2.5")

surv_names <- index.res(dps.xsa5)
names(surv_names) <- c("Medits")
bubbles(age ~ year|qname, data = surv_names, main = "Proportion at age by year fse3.0")

# The residuals looks similar but before deciding the final value let's have a look at the retrospective analysis


## Retrospective analysis ##
retro.years <- 2012:2014 # retro years range

dps.stk.retro <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(dps.stk,end=x)+FLXSA(window(dps.stk,end=x),dps.idx, FLXSA.control.dps)))

dps.stk.retro <- FLStocks(dps.stk.retro)
names(dps.stk.retro) <- as.character(unique(retro.years))
plot(dps.stk.retro)


dps.stk.retro1 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(dps.stk1,end=x)+FLXSA(window(dps.stk1,end=x),dps.idx, FLXSA.control.dps1)))

dps.stk.retro1<- FLStocks(dps.stk.retro1)
names(dps.stk.retro1) <- as.character(unique(retro.years))
plot(dps.stk.retro1)


dps.stk.retro2 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(dps.stk2,end=x)+FLXSA(window(dps.stk2,end=x),dps.idx, FLXSA.control.dps2)))

dps.stk.retro2<- FLStocks(dps.stk.retro2)
names(dps.stk.retro2) <- as.character(unique(retro.years))
plot(dps.stk.retro2)


dps.stk.retro3 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(dps.stk3,end=x)+FLXSA(window(dps.stk3,end=x),dps.idx, FLXSA.control.dps3)))

dps.stk.retro3<- FLStocks(dps.stk.retro3)
names(dps.stk.retro3) <- as.character(unique(retro.years))
plot(dps.stk.retro3)


dps.stk.retro4 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(dps.stk4,end=x)+FLXSA(window(dps.stk4,end=x),dps.idx, FLXSA.control.dps4)))

dps.stk.retro4<- FLStocks(dps.stk.retro4)
names(dps.stk.retro4) <- as.character(unique(retro.years))
plot(dps.stk.retro4)


dps.stk.retro5 <- tapply(retro.years,1:length(retro.years),function(x)
  return(window(dps.stk5,end=x)+FLXSA(window(dps.stk5,end=x),dps.idx, FLXSA.control.dps5)))

dps.stk.retro5<- FLStocks(dps.stk.retro5)
names(dps.stk.retro5) <- as.character(unique(retro.years))
plot(dps.stk.retro5)

###### Choose the final model and plot it

plot(dps.stk3)

