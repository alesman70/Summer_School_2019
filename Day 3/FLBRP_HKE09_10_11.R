#' ---
#' title: "Reference Points with FLBRP, Summer School in Quantative Fisheries Stock Assessment, Capo Granitola"
#' author: "Alessandro Ligas & Alessandro Orio"
#' date: "July 13th, 2017"
#' ---

# install.packages("FLBRP", repos="http://flr-project.org/R")
# load the library
library(FLBRP)

# Load the stock object 
# we are going to use one of the stock objects from the HKE_09_10_11 stock assessment
load("HKE9_10_11.xsa.Rdata")

# let's rename it:
hke <- HKE.stk

# This FLStock should contain the results of the assessment. To check that there are results we can look if the stock.n slot is full

stock.n(hke)  #stock number by age 


# To produce reference points we first need to create an FLBRP object corresponding to our assessment results 
hkebrp <- FLBRP(hke) 

summary(hkebrp)

# The FLBRP class has information on: 

# selection Patterns 
catch.sel(hkebrp) 
discards.sel(hkebrp)  

ggplot(catch.sel(hkebrp), aes( age, data))+geom_line()
# ggplot(discards.sel(hkebrp), aes( age, data))+geom_line() 


# mass-at-age 
stock.wt(hkebrp) 
catch.wt(hkebrp) 
# discards.wt(hkebrp) 

# biological parameters and other quantities by age 
m(hkebrp) 
mat(hkebrp) 



xyplot(data~age|qname, data=FLQuants(swt=stock.wt(hkebrp), cwt =catch.wt(hkebrp), mat= mat(hkebrp), m = m(hkebrp)), type="l",scale="free") 


# Since we have not provided a SR relationship yet, our analyses will be per-recruit 


# All *.obs slots hold the observations from FLStock 
fbar.obs(hkebrp) 

# Once an FLBRP object has been created then equilibrium 
# quantities can be estimated 

# to estimate equilibrium quantities we use the function brp() 
hkebrp <- brp(hkebrp) 


# and we get a table of reference points 
refpts(hkebrp) 



# In this case, Fmsy is the same as Fmax, since the assumed stock recruitment 
# relationship is mean recruitment 


refpts(hkebrp)[c('msy', ('fmax')), ] 


# We can plot our reference points and expected quantities using the function plot 
plot(hkebrp) 

# let's plot only the yield per recruit
plot(ypr(hkebrp))

# The derived reference points would be used to compare the fishing mortality (from Fbar) with the  
# reference point of choice. 

# in our case we can compare the F of the last year with F0.1 (a proxy for FMSY)
fbar(hke)[,"2014"]  
refpts(hkebrp)['f0.1','harvest'] 


# What is the ratio between our fishing mortality and our reference point?  i.e. F/F0.1  
fbar(hke)[,"2014"]  / refpts(hkebrp)['f0.1','harvest']  

### The ratio is definitely higher than 1 so we are fishing our stock at an unsustainable level



# SR 
### in our original FBRP objects we didn't have a SR relationship

# let's have a look at the formula of our SR model in our FLBRP
model(hkebrp)
# and parameter a 
params(hkebrp) 


# Now we provide a stock-recruit relationship

# To do that we first need to specify which SR model we want to use
model <- "geomean"

# Then we need to create an FLSR object and use it in the function mle
srr <- fmle(as.FLSR(hke, model = model))

## We can have a look at how is the fit of our SR model by plotting
plot(srr)

# Now we calcluate the reference points with brp and we also provide the SR
hkebrpgm <- brp(FLBRP(hke, sr = srr))
ref_points <- refpts(hkebrpgm)
ref_points

# Also in this case, Fmsy is the same as Fmax, since the assumed stock recruitment 
# relationship is the geometric mean of recruitment 


plot(hkebrpgm)

### we can also plot our observations
plot(hkebrpgm, obs=T)

### and we can save a table of reference points
rp_table <- data.frame(ref_points@.Data)[,1:5]
temp <- rownames(rp_table)
rp_table <- data.frame(rp_table, row.names = NULL)
rp_table <- cbind(temp, rp_table)
colnames(rp_table) <- c("", "F","Total Yield","Recruitment","SSB","Biomass")

write.table(rp_table,file="Ref_points_HKE.csv",sep=";",row.names=FALSE, col.names=T)

################################################

# Let's add a SR relationship (Beverton and Holt)

hkesr <- as.FLSR(hke, model=bevholt) 
hkesr <- fmle(hkesr) 

# Let's look at the residuals and the fit
plot(hkesr) 

# and provide it when constructing FLBRP 
hkebrp <- FLBRP(hke, sr=hkesr) 

# let's have a look at the formula of our SR model
model(hkebrp)
# and parameters a and b
params(hkebrp) 


# and we refit brp 
hkebrp <- brp(hkebrp) 


# and see the difference in reference points 
refpts(hkebrp) 

# let's remove Fcrash since it produced NaN
refpts(hkebrp) <- refpts(hkebrp)[-3,]


# and plot relationships 
plot(hkebrp) 


# EXERCISE 01
# Make a sensitivity analysis on a range of M to see how it affects the estimation of the main reference point(F0.1).
hkebrpa <- hkebrp
m(hkebrpa) <- c(1.38,	0.56,	0.27,	0.22,	0.19,	0.18,	0.17) # borrowed from GSA16

hkebrpb <- hkebrp
m(hkebrpb) <- c(1.03,	0.51,	0.33,	0.26,	0.22,	0.2, 0.2) # borrowed from GSA7






