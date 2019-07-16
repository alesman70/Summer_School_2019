# Class structure
# load data from Mediterranean Hake in GSA 9-10-11
rm(list=ls())
library(FLCore)
library(ggplotFL)

load("../Data/HKE_09_10_11_EWG15_11.RData")

# Rename the Stock object to something more handy and faster to write
hke <- HKE_09_10_11_EWG15_11

print(hke)

class(hke)
harvest(hke)
slotNames(hke)



# look at the structure
str(hke)

# Look  at the name of the stock, why is it different after we changed the object name? 
# name(hke) != object name
# If you prefer change it #
name(hke)[]="hke"
str(hke)

# Description
desc(hke)

#hke@range

# Range of ages, years, plus group and fbar
range(hke)

range(hke)["max"] <- 5
range(hke)
range(hke)["max"] <- 6

range(hke)[c("minfbar","maxfbar")] <- c(1,5)
range(hke)
range(hke)[c("minfbar","maxfbar")] <- c(1,4)

catch.n(hke) <- setPlusGroup(hke,5)
hke <- setPlusGroup(hke,6)


#let's plot it'
plot(hke)


# catch =~ landings + discards, in case discards had already been included in landings,
# so are 0 here. Normally discards should be reported in the discard quant
units(landings(hke)) <- units(discards(hke)) <- "t"
units(landings.n(hke)) <- units(discards.n(hke)) <- "1000"
units(landings.wt(hke)) <- units(discards.wt(hke)) <- "kg"

landings(hke) + discards(hke)
catch(hke)
catch(hke) <- landings(hke) + discards(hke)

landings.n(hke) + discards.n(hke)
catch.n(hke)
catch.n(hke) <- landings.n(hke) + discards.n(hke)

landings.n(hke)[,"2006",,,,]
catch.n(hke)[,"2006",,,,]

catch.wt(hke)

# Compute catch in weight for 2006 only,
# this is the product of number of fish @ age times the mean weight of fish @ age
quantSums(catch.n(hke)[,"2006",,,,] * catch.wt(hke)[,"2006",,,,])
catch(hke)
?quants
# is it in line with what is reported with the hke stock?
catch(hke)[,"2006",,,,]

# m, m.spwn
m(hke) # natural mortality

# What is the mean natural mortality applied to the stock?
mean(m(hke))
quantMeans(m(hke))
quantMeans(m(hke)[c("0","1"),,,,,,])
(1.16598+0.573)/2
quantMeans(m(hke)[c(0,1),,,,,,]) wrong!!!
exp(-m(hke)["0",,,,,,])
exp(-1.16598)

# m.spwn
m.spwn(hke) # fraction of the natural mortality ocurring before spawning (value can varying between 0 and 1)
hke@m.spwn[] <- 0.5
m.spwn(hke)[] <- 0.6
m.spwn(hke)[] <- 0
# equivalently you have some fishing mortality (harvest) occurring on the fish population before spawning.
# This is important when you have population that spawn in a narrow window of time, in particular with a short life cycle.
harvest.spwn(hke)

# maturity
mat(hke)

# weight
catch.wt(hke)
stock.wt(hke)
stock.wt(hke)/catch.wt(hke)


# ESTIMATED PARAMETERS ##

# harvest e.g. fishing mortality or harvest rate, normally defined as F
harvest(hke)
xyplot(data~age,groups=year,data=harvest(hke),type=c("l","p"))

# Size of the estimated population at sea, in terms of total biomass,total numbers at age.
stock.n(hke)
stock.wt(hke)
xyplot(data~age,groups=year,data=stock.n(hke),type=c("l","p"))

# So we compute it
computeStock(hke)

# and assign it to the slot stock
stock(hke) <- computeStock(hke)

# Recruitment
rec(hke)
# stock.n(hke)["0",,,,,,]

xyplot(data~year,data=rec(hke),type=c("l","p"))

# Spawning Stock Biomass (SSB)
# SSB = stock.n * exp(-F * F.spwn - M * M.spwn) * stock.wt * mat
# quantSums(stock.n(hke)*stock.wt(hke)*mat(hke))
ssb(hke)
xyplot(data~year,data=ssb(hke),type=c("l","p"))
mat(hke)
# METHODS Fbar = mean(F between fbar ages)
fbar(hke)

# METHODS fapex = max F per year
fapex(hke)
harvest(hke)
#METHODS Z = total mortality (F+M)
z(hke)
units(m(hke))[]="m"
z(hke)

harvest(hke)
z(hke)-m(hke)

hke@catch.n[1] # first position
hke@catch.n["1"] # first age
# hke@catch.n["1",]
hke@catch.n[,"2010"]
hke@catch.n["1","2010"]

hke@catch.n[,c("2008","2010")]
hke_rev <- hke
hke_rev@catch.n[,c("2008","2010")]
hke_rev@catch.n[,c("2008","2010")] <- NA
hke_rev@catch.n
str(hke@harvest)
hke@harvest@.Data[,,,,,2]
hke@harvest@.Data[,,,,,1]

# hke@harvest@.Data[1,,,,,]

# To print cohort in time
head(as.data.frame(catch.n(hke), cohort=TRUE),10)

# Other stuff
range(hke)
smallhke <- window(hke, start = 2008, end = 2013)

# Check new year range
range(smallhke)
summary(smallhke)

# SUBSET by year
# temp <- hke[,c("2008", "2009", "2010", "2011")]
# # or
# temp <- hke[,as.character(2008:2011)]

# replace using logical values
a=catch(hke)[[6]]
catch(hke)[[6]] <- 99
plot(catch(hke))
catch(hke)[catch(hke)==99] <- 5000
plot(catch(hke))
catch(hke)[[6]]=a
plot(catch(hke))


# plot the FLStock
plot(hke)

# or individual parts
plot(stock(hke))
plot(stock.n(hke))
plot(landings(hke))

# METHODS convert to data frame
# entire FlStock
temp<-as.data.frame(hke)
summary(temp)
# or only some slots
head(as.data.frame(FLQuants(catch.n=catch.n(hke), stock.n=stock.n(hke))))
head(model.frame(FLQuants(CN=catch.n(hke), TB=stock(hke))))
head(as.data.frame(hke))
head(as.data.frame(hke,drop=T))


data("ple4")
data("ple4.indices")

# If you want access to a position in indeces
ple4.indices[[1]]@catch.n[c(1,2),11]=100
ple4.indices[[2]]@catch.n[c(1,2),2]=100
ple4.indices[[3]]@catch.n[c(1,2),16]=100
ple4.indices[[3]]@catch.n[c(1,2,3),22]=100

# Change name index name
ple4.indices@names
ple4.indices@names[1]=c("MEDITS")
ple4.indices@names[1]=c("BTS-Isis-early")
ple4.indices@names

# PLUS GROUP INDEX
# #Set the plus group in the index
ple4.indices[[1]] <- FLIndex(index=setPlusGroup(index(ple4.indices[[1]]), 5))
range(ple4.indices[[1]], c("startf", "endf")) <- c(0.66, 0.75)
