# STARTING with FLQuant

# Load the package
library(FLCore)

# Create an empty FLQuant, the 6D array used for storing (almost) all data in FLR
FLQuant()

# Let's look at a toy example
# rnorm 
# dim is n ages and n years
(flq <- FLQuant(rnorm(40), dim=c(4,10), dimnames=list(age=1:4, year=1990:1999))) # rnorm mean=0 sd=1 closer according to number of values

# what if we get the dims wrong?
FLQuant(rnorm(40), dim=c(4,9), dimnames=list(age=1:4, year=1990:1999))
# FLQuant(rnorm(36), dim=c(4,9), dimnames=list(age=1:4, year=1990:1998))

# Inspecting it

# This is a 6D array
is(flq)
dim(flq)
length(dim(flq))

# with dimension names a.k.a. dimnames
dimnames(flq)

# and names of dimnames ...
names(flq)

# with an 'units' attribute, still unset
units(flq)

# and an specific name for the first dimension
quant(flq)
quant(flq) <- "length"
quant(flq) <- "age"
quant(flq)

# A summary look at its content and dims
summary(flq)

# or a full look at the object (useful for small objects only)
flq

# look at the structure of the FLQaunt
str(flq)

# A default plot spreads across panels all dims of length > 1
plot(flq)


# Use the CREATORS to build FLQuant objects

# FLQuant() can accept different inputs

# (1) Nothing, i.e. missing
FLQuant()

# (2) A vector, which goes by default along the year dim
FLQuant(1:10)

# TIP: to create it with a vector along the first dim, use
FLQuant(as.matrix(1:10))

# (3) A matrix
FLQuant(matrix(1:4, nrow=4, ncol=4))

# (4) Or an array, from 2D to 6D (have a look at unit values)
FLQuant(array(1:50, dim=c(2,5,5)))
# FLQuant(array(1:50, dim=c(2,5,1)))
# FLQuant(array(1:50, dim=c(10,5,1)))

# The object ATTRIBUTES can be set when constructing it

# dimnames
flq <- FLQuant(matrix(rnorm(16), nrow=4, ncol=4),
	dimnames=list(age=1:4, year=2000:2003))

# quant
FLQuant(matrix(rnorm(16), nrow=4, ncol=4),
	dimnames=list(year=2000:2003), quant='length')

# quant
FLQuant(matrix(rnorm(16), nrow=4, ncol=4),
        dimnames=list(year=2000:2003), quant='pippo')

# units
FLQuant(matrix(abs(rnorm(32)), nrow=4, ncol=4),
	dimnames=list(age=1:4, year=2000:2003), units="kg")

# dimnames<-
dimnames(flq) <- list(year=c('2001', '2002', '2003', '2004'))

dimnames(flq)

# NOTE: dimnames are always character ...
is.character(dimnames(flq)$year)

# but number can be used and are converted
dimnames(flq) <- list(year=2001:2004)
is.character(dimnames(flq)$year)

# units<-
units(flq) <- "t"

units(flq)

# here we go
flq


# FLQuant object can be SUBSET

# Using '[' to select

# a row
flq[1,]

# some columns
flq[, 2:4]

# or with negative indices
flq[-1,]

# Note that not all indices must be provided (unlike array)
flq[1,,,,,]
flq[1,1,,,,]
flq[1,1,1,1,1,1]

flq[1,,,,,]
# vs.
flq[1,]

flq[1,,,,,]
#vs
flq[1,1,1,1,1,1]


# Subsetting can be done by position, e.g. the first year
flq[,1]

# or name, e.g. 
flq[,"2002"]

# To select and expand along the year dimension only, window can be used
window(flq, start=2002, end=2004)

window(flq, start=2002, end=2010)


# trim
trim(flq, year=2003:2004, age=2:4)


# Array MATH still works as expected, with the usual R rules

# A fictional weight-at-age matrix
(waa <- FLQuant(matrix(seq(2, 30, length=6), nrow=6, ncol=10),
	dimnames=list(age=1:6, year=2000:2009), units="kg"))


# and the numbers-at-age matrix

set.seed(123)
(naa <- FLQuant(apply(matrix(rlnorm(10, log(0.5), 0.2), ncol=10), 2,
	function(x) exp( -1 * cumsum(rep(x, 6)))),
	dimnames=list(age=1:6, year=2000:2009), units="1000"))

# gives us the total biomass-at-age
(bma <- naa * waa)

# FLQuant(s) must be of the same dimensions
naa[1,] * waa

# %*% OPERATIONS
# Difference in dimensions taken care of
waa[1,] %*% naa

# Arithmetic operations by element work with vectors too
# get the actual numbers
naa * 1000

# Be careful with R's recycling rule, as this works (row first)
naa * c(1, 100)
naa * exp(c(1:6))

0.639557*2.718282
0.409033*7.389056

0.620330*2.718282
0.384809*7.389056

# but this issues a warning why?
naa * exp(c(1:7))
0.620330*1096.633158

# TRANSFORM
# apply

# A most useful function for arrays
# e.g. to sum over the first dimension
apply(bma, 2:6, sum)

# or get the mean abundance at age for the last 3 years
apply(bma[,as.character(2007:2009)], c(1,3:6), mean)


# ITERS
# Adding iters in dim, dimnames, or iter
(flq <- FLQuant(rlnorm(100), dim=c(10,1,1,1,1,10), quant='age'))

flq[iter=2]
as.data.frame(flq)

(flq <- FLQuant(rlnorm(100), dimnames=list(age=1:10, iter=1:10)))

(flq <- FLQuant(rlnorm(100), dimnames=list(age=1:10), iter=10))

# propagate, to expand an existing object
(bmi <- propagate(bma, iter=10))
bmidf <- as.data.frame(bmi)
bmidf[bmidf$age==1 & bmidf$year==2000,]

# with NAs
(bmi <- propagate(bma, iter=10, fill.iter=FALSE))

# or copies of the first iter
(bmi <- propagate(bma, iter=10, fill.iter=TRUE))

# iter, iter<-
# to access or modify a subset of iters, as in [,,,,,*]
iter(flq, 1)

iter(flq, 1:2)

# iterMeans, iterVars
# shortcuts for apply(x, 1:5, mean/var)
iterMeans(flq)

iterVars(flq)

iterMedians(flq)

# quantile
quantile(flq, probs=0.05)
quantile(flq, probs=0.5)


# EXERCISE #

# (1) Create an FLQuant object with elements numbered sequentially (i.e. 1 to N) and with ages from 1 to 6, years from 2003 to 2012 and four seasons (hint: its the fourth dimension). Call it flq.

# (2) Create an FLQuant with dimensions 6,10,1,1,1,100 and normally-distributed random numbers. Call it flq2.

# SUBSET

# (3) Extract from flq the values for the first three years


# (4) Select from flq leaving out the last age

# (5) What if do not know the precise last age name?


# APPLY

# (6) Calculate the proportion-at-age per year for flq. Suggestion: 
# First step: create an flquant with the total numbers by year and age (i.e. you need to sum all the seasons). 
# Second step: create another flquant with the total numbers by year (i.e. sum all the ages). 
# Third step: create an flquant and fill it with the total numbers by year repeated for all ages.
# Last step: divide your flquant with the total numbers by year and age (first step) by the flquant obtained in the third step to get the proportion at age per year.
#This is the long way... there are also shorter way :)

# (7) How many values in flq are greater than 0?



## END OF SCRIPT ###
