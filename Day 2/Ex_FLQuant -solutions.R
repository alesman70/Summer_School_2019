
# library(FLCore)

# CREATE

# (1) Create an FLQuant object with elements numbered sequentially (i.e. 1 to N) and with ages from 1 to 6, years from 2003 to 2012 and four seasons. Call it flq.

flq <- FLQuant(1:240, dim=c(6,10,1,4,1,1),
	dimnames=list(age=1:6, year=2003:2012, season=1:4))

# (2) Create an FLQuant with dimensions 6,10,1,1,1,100 and normally-distributed random numbers. Call it flq2.

flq2 <- FLQuant(rnorm(6000), dimnames=list(age=1:6, year=2003:2012, iter=1:100))

# SUBSET

# (3) Extract from flq the values for the first three years

fle3 <- flq[,1:3]

fle3 <- flq[,as.character(2003:2005)]

# (4) Select from flq leaving out the last age

fle4 <- flq[-6,]

# (5) What if do not know the precise last age name?

fle5 <- flq[-dims(flq)$max, ]

# APPLY

# (6) Calculate the proportion-at-age per year for flq. Suggestion: 
# First step: create an flquant with the total numbers by year and age (i.e. you need to sum all the seasons). 
# Second step: create another flquant with the total numbers by year (i.e. sum all the ages). 
# Third step: create an flquant and fill it with the total numbers by year repeated for all ages.
# Last step: divide your flquant with the total numbers by year and age (first step) by the flquant obtained in the third step to get the proportion at age per year.
#This is the long way... there are also shorter way :)

(fl1<- apply(flq,c(1,2),sum))

(fl2<-apply(flq, 2, sum))

(fl3<-FLQuant(matrix(fl2, ncol = 10,nrow=6, byrow = TRUE), dimnames=list(age=1:6, year=2003:2012)))

fl1/fl3

### or a faster way

apply(flq,c(1,2),sum) %/% apply(flq, 2, sum)


# (7) How many values in flq2 are greater than 0?

flq2 > 0

sum(flq2 > 0)

