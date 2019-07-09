rm(list=ls())

# loading_data_into_FLR.R - How to load data from outside R into FLR objects

# Load FLCore library
library(FLCore)

#---------------------------------------------------
# Reading into R, then creating an FLR object
#---------------------------------------------------

# read.table and its friends
?read.table

# Key options to look out for:
# file (obviously)
# header
# sep
# row.names
# col.names

# Save your data as a *.csv file (comma separated file)
# Example file in /Data/catch_numbers.csv
# Note that we have row and column names (ages and years)

# age,2006,2007,2008,2009,2010,2011,2012,2013,2014
# 0,8.24E+04,6.20E+04,6.51E+04,9.17E+04,4.25E+04,6.67E+04,3.00E+04,2.61E+04,4.26E+04
# 1,1.46E+04,1.40E+04,1.21E+04,1.24E+04,1.25E+04,1.28E+04,1.03E+04,1.29E+04,1.06E+04
# 2,2.30E+03,1.31E+03,8.70E+02,8.12E+02,1.45E+03,1.20E+03,8.80E+02,7.44E+02,1.33E+03
# 3,2.99E+02,2.09E+02,2.51E+02,1.72E+02,2.25E+02,2.85E+02,2.09E+02,1.34E+02,1.87E+02
# 4,1.03E+02,5.48E+01,8.05E+01,8.59E+01,6.83E+01,1.01E+02,5.36E+01,5.32E+01,3.96E+01
# 5,2.94E+01,1.24E+01,3.75E+01,2.52E+01,3.67E+01,3.79E+01,1.41E+01,1.17E+01,2.41E+01
# 6,1.00E-03,2.65E+00,7.76E+00,9.25E+00,1.19E+01,8.37E+00,2.02E+00,3.86E+00,1.98E+01


# Read this in using read.table() with default options
(catch.n <- read.table("../Data/hke_9_10_11_catch_n.csv"))

# Looks terrible
# what just happened?
# The separator in our file is a comma , so we need to specify that (default in read.table is white space)
(catch.n <- read.table("../Data/hke_9_10_11_catch_n.csv", sep=','))

# Better but the column and row names have been included as data
# We can try to fix this using the header and row.names options
(catch.n <- read.table("../Data/hke_9_10_11_catch_n.csv", header=TRUE, sep=','))


# The column names are ugly (with the Xs. Use check.names to sort out issue
(catch.n <- read.table("../Data/hke_9_10_11_catch_n.csv", header=TRUE, sep=',',check.names = F))

# Can use read.csv() instead - same as read.table() but different default options
(catch.n <- read.csv("../Data/hke_9_10_11_catch_n.csv",check.names=FALSE))
(catch.n <- read.csv("../Data/hke_9_10_11_catch_n.csv",header = FALSE))

library(data.table)
(catch.n2 <- fread("../Data/hke_9_10_11_catch_n.csv",stringsAsFactors = F))

# We have read in the data as a data.frame
class(catch.n)

# FLQuant objects accept ‘vector’, ‘array’ or ‘matrix’. We can convert the object catch.n to a matrix.
catch.n.matrix <- as.matrix(catch.n[1:7, -1])
catch.n.matrix

# We need to specify the dimnames
catch.n.flq <- FLQuant(catch.n.matrix, dimnames=list(age=0:6, year = 2006:2014))
catch.n.flq

# Another option for reading in the data is to omit column and row names from the csv data
# You would use header=FALSE (else you lose the first row of data)
# Also leave the row.names argument empty

(catch.n <- read.csv("../Data/hke_9_10_11_catch_n.csv", header=FALSE))
(catch.n.matrix <- as.matrix(catch.n[2:8, -1]))

(catch.n <- read.csv("../Data/hke_9_10_11_catch_n.csv", header=FALSE,skip=1)[,-1])

(catch.n.flq <- FLQuant(catch.n.matrix, dimnames=list(age=0:6, year = 2006:2014)))

# Read Mean weight at age (catch.wt)
# 
# age,2006,2007,2008,2009,2010,2011,2012,2013,2014
# 0,0.0097962,0.0111972,0.0099485,0.0107332,0.0100794,0.0090594,0.0122723,0.0112717,0.0091963
# 1,0.1385243,0.1465506,0.1437444,0.146009,0.1336534,0.1469369,0.1404243,0.146898,0.1544967
# 2,0.522766,0.5264274,0.5163683,0.5108274,0.5129791,0.5053598,0.5024037,0.5469402,0.4646127
# 3,1.1715577,1.1825167,1.1354463,1.159358,1.1450351,1.1673509,1.137563,1.1530822,1.1471284
# 4,1.9157514,1.8273481,1.8831095,1.87599,1.916567,1.9015151,1.9288463,1.8358521,1.8486048
# 5,2.6205658,2.8217072,2.6853483,2.6349913,2.7660951,2.6874283,2.7865475,2.603386,2.6832733
# 6,3.0005651,4.9239695,3.6367794,3.713922,3.9833698,4.0459298,3.8248526,4.382772,3.8651729
# 
catch.wt <- read.csv("../Data/hke_9_10_11_catch_wt.csv", header=FALSE)
catch.wt.matrix <- as.matrix(catch.wt[2:8, -1])
catch.wt.flq <- FLQuant(catch.wt.matrix, dimnames=list(age=0:6, year = 2006:2014))

# So we have two FLquants, one for catch.n and one for catch.wt, we want to combine them in an FLStock. 

# # create an FLStock
# hke.stk <- FLStock(catch.n = catch.n.flq, catch.wt = catch.wt.flq, catch = quantSums(catch.n.flq*catch.wt.flq))
# plot(hke.stk)
# 
#  # To plot you have to specify unit of measure 
# units(harvest(hke.stk)) <- "f"
# plot(hke.stk)

# What do we need now to have all the data for a VPA, SCA? 
# NEXT STEPS are to Load

#   discards.n discards.wt
#   landings.n  landings.wt

# Read Natural mortality at age (M)
m <- read.csv("../Data/hke_9_10_11_m.csv", header=FALSE)
m.matrix <- as.matrix(m[2:8, -1])
m.flq <- FLQuant(m.matrix, dimnames=list(age=0:6, year = 2006:2014))
m.flq

#   mat (0, 0.22, 0.87, 1)
(mat.flq <- FLQuant(matrix(c(0, 0.22, 0.87, 1, 1, 1, 1), nrow=7, ncol=9), dimnames=list(age=0:6, year = 2006:2014)))

#m.spwn(0)
(m.spawn <-  FLQuant(matrix(rep(0), nrow=7, ncol=9), dimnames=list(age=0:6, year = 2006:2014)))

#f.spawn(0)  
(harvest.spawn <-  FLQuant(matrix(rep(0), nrow=7, ncol=9), dimnames=list(age=0:6, year = 2006:2014)))

# Assemble the full FLStock for HAKE
hke.stk <- FLStock(catch.n = catch.n.flq, 
                   catch.wt = catch.wt.flq,
                   harvest.spwn = harvest.spawn,
                   m.spwn = m.spawn,
                   m = m.flq,
                   mat = mat.flq)


# Compute total catch
catch(hke.stk) <- computeCatch(hke.stk)

plot(catch(hke.stk))

plot(hke.stk)

# Add units for example on fising mortality
units(harvest(hke.stk)) <- "f"
plot(hke.stk)


# If you get your data into R you can get it into an FLQuant.
# If you get your data into an FLQuant you get it into any FLR object!

# What else do we need to run a stock assessment like? A tuning index, like a 
# trawl survey relative abundance index or a commercial CPUE.

# Load an FLIndex
# take the Tuning index from MEDITS
# 
# age,2006,2007,2008,2009,2010,2011,2012,2013,2014
# 0,1250.42,1907.19,1544.78,1890.43,813.51,639.35,907.4,1252.29,610.5
# 1,99.67,51.52,92.69,78.11,131.46,67.18,56.44,67.21,64.5
# 2,2.32,0.95,2.97,0.38,1.46,2.45,2.37,4.37,4
# 3,0.49,0.97,1.52,0.32,0.3,1.2,0.29,0.29,0.2
# 4,0.01,0.14,0.01,0.01,0.17,0.01,0.01,0.01,0.3
# 5,0.01,0.14,0.01,0.32,0.15,0.01,0.16,0.22,0.01
# 
# 
(catch.n_idx <- read.csv("../Data/hke_9_10_11_idx.csv", header = TRUE))
(catch.n_idx <- read.csv("../Data/hke_9_10_11_idx.csv", header = TRUE, row.names = 1))
(catch.n_idx.matrix <- as.matrix(catch.n_idx[1:6, ]))

hke.idx <- FLQuant(catch.n_idx.matrix, dimnames=list(age=0:5, year = 2006:2014))

hke.idx <- FLIndex(catch.n = hke.idx)

hke.idx <- FLIndices(hke.idx)

# plot the only filled slot, catch.n
plot(catch.n(hke.idx[[1]]))

# last thing, Set the timing of the survey!
hke.idx[[1]]@range[c('startf', 'endf')] <- c(0.66,0.75)


# "Tuning; HKE in GSA 9"							
# 101							
# Medits                        							
# 2006   2014                     							
# 1   1   0.4   0.6	              						
# 0  4                     							
# 1	1686.571	58.583	2.502	0.26	0.182
# 1	2514.259	38.88	2.24	1.537	0.098
# 1	5871.627	57.216	1.241	0.32	0.446
# 1	6573.9	52.838	1.085	0.457	0.076
# 1	2469.127	37.298	2.573	0.1	0.078
# 1	769.899	29.391	1.29	0.329	0.1
# 1	1464.35	21.931	0.991	0.484	0.312
# 1	1743.236	35.288	1.001	0.102	0.327
# 1	1564.17	27.137	1.901	0.218	0.294


hke9.idx <- readFLIndices("../Data/hke9_Tun_standard.dat")


#---------------------------------------------------------------
# Reading directly into an FLR object
#---------------------------------------------------------------

# Used for reading data that has already been prepared in a
# specific format:
# Lowestoft VPA suite
# Adapt
# CSA
# ISA

# readVPAFile()
# The Lowestoft VPA format uses lots of text files, with a single index file.
# readVPAFile() reads in a single file (e.g. fishing mortality, catch numbers etc.)
# Here we read in the catches at age of herring in VIa. This is in the file 'canum.txt'
# Look at this raw data file in a text editor
# Read in the file using readVPAFile("name_of_file") (include the file name extension, e.g. .txt)
#
#Catch in Numbers (thousands)
# 1 2
# 1957 2011
# 1 7
# 1
# 0	7709	9965	1394	6235	2062	1720
# 100	3349	9410	6130	4065	5584	6666
# 1060	7251	3585	8642	3222	1757	3699
# 516	18221	7373	3551	2284	770	1924
# 1768	7129	14342	6598	2481	2392	1659
# 259	7170	5535	10427	5235	3322	7289
# 132	6446	5929	2032	3192	3541	5889
# 88	7030	5903	4048	2195	3972	9168
# 234	3847	10135	9008	2426	2019	13362
# 0	16809	11894	10319	7392	3356	16208
# 
catch.n <- readVPAFile("../Data/her-irlw/canum.txt")

# Gives you an FLQuant 
catch.n

# readFLStock()
# Reads in a whole FLStock object - providing that the input data has been set up OK!
# Can take formats Lowestoft VPA, Adapt, CSA, ICA
# Example using Lowestoft VPA Format.
# The VPA format uses lots of text files, with a single index file. Here, the index file is "index.txt"
# Open the file "index.txt" in a text editor and take a look at it
# To read in the stock, pass readFLStock() the name of the index file. 
# You also need to tell R where the file is (i.e. which directory or folder it is in).
# 
# Herring VIa(S) VIIbc 
# 1
# caton.txt
# canum.txt
# weca.txt
# west.txt
# natmor.txt
# matprop.txt
# fprop.txt
# mprop.txt 

# 
her <- readFLStock("../Data/her-irlw/index.txt")
# Gives us FLStock
class(her)
summary(her)

# Easy!
# But note that we have only read in the data used by the stock assessment
# This does not include the estimated harvest rates or stock abundance
harvest(her)
stock.n(her)

# So we need to load these in separately using readVPAFile
stock.n <- readVPAFile("../Data/her-irlw/n.txt")
stock.n
# set the stock.n slot of her
stock.n(her) <- stock.n
# Do it all in one step for fishing mortality
harvest(her) <- readVPAFile("../Data/her-irlw/f.txt")
# Note that the units of the harvest slot have not been set - good idea to do this
units(harvest(her)) <- "f"

plot(her)

# Also need to do some tidying up
# Some of the data is inconsistent
# Total landings = sum(numbers * weight)
apply(landings.n(her) * landings.wt(her), 2, sum)
landings(her)
# So make consistent - use the computeLandings() method
landings(her)=computeLandings(her)
# No discard information
discards.wt(her)
discards.n(her)
# Set up the discards and catches
discards.wt(her)=landings.wt(her)
discards.n(her)=0
discards(her)=computeDiscards(her)
catch(her)=landings(her)
catch.wt(her)=landings.wt(her)
catch.n(her)=landings.n(her)
# Set fbar range
range(her)
range(her)[c("minfbar","maxfbar")]=c(3,6)
# Set plusgroup as the working group  (this may need to be done before stock.n and harvest are read in due to change in max. age)
her <- setPlusGroup(her,plusgroup=7)


# Also methods and functions available:
# readFLIndex() - read an abundance index
# readFLIndices() - read several abundance indices
# readMFCL() - for Multifan-CL
# readADMB() - for ADMB

# Other way to load your data, if you already have an FLStock saved as .RData, you can load it directly in your R workspace by using the command load()

load("../Data/HKE_09_10_11_stk.Rdata")
stk
plot(stk)
