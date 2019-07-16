# Spict Exercises

#########################################################################################
# EXERCISE 1

# 1 Trim catch time series to start the year when the index starts, fit the best model you can
#  - does it converge?
#  - if yes, does it change the paramters and the perception of the stocks ?

ane6$obsC <- ane6$obsC[c(59:72)]
ane6$timeC <- ane6$timeC[c(59:72)]


plotspict.data(ane6)

ane6TRUNC <- fit.spict(ane6)

# Explore convergence
capture.output(summary(ane6TRUNC))[1:4]

# Calculate residuals and main diagnostics

ane6fitT_diagn <- calc.osa.resid(ane6TRUNC)

plotspict.diagnostic(ane6fitT_diagn)

# Retrospective analysis
# Diagnostics, run it by taking away the last 3 years, one at a time
ane6fitT_retro <- retro(ane6TRUNC, nretroyear = 3)

# now plot it!
plotspict.retro(ane6fitT_retro)

# We can have a look at the final results.
# Fit Summary
# summary(ane6TRUNC)

x11()
plot(ane6TRUNC)

######################################################################################
# EXERCISE 2

# 1) Run a spict model with fishing effort

# 2) Is it converging? 

# 3) How are the diagnostics? 

# 4) Is the fit better?
ane6effort <- ane6

ane6effort$obsE <- ane$nominal_effort[59:71]
ane6effort$timeE <- ane$year[59:71]

plotspict.data(ane6effort)
ane6effortfit <- fit.spict(ane6effort)

capture.output(summary(ane6effortfit))[1:4]

ane6effortt_diagn <- calc.osa.resid(ane6effortfit)

plotspict.diagnostic(ane6effortt_diagn)


plot(ane6effortfit)


######################################################################################
# EXERCISE 3


# Robust estimation for effort (or catch), helps reduce influence of idividual data points on model fit or CIs.

# We think that there might be some problems with the effort data, in particular with the estimate of 2007

# 1) Turn on robust estimation
ane6effort$robflage <- 1 # here we turn on robust estimation on effort
# Rerun the assessment, does the fit improve?

ane6effortfitRE <- fit.spict(ane6effort)

capture.output(summary(ane6effortfitRE))[1:4]

plot(ane6effortfitRE)

ane6effortt_diagnRE <- calc.osa.resid(ane6effortfitRE)

plotspict.diagnostic(ane6effortt_diagnRE)





# 2) remove the value of fishing effort in 2007 and rerun the assessment, is it better?
ane6effort_rm2007 <- ane6effort

ane6effort_rm2007$obsE[5] <- NA

ane6effort_rm2007_fit <- fit.spict(ane6effort_rm2007)

capture.output(summary(ane6effort_rm2007_fit))[1:4]

plot(ane6effort_rm2007_fit)

ane6effort_rm2007_fit_diag <- calc.osa.resid(ane6effort_rm2007_fit)

plotspict.diagnostic(ane6effort_rm2007_fit_diag)
