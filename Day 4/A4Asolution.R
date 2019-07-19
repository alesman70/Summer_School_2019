load("HKE_09_10_11_EWG15_11.RData")
load("HKE_09_10_11_idx.Rdata")

# rename the stocks
hke <- HKE_09_10_11_EWG15_11
hke.idx <- flq.idx 

# Solution 1
fmodel <- ~ age + I(age^2) + factor(year)
qmodel <- list(~ factor(age), # q of MEDITS 10
               ~ factor(age), # q of CPUE LLS 10
               ~ factor(age), # q of MEDITS 9
               ~ factor(age)) # q of MEDITS 11

fitQ <- sca(hke, hke.idx, fmodel, qmodel)
hkeQ <- hke + fitQ

# diagnostics
resQ <- residuals(fitQ, hke, hke.idx)
plot(resQ)
bubbles(resQ)
qqmath(resQ)

z <- as.matrix(harvest(hkeQ)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fitQ, hke)
plot(fitQ, hke.idx[1])

AIC(fitQ)
BIC(fitQ)

# Solution 2 fit other linear models that use transformations of age or year

fmodel <- ~ age + I(log(age+1)) + factor(year)
fitlog <- sca(hke, hke.idx, fmodel, qmodel)
hkelog <- hke + fitlog

# diagnostics
reslog <- residuals(fitlog, hke, hke.idx)
plot(reslog)
bubbles(reslog)
qqmath(reslog)

z <- as.matrix(harvest(hkelog)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))

plot(fitlog, hke)
plot(fitlog, hke.idx[1])

AIC(fitlog)
BIC(fitlog)

# Solution 3
fmod <- ~ s(age, k=6) + s(year, k = 10)
qmodel <- list(~ factor(age), # q of MEDITS 10
               ~ factor(age), # q of CPUE LLS 10
               ~ factor(age), # q of MEDITS 9
               ~ factor(age)) # q of MEDITS 11

fitbest <- sca(hke, hke.idx, fmod,qmodel)
                              
# Solution 4
fmodel <- ~ factor(age) + factor(year)
qmodel <- list(~ factor(age), ~ factor(age), ~ factor(age), ~ factor(age)) 
fitbest <- sca(hke, hke.idx, fmodel, qmodel)
stkbest <- hke + fitbest
resbest <- residuals(fitbest, hke, hke.idx)
plot(resbest)
bubbles(resbest)
qqmath(resbest)

z <- as.matrix(harvest(stkbest)[,,drop = TRUE])
x <- as.numeric(rownames(z))
y <- as.numeric(colnames(z))

plot3d(surface3d(z = z,  y = y , x= x, type = "n"))
surface3d(z = z,  y= y , x= x, col = jet.colors(100))


plot(fitbest, hke)
plot(fitbest, hke.idx)

plot(stkbest)
                                      
                                      
# Solution 5
plot(FLStocks(XSA=hke,a4abest=stkbest))
                                      