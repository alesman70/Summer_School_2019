ane_lm <- ane[ane$year > 2002,]

ane_lm$cat_ind <- ane_lm$catch/ane_lm$index # Catch/Index (E, effort proxy)

# Linear model index 
mod1 <- lm(ane_lm$index ~ ane_lm$cat_ind, data = ane_lm)
co <- coef(mod1)


## Index cersus catch/index plot
summary(mod1)
plot(ane_lm$cat_ind, ane_lm$index, col = 'blue')
lines(seq(0,1,0.01), co[1] + co[2] * seq(0,1,0.01), col = 'blue')



## Catch (Yield) versus catch/index plot
plot(ane_lm$cat_ind, ane_lm$catch, col = 'blue',xlim= c(0,1))
lines(seq(0,1,0.01), co[1] * seq(0,1,0.01) + co[2]* (seq(0,1,0.01))^2, col = 'blue')

## Effort guess at MSY
-co[1]/(2 * co[2]) 


## Catch at msy
f <- -co[1]/(2 * co[2]) 


co[1] * f + co[2]*f^2


## Catch (Yield) versus catch/index plot
plot(ane_lm$cat_ind, ane_lm$catch, col = 'blue',xlim= c(0,1))
lines(seq(0,1,0.01), co[1] * seq(0,1,0.01) + co[2]* (seq(0,1,0.01))^2, col = 'blue')
abline(h = co[1] * f + co[2]*f^2, lty = 2)
