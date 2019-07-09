rm(list=ls())
library(FLCore)
library(ggplotFL)
# library(lattice)

load("../Data/HKE_09_10_11_EWG15_11.RData")

HKE_09_10_11_EWG15_11


# Rename the Stock object to something more handy and faster to write
hke <- HKE_09_10_11_EWG15_11

ggplot(data = catch(hke), aes(year, data)) + geom_point(col=3) + geom_line(col=2) + ylab("Catch (tonnes)") + xlab("Year") # passing directly an FLQuant object for the data argument in ggplot

# Plotting abundance by age #
plot(catch.n(hke))
p <- ggplot(data=catch.n(hke),aes(year, data,col=as.factor(age))) + 
  geom_line() + facet_wrap(~age, scales="free_y", nrow=3) + labs(x="", y="") # using ggplot arguments
p
p + theme_gray(base_size = 14)
p + theme_bw(base_size = 24)
p + theme_bw(base_size = 24) + theme(strip.background =element_rect(fill="lightyellow"))+
  theme(strip.text = element_text(colour = 'blue'))

q <- ggplot(data=FLQuants(Yield=catch(hke), SSB=ssb(hke), F=fbar(hke)), aes(year, data)) + 
  geom_line() + facet_wrap(~qname, scales="free_y", nrow=3) + labs(x="", y="") # passing directly more FLQuant objects for the data argument in ggplot
q
q + theme_gray(base_size = 14)
q + theme_bw(base_size = 24)
q + theme_bw(base_size = 24) + theme(strip.background =element_rect(fill="lightyellow"))+
  theme(strip.text = element_text(colour = 'blue'))


# par(mfrow=c(1,1))
plot(fbar(hke))

plot(hke)

# plot(rlnorm(200, fbar(hke), 0.15))

# Many units are missing. Unit are need to plot. So we are going to assign them
units(discards(hke)) <- units(landings(hke)) <- units(stock(hke)) <- "t"
units(discards.wt(hke)) <- units(landings.wt(hke)) <- units(stock.wt(hke)) <- "kg"
units(discards.n(hke)) <- units(landings.n(hke)) <- units(stock.n(hke)) <- "1000"
units(m(hke)) <- "m"
hke

# We want add total stock biomass estimated by the model #
computeStock(hke)
(stock(hke) <- quantSums(stock.n(hke)*stock.wt(hke)))

fqs <- FLQuants(F = fbar(hke), SSB = ssb(hke), Rec = rec(hke), Catch = catch(hke), TotalBiomass = stock(hke))
plot(fqs)
aa <- plot(fqs) + facet_wrap(~qname, scales="free") + 
  theme_gray(base_size = 14) 
  aa
   aa+theme(strip.background =element_rect(fill="lightyellow"))
  aa+theme(strip.text = element_text(colour = 'blue'))


ggplot(catch.n(hke), aes(year, as.factor(age), size=data, fill=as.factor(age))) + geom_point(shape=21) + 
  scale_size(range = c(1, 20)) + ylab("age") + theme(legend.position = "none")

dat <- as.data.frame(catch.n(hke))
dat$resid <- dat$data - mean(dat$data)
ggplot(dat, aes(year, as.factor(age), size=resid)) +
  geom_point(shape=21, aes(colour=factor(sign(resid)), fill=factor(sign(resid)))) +
  scale_size(range = c(1, 20)) +
  scale_colour_manual(values=c("black", "white")) +
  scale_fill_manual(values=c("lightgray", "black")) +
  ylab("age")

# Data by sex as in ple4 default##
data("ple4sex")
plot(FLStocks(Male=ple4sex[,,'male'], Female=ple4sex[,,'female'])) + theme(legend.position="top")

print(ple4sex)

bwplot(data~year, rlnorm(200, fbar(hke), 0.15),scales=list(x=list(rot=90)), ylab="Fbar")

wireframe(data~age+year, data=harvest(hke),zlab="F",drape = TRUE,
          col.regions = colorRampPalette(c("green", "red"))(100))

wireframe(data~age+year, data=harvest(hke),zlab="F",drape = TRUE,
          col.regions = colorRampPalette(c("green", "red"))(100), screen = list(z = -30, x = -60))


ggplot(data=hke, aes(year, data)) + geom_line(aes(group=age, colour=age)) + facet_wrap(~slot, scales="free", nrow=3) + labs(x="", y="") + theme(legend.position = "bottom") + 
  theme_bw(base_size = 14) +
  theme(strip.background =element_rect(fill="lightyellow"))+
  theme(strip.text = element_text(colour = 'blue'))
