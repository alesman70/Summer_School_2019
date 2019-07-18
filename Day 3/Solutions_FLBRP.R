# EXERCISE 01
# Make a sensitivity analysis on a range of M to see how it affects the estimation of the main reference point(F0.1).
hkebrpa <- hkebrp
m(hkebrpa) <- c(1.38,	0.56,	0.27,	0.22,	0.19,	0.18,	0.17) # borrowed from GSA16

hkebrpb <- hkebrp
m(hkebrpb) <- c(1.03,	0.51,	0.33,	0.26,	0.22,	0.2, 0.2) # borrowed from GSA7

hkebrpa <- brp(hkebrpa) 
hkebrpb <- brp(hkebrpb) 


# take a look
refpts(hkebrpa)
refpts(hkebrpb)


refpts(hkebrp)['f0.1','harvest']
refpts(hkebrpa)['f0.1','harvest']
refpts(hkebrpb)['f0.1','harvest']


refpts(hkebrpa)['f0.1','harvest']/refpts(hkebrp)['f0.1','harvest']




