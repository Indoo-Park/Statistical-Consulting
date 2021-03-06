

options(stringAsFactor=FALSE, width=300)

library(MASS)

xbool_save_file <- FALSE




set.seed(777)

n <- 700

probCOs <- c(0, 0.2, 0.4, 0.6, 0.8, 1)
ordinalDom <- c(1, 2, 3, 4, 5)

actualProbs <- runif(n, 0, 1)

xndx <- findInterval(actualProbs, probCOs)

y <- as.factor(ordinalDom[ xndx ])

xpolr <- polr(y ~ 1)
summary(xpolr)

########## recover the partition

xx <- summary(xpolr)$coefficients[ , "Value" ]
1 / (1 + exp(-xx))

xx <- summary(xpolr)$zeta
1 / (1 + exp(-xx))



##################################### with numeric predictor

x <- qnorm(actualProbs)

y <- x + rnorm(n)

y <- floor((y - min(y)) * (5 / diff(range(y)))) + 1
table(y)


y <- as.factor(y)

xpolr <- polr(y ~ 1)
summary(xpolr)
xx <- summary(xpolr)$zeta
1 / (1 + exp(-xx))


xpolr <- polr(y ~ x)
summary(xpolr)


xx <- summary(xpolr)$zeta
1 / (1 + exp(-xx))


xxcoef <- summary(xpolr)$coefficients[ "x", "Value" ]
1 / (1 + exp(-xxcoef * (-1))) #### x is -1
1 / (1 + exp(-xxcoef * (+1))) #### x is 1


########### note the symmetry
###### the polr function estimates this coefficient for centered sigmoid
###### the intercept terms above 'shift' this effect to account for baserate probs
1 / (1 + exp(-xxcoef * (-1))) +
1 / (1 + exp(-xxcoef * (+1)))


