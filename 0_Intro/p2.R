help(cenfit)
galLx <- c(39.7, 42.3, 41.7, 41.6, 43.0, 40.2, 40.6, 41.1, 39.9, 40.7, 41.2, 42.9)
cens <- c(F,F,F,F,F,F,F,F, T,T,T,T)  # FALSE=detected TRUE=left-censored. 
galKM <- cenfit(galLx, cens)
str(galKM)   # The help(cenfit) should describe this output but does not!  

summary(galKM)  # display KM estimator on console
plot(galKM, xlab=expression("log L"[x]~~"(erg/s)"))     # plot KM estimator with 95% confidence band
quantile(galKM) # median and other quantiles
mean(galKM)  ;  sd(galKM)  # mean, uncertainty of mean, standard deviation about the mean
