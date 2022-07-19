# I.  Set up your session

sessionInfo()  

getwd()                     # find working directory.  
#setwd('/Users/ericfeigelson/Desktop/Rdir')  # set working directory if you run 
                                             # locally on your computer
#getwd()                     # see the working directory has changed

library()                   # see packages installed on your computer
                            # ~30 are installed automatically with R
citation()                  # quote this citation in any publication using R

# II. Create and characterize a vector

a <- c(33, 44, 92, 58)      # combine numbers into a vector
length(a)
sum(a)
median(a)  ;  mad(a)
ls()                        # list names of objects in your environment
class(a)                    # state the `class' of an R object (described in III below)
str(a)                      # state the structure of an R object
summary(a)                  # many R objects have a built-in 'summary' function

a                           # display the contents of an R object on the console
# Annotated write to console. The \n symbol is a carriage return.
cat('Sum of ', length(a), ' elements in the vector a = ', sum(a), '\n')

write(file='output', a)     # write an ASCII file into the working directory
save(file='output_bin', a)  # write a binary file
save(file='output_bin', a)  # error because 'Save' is not a known function.
                            # R syntax is case sensitive.

# Manipulation of vector indices

a[1:4]          # show all four elements of the vector
a[3]            # note that R vectors start with index 1 not 0, unlike Python
a > 40          # logical operation
sum(a[a>40])    # note the self-referential use of vector/array indices here
which.max(a)    # R has many built-in functions for vectors and arrays
match(44, a)

# III.  R classes and packages

# Make and write a data.frame, a 2D array with column names

d <- data.frame(cbind(seq(1:4), a, a^3))  # Bind columns into data frame
class(d)
names(d) <- c('ID', 'a', 'a_cubed') # Column names for data frame                                         
d2 <- d[-4,-1]                            # Remove 4th row and 1st column
d ; d2
write.table(d, file='d.txt', quote=FALSE, row.names=FALSE)

# Make and show a list.

b_list <- list(star=c('Sirius', 'Procyon'), SpTy=c('O','B','A'), Hubble_km.s=68)
str(b_list)
b_list[['SpTy']] = list(subtype=seq(0.1:0.9, by=0.1))
str(b_list)
b_list$SpTy$subtype[1:3]

# IV. Arithmetic, algebra, trigonometry, and formatting numerics

5 + 3 ; 5-3 ; 5*3 ; 5/3 ; 5^3
x <- 5 ; y <- 3
x+y 

sin(0)  ; sin(pi/2)         # note angles are in radians
ang <- seq(0, pi/2, length=30) 
sin(ang)

trunc(12345.6789) ; round(12345.6789)  
format(12345.6789, digits=2, scientific=TRUE)   

log(20)  ;  log10(20)   # log() in R is base-e. Use log10() for base-10 logarithms.

# V. Astrophysical calculations of galaxy distances 

# The `function' function: Note how one function uses another
# This how R builds new capabilities based on old capabilities in a compact syntax.

# First, make a simple calculation without functions: galaxy distances in Mpc for six redshifts  

z <- seq(0.0, 0.5, 0.1)
z
H_0 <- 68  	 		               	# km/s/Mpc,  Planck value
speed.light <- 3.0E5          	# km/s
dist <- speed.light*z / H_0     # in Mpc
dist
class(dist)

# Now, make a more complicated relativistic calculation using new functions.

Omega_m <- (0.022068 + 0.12029) / (H_0/100)^2    
Omega_Lambda <- 0.6825                    # Planck satellite values

E.H0 <- function(redshift) {sqrt(Omega_m*(1+redshift)^3 + Omega_Lambda)}

lum.dist <- function(redshift) {  
	luminosity.distance = (speed.light/H_0) * integrate(E.H0, 0, redshift)$value 
	return(luminosity.distance)  }

distGR <- Vectorize(lum.dist)(z)

# Plot the results
# The 'plot' function has extensive options to change format; see 'help(par)'

options(jupyter.plot_scale=1)
options(repr.plot.width = 7, repr.plot.height = 5)

plot(z, distGR, type='l', lty=2, lwd=2, ylab='Distance (Mpc)')
lines(z, dist, lty=1, lwd=2)   
legend(0.0, 2500, lty=c(1,2), lwd=c(2,2), title='Galaxy distances', 
     	legend=c('Euclidean', expression(Lambda*CDM)))

# VI.  Calculate the Kaplan-Meier estimator of the galaxy X-ray Luminosity Function from a small dataset.

install.packages('NADA')
library(NADA)

help(cenfit)
galLx <- c(39.7, 42.3, 41.7, 41.6, 43.0, 40.2, 40.6, 41.1, 39.9, 40.7, 41.2, 42.9)
cens <- c(F,F,F,F,F,F,F,F, T,T,T,T)  # FALSE=detected TRUE=left-censored. 
galKM <- cenfit(galLx, cens)
str(galKM)   # The help(cenfit) should describe this output but does not!  

summary(galKM)  # display KM estimator on console
plot(galKM, xlab=expression("log L"[x]~~"(erg/s)"))     # plot KM estimator with 95% confidence band
quantile(galKM) # median and other quantiles
mean(galKM)  ;  sd(galKM)  # mean, uncertainty of mean, standard deviation about the mean

# VII. Examine, summarize and plot bivariate and univariate data 


set.seed(1)
x <- sample(seq(0.01, 3, length.out=500))
y <- 0.5*x + 0.3^(x^2) + rnorm(500, mean=0, sd=(0.05*(1+x^2)))
xy <- cbind(x, y)

plot(xy, pch=20)
summary(x) ; summary(y)   	# Summarizes properties of an R object

par(mfrow=c(1,2))  		# Set up a two-panel figure
boxplot(x,  notch=T, main='Boxplot for X')  # See help(boxplot.stats) for explanation
boxplot(y,  notch=T, pch=20, cex=0.5, main='Boxplot for Y')
dev.copy2pdf(file='box.pdf')

par(mfrow=c(1,1))
hist(x, breaks=30, main='', xlim=range(x), ylim=c(0,100), 
     xlab='Yvar', col='royalblue4')
hist(y, breaks=30, main='', xlab='Yvar', 
     col='#ee3b3b70', add=TRUE) # add=TRUE suppresses a new plot

plot(ecdf(x), pch=20, cex=0.0, verticals=TRUE, main='',ylab='EDF',xlab='')  
plot(ecdf(y), pch=20, cex=0.0, verticals=TRUE, add=T)
text(2.0,0.5,"X") ; text(1.4,0.8,"Y")             # text adds annotation within a plot
dev.copy2pdf(file='ecdf.pdf')

# VIII. Arrays, data frames and filtering

# Here xy is an `array' of numbers created by `column bind'

xy <- cbind(x, y)  ;  str(xy) 	

# A data.frame associates names to the columns

xy <- as.data.frame(xy)         
names(xy) <- c('Xvar', 'Yvar')  

# Collect rows where the first column value exceeds 2

high_x1 <- xy[xy[,1]>2,]        
high_x2 <- subset(xy, xy[,1]>2)	# Another way to extract rows
setequal(high_x1, high_x2)      # test equality of two vectors

# Sampling and bootstrapping 

trials <- sample.int(length(xy[,1]),20) # 20 random rows
xy[trials,]	

# 20 bootstrap resamples

trials <- sample.int(length(xy[,1]),20, replace=T) 
xy[trials,]	

# Estimate the standard error of the median of Yvar

median(xy[,2]) 

# Median absolute deviation estimate of median s.e.

mad(xy[,2]) / sqrt(500)	

library(boot)  			# The following function in a base-R library
med <- function(x,index){ median(x[index]) }

# Read help(boot) to understand its output list structure

boot(xy[,2], med, R=1000) # Bootstrap estimate of median s.e. 
hist(boot(xy[,2], med, R=1000)$t, breaks=50, 
     xlab='Bootstrap median of Yvar')

# IX. Bivariate plots and correlation tests

# Scatterplot. See help(points) for symbol shapes defined by the 'pch' argument.
par(mfrow=c(1,2))
plot(xy, pch=20, cex=0.5)	 
plot(log10(xy), pch=20, cex=0.5, xlab='log(Xvar)', ylab='log(Yvar)')

length(x[x>2])		# State length of a vector.  Use `dim' for an array or data.frame.
# Parametric hypothesis test for bivariate correlation 
cor.test(x[x>2],y[x>2], method='pearson')	
# Nonparametric hypothesis test for bivariate correlation
cor.test(x[x>2],y[x>2], method='kendall')	

# A list of the ~30 important CRAN packages embedded in the base-R environment
library()

# A full list of ~400 functions in R's `base' package
library(help = "base")

# Statistics in base R (~400 functions, tens-of-thousands more in CRAN and elsewhere in R)
library(help='stats')

# List current contents of your session environment
ls()

# Programming utilities including:
#    Use `source' to bring in external R scripts
#    Use `edit' to edit an R object
#    Use 'environment' to segregate a collection of objects
#    Functions `debug', `trace' and `browser' assist with code testing
#    Function 'process.events' allows low-level handling of R commands
library(help = 'utils')

# Loops:  for( i in 1:100) { ... }
# Program flow control:  if/else, ifelse, switch, while, repeat, next, break, stop
foo <- 2
if(foo == 1) cat('Hello world!') else cat('Do nothing')
for(i in 1:10) { cat(' Num = ', i, '\n') }

# Graphics and devices in base R (other packages in CRAN)
library(help='graphics')
library(help='grDevices')

# Parallel computing control in base R 
# CRAN has dozens of other high performance computing packages
library(help='parallel')

# Run an R script residing on disk
help(source)

# Save R objects (or your full environment) onto disk
help(save) ; help(load)

# Save or load history of R commands
help(savehistory)  ;  help(loadhistory)

# Connections, pipes, sockets, URLs, clipboard, compression, etc.
help(connections)
 
# Interact with host computer 
Sys.info()
system('ls -l')
system.time(fft(seq(0,1,length.out=1000000)))	# A million fast Fourier transforms

# Construct composite strings using 'paste'
# Extract postions of a string using `substring'
band_ir <- 'J'
paste('NGC1068',band_ir,'FITS', sep='.')

# FITS format reader/writer
install.packages('FITSio') ; library(FITSio)

# IDL Astro Library translated into R
install.packages('astrolibR') ; library(astrolibR)

# R/CRAN functions are public domain and can be wrapped from Python
# programs using package rpy2.  Example:
### pip install rpy2
### import rpy2
### import rpy2.robjects as robjects
### R = robjects.r
### ranGauss = R.rnorm(100)
### print ranGauss

# Python code can be wrapped into R using CRAN package 'reticulate' (among others)
# R has similar interfaces to many other languages.
