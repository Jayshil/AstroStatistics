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
