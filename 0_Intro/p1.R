# V. Astrophysical calculations of galaxy distances 

# The `function' function: Note how one function uses another
# This how R builds new capabilities based on old capabilities in a compact syntax.

# First, make a simple calculation without functions: galaxy distances in Mpc for six redshifts  

#print(getwd())
#setwd('/Users/japa6985')
#library(txtplot)
z <- seq(0.0, 0.5, 0.1)
z
H_0 <- 68  	 		               	# km/s/Mpc,  Planck value
speed.light <- 3.0E5          	# km/s
dist <- speed.light*z / H_0     # in Mpc
dist
class(dist)

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
