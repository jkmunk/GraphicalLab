
# Generate transformation data to be used as lookup table since the inverse equation for the double sigmoid is
# not nice
xs <- seq(-10,
          10,
          length.out = 1001) # This will be the Y axis, extremes are -10 and 10

intervals <- cumsum(dnorm(xs, 
                          sd=3))/2.5-10

yssigm2 <- sigmoid(intervals+4) + sigmoid(intervals-4) # The inverse of the transformation we want

yssigm2 <- yssigm2-1 # Adjustment to make it symmetric around axis

yssigm2 <- yssigm2*10 # Adjustment to give it the same range as Y axis

dsigmtrdt <- data.table(cbind(intervals,
                              yssigm2),
                        key="intervals") # Bind them in data frame

