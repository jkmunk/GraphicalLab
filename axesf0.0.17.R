
# X axis ticks
Xticks <- as.numeric(names(Data)[2:ncol(Data)])

# X axis tick ages
Xages <- as.Date(format(Sys.time(), "%Y-%m-%d")) - as.Date(Xticks, "1900-01-01")

# X axis labels
Xlabels <- as.Date(as.numeric(names(Data)[2:ncol(Data)]), 
                   origin = "1900-01-01")
#                   origin = "1899-12-30")

Xlabels_age <- paste0(round(Xages/365,1), " år siden")

# Y axis ticks
Yticks <- c(-10:10)
Ys <- c(-10,
        10)
Ylabels <- c(-10,
             -7,
             -6,
             -5,
             -4,
             -3,
             -2,
             2,
             3,
             4,
             5,
             6,
             7,
             10)

Yticks_lab <- c(-9.975265, 
                -9.525574,
                -8.807517, 
                -7.309352,
                -4.996646,
                -2.680304,
                -1.167303,
                1.167303, 
                2.680304, 
                4.996646, 
                7.309352, 
                8.807517, 
                9.525574, 
                9.975265)

# Make some data for some straight lines
strline <- NULL # Initialize
strline$x <- Ys # X values
strline$y <- Ys # Y values
strline <- as.data.frame(strline) # Make it a data frame
