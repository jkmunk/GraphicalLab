# this is a data point in the measurement range, put equality sign in "extreme"
extreme[counter] <- "="

# put value in tempraw3
tempraw3[counter] <- as.numeric(str_replace(tempraw[1,
                                                    i],
                                            ",",
                                            "."))

# put date in tempraw2
tempraw2[counter] <- names(tempraw)[i]

# increment counter
counter <- counter + 1
