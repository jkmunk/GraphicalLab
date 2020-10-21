# this is a data point beyond the measurement range, put inequality sign in "extreme"
extreme[counter] <- substr(tempraw[1,
                                   i],
                           2,
                           2)

# put value in tempraw3
tempraw3[counter] <- as.numeric(str_replace(substr(tempraw[1,
                                                           i],
                                                   3,
                                                   nchar(tempraw[1,
                                                                 i])),
                                            ",",
                                            "."))

# put its name (date) in tempraw2
tempraw2[counter] <- names(tempraw)[i]

# increment counter
counter <- counter + 1
