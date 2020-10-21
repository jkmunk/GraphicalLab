# # make it all into a data frame and sort by V2
# temp <- data.table(cbind(as.numeric(names(temp)),
#                          tmp$V2,
#                          as.numeric(temp),
#                          as.double(tempup),
#                          as.double(tempdn)),
#                    key = "V3")

# sort by SD
tmp <- data.table(tmp,
                   key="SD")

# transform SD using dsigmtrdt
tmp <- dsigmtrdt[tmp,
                  list(time,
                       value,
                       extreme,
                       SD,
                       yssigm2,
                       up,
                       dn),
                  roll = "nearest"]

# set column name for transformed data
colnames(tmp)[5] <- "SDtrans"

# sort by up
tmp <- data.table(tmp,
                   key="up")

# transform up using dsigmtrdt
tmp <- dsigmtrdt[tmp,
                  list(time,
                       value,
                       extreme,
                       SD,
                       SDtrans,
                       up,
                       yssigm2,
                       dn),
                  roll = "nearest"]

# set column names for transformed data
colnames(tmp)[7] <- "uptrans"

# Calculate up transformed
tmp$uptrans <- tmp$uptrans - tmp$SDtrans

# sort by dn
tmp <- data.table(tmp,
                   key="dn")

# transform dn using dsigmtrdt
tmp <- dsigmtrdt[tmp,
                  list(time,
                       value,
                       extreme,
                       SD,
                       SDtrans,
                       uptrans,
                       dn,
                       yssigm2),
                  roll = "nearest"]

# set column names for transformed data
colnames(tmp)[8] <- "dntrans"

# Calculate dn transformed
tmp$dntrans <- tmp$SDtrans - tmp$dntrans

# sort by time and remove untransformed dn
tmp <- tmp[order(time),-"dn"]

# Add extreme to temp
# temp <- cbind(temp,tmp$V3)
# names(temp) <- c("time",
#                  "value",
#                  "SD",
#                  "up",
#                  "dn",
#                  "extreme")

# Define SD string
if (is.numeric(tmp$value)) { #} != 0) {
  SDstring <- paste0(
    # space and parenthesis start
    " (",
    
    # SD value
    as.character(round(tmp$SD,
                       digits = 1)),
    
    # "SD" and parenthesis end
    " SD)"
  )
} else {
  SDstring = ""
}

# add trace to pltydsigm
pltydsigm <- add_trace(pltydsigm,
                       
                       # dataset is temp
                       data = tmp,
                       
                       # X is time
                       x= ~time,
                       
                       # Y is transformed data
                       y= ~SDtrans,
                       
                       # type is scatter
                       type="scatter",
                       
                       # mode is markers and lines
                       mode="markers+lines",
                       
                       # set line type
                       line = list(dash = linetypes[(d-1)%/%10+1],
                                   color = linecolours[(d-1)%%10+1]),
                       
                       # marker colour
                       marker = list(color = linecolours[(d-1)%%10+1]),
                       
                       # define hovertext, first analysis name
                       text = paste0(analyses[Alistrow,
                                              1],
                                     
                                     # space
                                     " ",
                                     
                                     # extreme sign
                                     tmp$extreme,
                                     
                                     # space
                                     " ",
                                     
                                     # real value
                                     as.character(tmp$value),
                                     
                                     # space
                                     " ",
                                     
                                     # unit
                                     analyses[Alistrow,
                                              5],
                                     
                                     # SD string
                                     SDstring),
                       
                       # set hoverinfo to what was defined
                       hoverinfo = 'text',
                       
                       # set data series name
                       name = analyses[Alistrow,
                                       1],
                       
                       # assign to legend group
                       legendgroup = analyses[Alistrow,
                                              4],
                       
                       # set Y error bar to type "data"
                       error_y = list(type="data",
                                      
                                      # asymmetrical error bars
                                      symmetric=FALSE,
                                      
                                      # positive error bar lengths
                                      array=tmp$uptrans,
                                      
                                      # negative error bar lengths
                                      arrayminus=tmp$dntrans,
                                      
                                      # error bar colour
                                      color = linecolours[(d-1)%%10+1])) %>%
  
  # Trace between markers
  
  # X is data range and back
  add_trace(x = c(tmp$time,
                  rev(tmp$time)),
            
            # Y is lower bounds forward and upper bounds back
            y = c(tmp$uptrans - tmp$dntrans,
                  rev(tmp$uptrans + tmp$dntrans)),
            
            # Type scatter
            type = 'scatter',
            
            # Mode lines
            mode = "lines",
            
            # Fill to oneself
            fill = 'toself',
            
            # Dynamic fill colour
            fillcolor = paste0(linecolours[(d-1)%%10+1],"22"),
            
            # Transparent line
            line = list(color = 'transparent'),
            
            # Transparent markers
            marker = list(color = 'transparent'),
            
            # assign to legend group
            legendgroup = analyses[Alistrow,
                                   4],
            
            # turn off legend entry for trace
            showlegend = FALSE,
            
            # turn off hoverinfo
            hoverinfo = 'none')
