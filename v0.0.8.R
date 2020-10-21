# Graphical representation of lab results, with journal notes
#
# version 0.0.1 - SP compatible (meaning that SP data pasted into Excel can be imported).
#

# Left to implement/ideas:
# - Look at partial time, possibly at first present histogram-like plot of
#   data density in which user can pick a range to view
# - When looking at only some traces, +/- 2 SD lines disappear. Somehow keep 
#   them in at all times
# - Marker size = measurement imprecision, and then, connecting line width/shade
#   = confidence interval? Instead of error bar? Shaded are of critical difference
#   made
# - Tabs for classes of analyses as https://dashr.plotly.com/dash-core-components/tabs
# - Grouped analyses that can be turned on/off with one click - requires legendgroup
#   or similar in more than one level
#
# These ideas are trivial and possibly not for me to implement. My invention is the
# graphical presentation of data itself as deviation from ± 2 SD on a crooked Y axis.
# 
# Idea for us clinical chemists: highlight the measurements that show critically different
# measurements vs. those that don't, to estimate how much has been "wasted", or maybe 
# rather, show how few of the measurements could have provided the same clinical 
# information... Clearly, if two subsequent measurements of the same analyte are 
# critically different, both measurements provide new information and the time between 
# them is fine. Start by highlighting the parts of the traces that are within critical
# difference...

# September 2020, Jens K. Munk, jmun0029@regionh.dk

#---------------------------------------------------------

# Load stuff we need
library(sigmoid)
library(ggplot2)
library(plotly)
require(data.table)
library(readr)
library(tictoc)
library(stringr)
#require(scales)

# Set working directory
setwd("~/Ongoing projects/Graphical Lab/v0.1/SP/")

#---------------------------------------------------------

# Import analyses characteristics
source("~/Ongoing projects/Graphical Lab/v0.1/SP/imp-anals.R")

#---------------------------------------------------------

# Read data
source("~/Ongoing projects/Graphical Lab/v0.1/SP/SPimport3.R")

tic()

#---------------------------------------------------------

# Generate transformation data to be used as lookup table since the inverse equation for the double sigmoid is not nice
source("~/Ongoing projects/Graphical Lab/v0.1/SP/transf.R")

#---------------------------------------------------------

# Format axes
source("~/Ongoing projects/Graphical Lab/v0.1/SP/axesf.R")

#-----------------------------------------------------------------

# Initialize plot
source("~/Ongoing projects/Graphical Lab/v0.1/SP/initplot.R")

#-----------------------------------------------------------------

# Line colours
source("~/Ongoing projects/Graphical Lab/v0.1/SP/linecolours.R")

# Functions

# For when a data point contains "<" or ">"
ltgtDP <- function(counter,temprawentry) {
  # this is a data point beyond the measurement range, put inequality sign in "extreme"
  extreme <- substr(temprawentry[1,],
                             2,
                             2)
  
  # put value in tempraw3
  tempraw3 <- as.numeric(str_replace(substr(temprawentry[1,],
                                            3,
                                            nchar(temprawentry[1,])),
                                     ",",
                                     "."))
  
  # put its name (date) in tempraw2
  tempraw2 <- names(tempraw)[i]
  
  # increment counter
  counter <- counter + 1
  
  return(c(tempraw2,tempraw3,extreme))
  
}

inRIDP <- function(counter,temprawentry) {
  # this is a data point in the measurement range, put equality sign in "extreme"
  extreme <- "="
  
  # put value in tempraw3
  tempraw3 <- as.numeric(str_replace(tempraw[1,
                                             i],
                                     ",",
                                     "."))
  
  # put date in tempraw2
  tempraw2 <- names(tempraw)[i]
  
  # increment counter
  counter <- counter + 1
  
  return(c(tempraw2,tempraw3,extreme))
  
}

transform_add <- function(tmp,pltydsigm,yssigm2,dsigmtrdt,linetypes,linecolours,analyses) {
  
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
  
  return(pltydsigm)
}

#-----------------------------------------------------------------

# Add d datasets
for (d in 1:1) { #nrow(Data)) { # For every analyte,

  Alistrow <- match(Data[d,1],analyses$SPnavn)
  
  # Read the measurements
  tempraw <- Data[d,
                  2:ncol(Data)]
  
  # Read the dates, too
#  names(tempraw) <- names(Data)[2:ncol(Data)]
  
  # Initialize some temporary data holders
  tmp <- NULL
  
  # A counter
  counter <- 1

  # If both an upper and a lower reference interval is defined,
  if (!((is.na(analyses[Alistrow,
                        6])) || is.na(analyses[Alistrow, 
                                               7]))) {
    
    # For every data point,
    for (i in 1:length(tempraw)) {
      
      # If there is a greater than sign or a less than sign in the data point,
      if (grepl(">",
                tempraw[1,
                        i]) || grepl("<",
                                     tempraw[1,
                                             i])) {
        
#        source("ltgtDP.R")
        tmp <- rbind(tmp,ltgtDP(counter,tempraw[,i]))
        counter <- counter + 1
        
      }
      
      # Else, if it's not empty or NA,
      else if (!((tempraw[1,
                          i] == "") || (is.na(tempraw[1,
                                                      i])))) {
        
#        source("inRIDP.R")
        tmp <- rbind(tmp,inRIDP(counter,tempraw[,i]))
        counter <- counter + 1
        
      }
      
    }
    
#    source("juggle.R")
    
    tmp <- as.data.frame(tmp,
                         stringsAsFactors = FALSE)
    tmp$V1 <- as.numeric(tmp$V1)
    tmp$V2 <- as.numeric(tmp$V2)
    
    names(tmp) <- c("time",
                    "value",
                    "extreme")
    
    # Scale so reference interval is between -2 SD and +2 SD
    tmp$SD <- (4 * (tmp$value - analyses[Alistrow,
                                             6]) / (analyses[Alistrow,
                                                             7] - analyses[Alistrow,
                                                                           6])) - 2
    
    # Calculate length of positive error bar
#    tempup <- tmp$V2 + tmp$V2 * analyses[Alistrow,
#                                           8] / 200
    
    # Scale so reference interval is between -2 SD and +2 SD
    tmp$up <- (4 * ((tmp$value + tmp$value * analyses[Alistrow,
                                                         8] / 200) - analyses[Alistrow,
                                                                              6]) / (analyses[Alistrow,
                                                                                              7] - analyses[Alistrow,
                                                                                                            6])) - 2
#    tempup <- (4 * (tempup - analyses[Alistrow,
#                                      6]) / (analyses[Alistrow,
#                                                      7] - analyses[Alistrow,
#                                                                    6])) - 2
    
    # Calculate length of negative error bar
#    tempdn <- tmp$V2 - tmp$V2 * analyses[Alistrow,
#                                           8] / 200
    
    # Scale so reference interval is between -2 SD and +2 SD
    tmp$dn <- (4 * ((tmp$value - tmp$value * analyses[Alistrow,
                                                         8] / 200) - analyses[Alistrow,
                                                                              6]) / (analyses[Alistrow,
                                                                                              7] - analyses[Alistrow,
                                                                                                            6])) - 2
#    tempdn <- (4 * (tempdn - analyses[Alistrow,
#                                      6]) / (analyses[Alistrow,
#                                                      7] - analyses[Alistrow,
#                                                                    6])) - 2
    
    source("transform_add.R")

  # Else, if only an upper RI is defined, such as for CRP, for which a lower limit makes no sense,
  } else if ((is.na(analyses[Alistrow,
                             6])) && (!(is.na(analyses[Alistrow,
                                                       7])))) {
    
    # Here, set 0 to 0 SD axis and upper RI to +2 SD... How to do that? And set UL in measurement interval
    # to +10 SD
    
    # Clear out in tempraw, keeping only real data points
    # for every data point,
    for (i in 1:length(tempraw)) {
      
      # If there is a greater than sign or a less than sign in the data point,
      if (grepl(">",
                tempraw[1,
                        i]) || grepl("<",
                                     tempraw[1,
                                             i])) {
        
#        source("ltgtDP.R")
        tmp <- rbind(tmp,ltgtDP(counter,tempraw[,i]))
        counter <- counter + 1
        
      }
      
      # or if data point is not missing, nor NA,
      else if (!((is.na(tempraw[1,
                          i])) || (tempraw[1,
                                           i] == ""))) {
        
#        source("inRIDP.R")
        tmp <- rbind(tmp,inRIDP(counter,tempraw[,i]))
        counter <- counter + 1
        
      }
    
    }
    
#    source("juggle.R")
    
    tmp <- as.data.frame(tmp,
                         stringsAsFactors = FALSE)
    tmp$V1 <- as.numeric(tmp$V1)
    tmp$V2 <- as.numeric(tmp$V2)

    # tmp is now times, real values and extreme
    
    names(tmp) <- c("time",
                    "value",
                    "extreme")
    
    # scale so URI is at +2 SD on Y axis
    tmp$SD <- tmp$value / (analyses[Alistrow,
                                7] / 2)
    
#    names(temp) <- tmp$V1
    
    # scale positive error bars
    tmp$up <- tmp$SD + tmp$SD * analyses[Alistrow,
                                           8] / 200
    
    # scale to Y axis
#    tmp$up <- tmp$up / (analyses[Alistrow,
#                                 10] / 10)
    
    # scale negative error bars
    tmp$dn <- tmp$SD - tmp$SD * analyses[Alistrow,
                                           8] / 200
    
    # scale to Y axis
    # tempdn <- tempdn / (analyses[Alistrow,
    #                              10] / 10)
    
    # tmp is now also SD and positions of upper and lower error bars
    
#    source("transform_add.R")
    transform_add(tmp,pltydsigm,yssigm2,dsigmtrdt,linetypes,linecolours,analyses)

   }
  
}

# # add journal notes, data is JN
# pltydsigm <- pltydsigm %>% add_annotations(data = JN, 
#                                            
#                                            # X is time
#                                            x = names(JN),
#                                            
#                                            # Y is Ymax
#                                            y = 10,
#                                            
#                                            # text is "JN"
#                                            text = "JN", 
#                                            
#                                            # not sure what this does
#                                            xref = "x",
#                                            yref = "y", 
#                                            
#                                            # show an arrow
#                                            showarrow = TRUE,
#                                            
#                                            # arrow head size
#                                            arrowhead = 4,
#                                            
#                                            # arrow thickness
#                                            arrowsize =.5,
#                                            
#                                            # arrow X length
#                                            ax = 0,
#                                            
#                                            # arrow Y langth
#                                            ay = -40,
#                                            
#                                            # text to show on hover (the actual journal note)
#                                            hovertext = JN)

# 
# 
# 
# # X is data range and back
# pltydsigm <- pltydsigm %>% add_trace(x = c(temp$V1,
#                 rev(temp$V1)),
#           
#           # Y is lower bounds forward and upper bounds back
#           y = c(temp$V2trans - temp$V3trans,
#                 rev(temp$V2trans + temp$V3trans)),
#           
#           # Type scatter
#           type = 'scatter',
#           
#           # Mode lines
#           mode = "lines",
#           
#           # Fill to oneself
#           fill = 'toself',
#           
#           # Dynamic fill colour
#           fillcolor = paste0(linecolours[(d-1)%%10+1],"22"),
#           
#           # Transparent line
#           line = list(color = 'transparent'),
#           
#           # Transparent markers
#           marker = list(color = 'transparent'),
#           
#           # assign to legend group
#           legendgroup = analyses[Alistrow,
#                                  3],
#           
#           # turn off legend entry for trace
#           showlegend = FALSE,
#           
#           # turn off hoverinfo
#           hoverinfo = 'none')
# 




###
pltydsigm # make the graph!

toc()

# save as HTML widget, too
# htmlwidgets::saveWidget(as_widget(pltydsigm), 
#                         "~/Ongoing projects/Graphical Lab/index.html")
