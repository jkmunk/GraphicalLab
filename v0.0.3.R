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

#-----------------------------------------------------------------

# Add d datasets
for (d in 1:nrow(Data)) { # For every analyte,

  # Find the row containing same SPname in the list of analyses
  Alistrow <- which(unlist(regexec(Data[d,
                                        1],
                                   analyses$SPnavn,
                                   fixed = TRUE)) == max(unlist(regexec(Data[d,
                                                                             1],
                                                                        analyses$SPnavn,
                                                                        fixed = TRUE))))
  
  # which(gregexpr(Data[d,
  #                              1],
  #                         analyses$SPnavn,
  #                         fixed = TRUE))
  # 
  # If both an upper and a lower reference interval is defined,
  if (!((is.na(analyses[Alistrow,
                        6])) || is.na(analyses[Alistrow, 
                                               7]))) {

    # Read the measurements
    tempraw <- Data[d,
                    2:ncol(Data)]
    
    # Read the dates, too
    names(tempraw) <- names(Data)[2:ncol(Data)]
    
    # Initialize some temporary data holders
    tempraw2 <- tempraw3 <- NULL
    
    # A counter
    counter <- 1
    
    # For every data point,
    for (i in 1:length(tempraw)) {
      
      # If it's not empty or NA,
      if (!((tempraw[1,
                     i] == "") || (is.na(tempraw[1,
                                                 i])))) { 
        
        # put its name (date) in tempraw2
        tempraw2[counter] <- names(tempraw)[i]
        
        # and put its value in tempraw3
        tempraw3[counter] <- as.numeric(str_replace(tempraw[1,
                                                            i],
                                                    ",",
                                                    ".")) 
        
        # increment counter
        counter <- counter + 1 
      }
    }

    # name tempraw3 with tempraw2
    names(tempraw3) <- tempraw2
    
    # copy tempraw3 to tempraw
    tempraw <- tempraw3
    
    # clean up
    rm(tempraw2,
       tempraw3) 
    
    # Scale so reference interval is between -2 SD and +2 SD
    temp <- (4 * (tempraw - analyses[Alistrow,
                                     6]) / (analyses[Alistrow,
                                                     7] - analyses[Alistrow,
                                                                   6])) - 2
    
    # Calculate length of positive error bar
    tempup <- tempraw + tempraw * analyses[Alistrow,
                                           8] / 200 
    
    # Scale so reference interval is between -2 SD and +2 SD
    tempup <- (4 * (tempup - analyses[Alistrow,
                                      6]) / (analyses[Alistrow,
                                                      7] - analyses[Alistrow,
                                                                    6])) - 2
    
    # Calculate length of negative error bar
    tempdn <- tempraw - tempraw * analyses[Alistrow,
                                           8] / 200
    
    # Scale so reference interval is between -2 SD and +2 SD
    tempdn <- (4 * (tempdn - analyses[Alistrow,
                                      6]) / (analyses[Alistrow,
                                                      7] - analyses[Alistrow,
                                                                    6])) - 2
    
    # make it all into a data frame and sort by V2
    temp <- data.table(cbind(as.numeric(names(temp)),
                             as.numeric(temp),
                             as.double(tempup), 
                             as.double(tempdn)),
                       key = "V2")
    
    # transform V2 using dsigmtrdt
    temp <- dsigmtrdt[temp,
                      list(V1,
                           yssigm2,
                           V2,
                           V3,
                           V4),
                      roll = "nearest"] 
    
    # set column names for transformed data
    colnames(temp)[2] <- "V2trans" 
    
    # sort by V3
    temp <- data.table(temp,
                       key="V3") 
    
    # transform V3 using dsigmtrdt
    temp <- dsigmtrdt[temp, 
                      list(V1,
                           V2,
                           V2trans,
                           yssigm2,
                           V4),
                      roll = "nearest"]
    
    # set column names for transformed data
    colnames(temp)[4] <- "V3trans" 
    
    # Calculate V3 transformed
    temp$V3trans <- temp$V3trans - temp$V2trans 
    
    # sort by V4
    temp <- data.table(temp,
                       key="V4")
    
    # transform V4 using dsigmtrdt
    temp <- dsigmtrdt[temp,
                      list(V1,
                           V2,
                           V2trans, 
                           V3trans, 
                           yssigm2),
                      roll = "nearest"]
    
    # set column names for transformed data
    colnames(temp)[5] <- "V4trans"
    
    # Calculate V4 transformed
    temp$V4trans <- temp$V2trans - temp$V4trans
    
    # sort by V1 (date)
    temp <- temp[order(V1),
                 ]
    # add trace to pltydsigm
    pltydsigm <- add_trace(pltydsigm,
                           
                           # dataset is temp
                           data = temp,
                           
                           # X is time
                           x= ~V1, 
                           
                           # Y is transformed data
                           y= ~V2trans, 
                           
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
                                         # equal sign
                                         " = ",
                                         
                                         # real value
                                         as.character(tempraw), 
                                         
                                         # space
                                         " ",
                                         
                                         # unit
                                         analyses[Alistrow,
                                                  5], 
                                         
                                         # space and parenthesis start
                                         " (", 
                                         
                                         # SD value
                                         as.character(round(temp$V2, 
                                                            digits = 1)), 
                                         
                                         # "SD" and parenthesis end
                                         " SD)"), 
                           
                           # set hoverinfo to what was defined
                           hoverinfo = 'text', 
                           
                           # set data series name
                           name = analyses[Alistrow,
                                           1], 
                           
                           # assign to legend group
                           legendgroup = analyses[Alistrow,
                                                  3],
                           
                           # set Y error bar to type "data"
                           error_y = list(type="data", 
                                          
                                          # asymmetrical error bars
                                          symmetric=FALSE, 
                                          
                                          # positive error bar lengths
                                          array=temp$V3trans, 
                                          
                                          # negative error bar lengths
                                          arrayminus=temp$V4trans, 
                                          
                                          # error bar colour
                                          color = linecolours[(d-1)%%10+1])) %>%
      
      # Trace between markers
      
      # X is data range and back
      add_trace(x = c(temp$V1,
                      rev(temp$V1)),
                
                # Y is lower bounds forward and upper bounds back
                y = c(temp$V2trans - temp$V3trans,
                      rev(temp$V2trans + temp$V3trans)),
                
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
                                       3],
                
                # turn off legend entry for trace
                showlegend = FALSE,
                
                # turn off hoverinfo
                hoverinfo = 'none')
  
  # Else, if only an upper RI is defined, such as for CRP, for which a lower limit makes no sense,
  } else if (!(is.na(analyses[Alistrow,
                              7]))) {
    
    print(Data[d,1])
    # Read the measurements
    tempraw <- Data[d,
                    3:ncol(Data)]
    
    # Read the dates, too
    names(tempraw) <- names(Data)[3:ncol(Data)]
    
    # initialize some temprary data holders
    tempraw2 <- tempraw3 <- ltgtnames <- ltgt <- extreme <- NULL
    
    # Two counters
    counterA <- counterB <- 1
    
    # for every data point,
    for (i in 1:length(tempraw)) {

      # If there is a greater than sign or a less than sign in the data point,
      if (grepl(">",
                tempraw[1,
                        i]) || grepl("<",
                                     tempraw[1,
                                             i])) {

        # this is a data point beyond the measurement range, put inequality sign in "extreme"
        extreme[counterA] <- substr(tempraw[1,
                                            i],
                                    1,
                                    1)

        # put date in ltgtnames
        ltgtnames[counterA] <- names(tempraw)[i]

        # put value in ltgt
        ltgt[counterA] <- as.numeric(substr(tempraw[1,
                                                    i],
                                            2,
                                            nchar(tempraw[1,
                                                          i])))

        # increment counter
        counterA <- counterA + 1

      }

      # or if data point is not missing, nor NA,
      else if (!((is.na(tempraw[1,
                          i])) || (tempraw[1,
                                           i] == ""))) {

        # put date in tempraw2
        tempraw2[counterB] <- names(tempraw)[i]

        # put value in tempraw3
        tempraw3[counterB] <- as.numeric(tempraw[1,
                                                 i])

        # increment counter
        counterB <- counterB + 1

        # put equal sign in "extreme"
        extreme[counterA] <- "="

        # put date in ltgtnames
        ltgtnames[counterA] <- names(tempraw)[i]

        # put value in ltgt
        ltgt[counterA] <- tempraw[1,
                                  i]

        # increment counter
        counterA <- counterA + 1

      }

    }

    # make extremes and their inequality signs a data frame with strings as character
    ltgt <- as.data.frame(cbind(as.numeric(ltgtnames),
                                as.numeric(as.character(ltgt)),
                                extreme),
                          stringsAsFactors = FALSE)

    # name tempraw3 with tempraw2
    names(tempraw3) <- tempraw2

    # copy tempraw3 to tempraw
    tempraw <- tempraw3

    # clean up
    rm(tempraw2,
       tempraw3)

    # make numeric
    ltgt$V1 <- as.numeric(ltgt$V1)
    ltgt$V2 <- as.numeric(ltgt$V2)

    # scale so 100 % is at 10 SD on Y axis
    temp <- ltgt$V2 / (analyses[Alistrow,
                                10] / 10)

    # name temp using lgtg$V1
    names(temp) <- ltgt$V1

    # scale positive error bars
    tempup <- ltgt$V2 + ltgt$V2 * analyses[Alistrow,
                                           8] / 200

    # scale to Y axis
    tempup <- tempup / (analyses[Alistrow,
                                 10] / 10)

    # scale negative error bars
    tempdn <- ltgt$V2 - ltgt$V2 * analyses[Alistrow,
                                           8] / 200

    # scale to Y axis
    tempdn <- tempdn / (analyses[Alistrow,
                                 10] / 10)

    # make temp a data table sorted by V2
    temp <- data.table(cbind(as.numeric(names(temp)),
                             as.numeric(temp),
                             as.double(tempup),
                             as.double(tempdn)),
                       key = "V2")

    # transform V2 using dsigmtrdt
    temp <- dsigmtrdt[temp,
                      list(V1,
                           yssigm2,
                           V2,
                           V3,
                           V4),
                      roll = "nearest"]

    # set column names for transformed data
    colnames(temp)[2] <- "V2trans"

    # sort by V3
    temp <- data.table(temp,
                       key="V3")

    # transform V3 using dsigmtrdt
    temp <- dsigmtrdt[temp,
                      list(V1,
                           V2,
                           V2trans,
                           yssigm2,
                           V4),
                      roll = "nearest"]

    # set column names for transformed data
    colnames(temp)[4] <- "V3trans"

    # Calculate V3 transformed
    temp$V3trans <- temp$V3trans - temp$V2trans

    # sort by V4
    temp <- data.table(temp,
                       key="V4")

    # transform V4 using dsigmtrdt
    temp <- dsigmtrdt[temp,
                      list(V1,
                           V2,
                           V2trans,
                           V3trans,
                           yssigm2),
                      roll = "nearest"]

    # set column names for transformed data
    colnames(temp)[5] <- "V4trans"

    # Calculate V4 transformed
    temp$V4trans <- temp$V2trans - temp$V4trans

    # sort by V1 (date)
    temp <- temp[order(V1),
                 ]
    # set upper ribbon value to Y axis max + 0.5
    temp$ribbonup <- 10.5

    # set lower ribbon value to 0
    temp$ribbondn <- 0

    # add temp and ltgt by V1
    temp <- merge(temp,
                  ltgt,
                  by="V1")

    # set ribbondn equal to V2trans unless there is a less than sign
    setDT(temp)[extreme != "<",
                ribbondn:=V2trans]

    # set ribbonup equal to V2trans unless there is a greater than sign
    setDT(temp)[extreme != ">",
                ribbonup:=V2trans]

    # add ribbon to pltydsigm
    pltydsigm <- add_ribbons(pltydsigm,
                             # data is temp
                             data = temp,

                             # X is time
                             x= ~V1,

                             # lower ribbon values are ribbondn
                             ymin = ~ribbondn,

                             # upper ribbon values are ribbonup
                             ymax = ~ribbonup,

                             # set opacity/transparency
                             opacity=.6,

                             # set line colour
                             color = linecolours[(d-1)%%10+1],

                             # name the data series
                             name = paste0(analyses[Alistrow,
                                                    1], " kritisk forskel"),

                             # assign to legend group
                             legendgroup=analyses[Alistrow,
                                                  3],

                             # turn off legend entry for ribbon
                             showlegend=FALSE,

                             # turn off hoverinfo
                             hoverinfo='none') %>%

      # add trace to pltydsigm
      add_trace(
        # data is temp
        data=temp,

        # X is time
        x= ~V1,

        # Y is transformed data
        y= ~V2trans,

        # type is scatter
        type="scatter",

        # mode is markers and lines
        mode="markers+lines",

        # set line type
        line=list(dash = linetypes[(d-1)%/%10+1],
                  color = linecolours[(d-1)%%10+1]),
        
        # marker colour
        marker = list(color = linecolours[(d-1)%%10+1]),
        
        # define hover text, first analysis name
        text = paste0(analyses[Alistrow,
                               1],

                      # space
                      " ",

                      # equal sign
#                      " = ",

                      # inequality or equal sign
                      extreme,

                      # space
                      " ",

                      # value
                      as.character(temp$V2.y),

                      # space
                      " ",

                      # unit
                      analyses[Alistrow,
                               4]),

        # assign to legend group
        legendgroup = analyses[Alistrow,
                               3],

        # set hover info to what was defined
        hoverinfo = 'text',

        # name series
        name = analyses[Alistrow,
                        1],

        # set Y error bar to type "data"
        error_y = list(type="data",

                       # asymmetrical error bars
                       symmetric=FALSE,

                       # positive error bar lengths
                       array=temp$V3trans,

                       # negative error bar lengths
                       arrayminus=temp$V4trans,

                       # error bar colour
                       color = linecolours[(d-1)%%10+1])) %>% 
  
      # Trace between markers
      
      # X is data range and back
      add_trace(x = c(temp$V1,
                      rev(temp$V1)),
                
                # Y is lower bounds forward and upper bounds back
                y = c(temp$V2trans - temp$V3trans,
                      rev(temp$V2trans + temp$V3trans)),
                
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
                                       3],
                
                # turn off legend entry for trace
                showlegend = FALSE,
                
                # turn off hoverinfo
                hoverinfo = 'none')
    
    
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
