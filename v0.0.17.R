# Graphical representation of lab results, with journal notes
#
# version 0.0.17 - SP compatible (meaning that SP data pasted into Excel can be imported).
# With new, log x axis!

# Left to implement/ideas:
# - Look at partial time, possibly at first present histogram-like plot of
#   data density in which user can pick a range to view
# - When looking at only some traces, +/- 2 SD lines disappear. Somehow keep 
#   them in at all times
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
library(svDialogs)

#require(scales)

# Set working directory
setwd("~/Ongoing projects/Graphical Lab/v0.1/SP/")

#---------------------------------------------------------

# Import analyses characteristics
source("~/Ongoing projects/Graphical Lab/v0.1/SP/imp-anals.R")

#---------------------------------------------------------

# Read data
source("~/Ongoing projects/Graphical Lab/v0.1/SP/SPimport3.R")

harvesttime <- tic()

#---------------------------------------------------------

# Generate transformation data to be used as lookup table since the inverse equation for the double sigmoid is not nice
source("~/Ongoing projects/Graphical Lab/v0.1/SP/transf.R")

#---------------------------------------------------------

# Format axes
source("~/Ongoing projects/Graphical Lab/v0.1/SP/axesf0.0.17.R")

#-----------------------------------------------------------------

# Initialize plot
source("~/Ongoing projects/Graphical Lab/v0.1/SP/initplot0.0.17.R")

#-----------------------------------------------------------------

# Line colours
source("~/Ongoing projects/Graphical Lab/v0.1/SP/linecolours.R")

# Functions

# For when a data point contains "<" or ">"
ltgtDP <- function(counter,
                   temprawentry) {
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
  
  return(c(tempraw2,
           tempraw3,
           extreme))
  
}

# For when a data point is just a number
inRIDP <- function(counter,
                   temprawentry) {
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

  return(c(tempraw2,
           tempraw3,
           extreme))
  
}

# Transform and add data to plot
transform_add <- function(tmp,
                          pltydsigm,
                          yssigm2,
                          dsigmtrdt,
                          linetype,
                          linecolour,
                          analysis) {
  
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
  
  # Look at extreme signs and set error bar lengths accordingly
  tmp <- tmp %>% 
    mutate(dntrans = ifelse(extreme == "<",
                             10 - abs(SDtrans),
                             dntrans)) %>% 
    mutate(uptrans = ifelse(extreme == ">",
                             10 - abs(SDtrans),
                             uptrans))

  # Define SD string
  if (is.numeric(tmp$value)) { #} != 0) {
    SDstring <- paste0(
      # space and parenthesis start
      " (",
      
      # extreme sign
      tmp$extreme,
      
      # space
      " ",
      
      # SD value
      as.character(round(tmp$SD,
                         digits = 1)),
      
      # "SD" and parenthesis end
      " SD)"
    )
  } else {
    SDstring = ""
  }
  
  # Construct hovertext if SD is beyond 2
  tmp$hovertext <- paste0(analysis[1],
                          
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
                          analysis[2],
                          
                          # SD string
                          SDstring)
  
  # SD values between -2 and 2 replaces hovertext with ""
#  tmp$hovertext[abs(tmp$SD) < 2] <- ""
  
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
                         line = list(dash = linetype,
                                     color = linecolour),
                         
                         # marker colour
                         marker = list(color = linecolour),
                         
                         # define hovertext, first analysis name
                         text = tmp$hovertext,
                         
                         # set hoverinfo to what was defined
                         hoverinfo = 'text',
                         
                         # set data series name
                         name = analysis[1],
                         
#                         # set group
#                         customdata = analysis[8],
                         
                         # assign to legend group
                         legendgroup = analysis[1],
                         
                         # set Y error bar to type "data"
                         error_y = list(type="data",
                                        
                                        # asymmetrical error bars
                                        symmetric=FALSE,
                                        
                                        # positive error bar lengths
                                        array=tmp$uptrans,
                                        
                                        # negative error bar lengths
                                        arrayminus=tmp$dntrans,
                                        
                                        # error bar colour
                                        color = linecolour)) %>%
    
    # Play with what hovertexts are being displayed - doesn't work the way I want it to; i.e.
    # I would like to show only those hovertexts that are from datapoints within a reasonably short
    # distance from the cursor
    layout(hoverdistance = 1,
           hovermode = "x") %>% 
    
    # Trace between markers
    
    # X is data range and back
    add_trace(x = c(tmp$time,
                    rev(tmp$time)),
              
              # Y is lower bounds forward and upper bounds back
              y = c(tmp$SDtrans + tmp$uptrans,
                    rev(tmp$SDtrans - tmp$dntrans)),
              
              # Type scatter
              type = 'scatter',
              
              # Mode lines
              mode = "lines",
              
              # Fill to oneself
              fill = 'toself',
              
              # Dynamic fill colour, semitransparent
              fillcolor = paste0(linecolour,
                                 "22"),
              
              # Transparent line
              line = list(color = 'transparent'),
              
              # Transparent markers
              marker = list(color = 'transparent'),
              
              # assign to legend group
              legendgroup = analysis[1],
              
              # turn off legend entry for trace
              showlegend = FALSE,
              
              # turn off hoverinfo
              hoverinfo = 'none')
  
  # Return the new plot
  return(pltydsigm)
}

#-----------------------------------------------------------------

# Prepare data - change to function? Include class/group data
harvested <- list()
Metadata <- as_tibble()
for (d in 1:nrow(Data)) { # For every analyte,
  
  # Read the measurements
  tempraw <- Data[d,
                  2:ncol(Data)]
  
  # Initialize a temporary data holder
  tmp <- NULL
  
  # A counter
  counter <- 1
  
  # Harvest data - remove NAs, look for inequality signs
  # For every data point,
  for (i in 1:length(tempraw)) {
    
    # If there is a greater than sign or a less than sign in the data point,
    if (grepl(">",
              tempraw[1,
                      i]) || grepl("<",
                                   tempraw[1,
                                           i])) {
      
      # Add datapoint using function ltgtDP
      tmp <- rbind(tmp,ltgtDP(counter,tempraw[,i]))
      counter <- counter + 1
      
    }
    
    # Else, if it's not empty or NA,
    else if (!((tempraw[1,
                        i] == "") || (is.na(tempraw[1,
                                                    i])))) {
      
      # Add datapoint using function inRIDP
      tmp <- rbind(tmp,inRIDP(counter,tempraw[,i]))
      counter <- counter + 1
      
    }
    
  }
  # Convert harvested datapoints including (in)equality signs to table
  tmp <- as.data.frame(tmp,
                       stringsAsFactors = FALSE)
  
  # If any data has been found,
  if (nrow(tmp) > 0) {
    # Convert to numeric
    tmp$V1 <- as.numeric(tmp$V1)
    tmp$V2 <- as.numeric(tmp$V2)
    
    # Name columns
    names(tmp) <- c("time",
                    "value",
                    "extreme")
    
    # Find this analysis in this patient's analyses
    Alistrow <- ThisPtAnalyses[match(Data[d,
                                          1],
                                     ThisPtAnalyses$SPnavn),]
    
    # If both an upper and a lower reference interval is defined,
    if (!((is.na(Alistrow[3])) || is.na(Alistrow[4]))) {
      
      # Scale so reference interval is between -2 SD and +2 SD
      tmp$SD <- (4 * (tmp$value - unlist(Alistrow[3])) / (unlist(Alistrow[4]) - unlist(Alistrow[3]))) - 2
      
      # Calculate length of positive error bar
      tmp$up <- tmp$SD + abs(tmp$SD) * unlist(Alistrow[5]) #/ 200
      
      # Calculate length of negative error bar
      tmp$dn <- tmp$SD - abs(tmp$SD) * unlist(Alistrow[5]) #/ 200
      
      # Else, if only an upper RI is defined, such as for CRP, for which a lower limit makes no sense,
    } else if ((is.na(Alistrow[3])) && (!(is.na(Alistrow[4])))) {
      # If the uRI is nonzero,
      if (unlist(Alistrow[4]) != 0) {
        # Scale so URI is at +2 SD on Y axis
        tmp$SD <- tmp$value / unlist(Alistrow[4] / 2)
        
        # Scale positive error bars
        tmp$up <- tmp$SD + tmp$SD * unlist(Alistrow[5]) #/ 200
        
        # Scale negative error bars
        tmp$dn <- tmp$SD - tmp$SD * unlist(Alistrow[5]) #/ 200
        
        # Else,
      } else {
        # Put in 0s
        tmp$SD <- tmp$up <- tmp$dn <- 0
      }
      
      # Else, if only a lower RI is defined, such as for vitamin D, for which an upper limit makes no sense,
    } else if ((!(is.na(Alistrow[3]))) && (is.na(Alistrow[4]))) {
      
      # Scale so URI is at -2 SD on Y axis
      tmp$SD <- tmp$value / unlist(Alistrow[3] / 2)
      
      # Scale positive error bars
      tmp$up <- tmp$SD + tmp$SD * unlist(Alistrow[5]) #/ 200
      
      # Scale negative error bars
      tmp$dn <- tmp$SD - tmp$SD * unlist(Alistrow[5]) #/ 200
      
      # There's no lower or upper RI defined!  
    } else {
      # Scale results to area!
      
      # TODO
      
      # Put in 0s
      tmp$SD <- tmp$up <- tmp$dn <- 0
    }
    
  # Add to list of harvested data
  harvested[[length(harvested) + 1]] <- tmp

  # Get Class, Group, normality and whether number of replies > 1 into Metadata for this analysis
  Metadata <- rbind(Metadata,
                    c(Alistrow$Class,
                      Alistrow$Group,
                      ((max(tmp$SD) < 2) & (min(tmp$SD) > -2)),
                      (nrow(tmp) == 1),
                      Alistrow$SPnavn,
                      Alistrow$Enhed))
  }
}

colnames(Metadata) <- c("Class",
                        "Group",
                        "Normal",
                        "Single",
                        "Name",
                        "Unit")

harvesttime <- toc() - harvesttime
###

# Ask user how (s)he would like to present the data
# - Skip analytes with only normal results?
# - Split evenly (but randomly) in plots of max 10 analytes each?
# - Split in groups/classes
# - Skip analytes with only one (normal) result


tic()
# Plot only normal results
for (h in 1:length(harvested)) {
  if (Metadata[h,3] == FALSE) {
    
    # Add to plot
    pltydsigm <- transform_add(harvested[[h]],
                               pltydsigm,
                               yssigm2,
                               dsigmtrdt,
                               linetypes[(h-1)%/%10+1],
                               linecolours[(h-1)%%10+1],
                               c(Metadata[h,5:6]))
    
  }
}



###
pltydsigm # make the graph!


# save as HTML widget, too
# htmlwidgets::saveWidget(as_widget(pltydsigm), 
#                         "~/Ongoing projects/Graphical Lab/index.html")


# pltydsigm <- pltydsigm %>% layout(
#   title = "Button",
#   updatemenus = list(
#     list(
#       type = "buttons",
#       buttons = list(
#         list(method = "restyle",
#              args = list("line.color",
#                          "blue"),
#              label = "Blue"),
#         list(method = "restyle",
#              args = list("line.color",
#                          "red"),
#              label = "Red")
#       )
#     )
#   )
# )

# Try this with hovertexts
# Two types of hovertexts; one for data within -2 - 2 SD, and another for data outside that.


# pltydsigm <- pltydsigm %>% layout(
#   title = "SD",
#   updatemenus = list(
#     list(
#       type = "dropdown",
#       buttons = list(
#         list(method = "restyle",
#              args = list("line.color",
#                          "blue"),
#              label = "Blue"),
#         list(method = "restyle",
#              args = list("line.color",
#                          "red"),
#              label = "Red")
#       )
#     )
#   )
# )
# 
# 
# # Play with groups of analyses
# 
# # There are three groups:
# #   Hematology
# #   Immunology
# #   Chemistry
# 
# pltydsigm <- pltydsigm %>% layout(
#   updatemenus <- list(
#     list(
#       active = -1,
#       type= 'buttons',
#       buttons = list(
#         list(
#           label = "Alle",
#           method = "update",
#           args = list(list(visible = c(TRUE, 
#                                        TRUE,
#                                        TRUE)))),
#         list(
#           label = "Hæmatologi",
#           method = "update",
#           args = list(list(visible = c(TRUE, 
#                                        FALSE,
#                                        FALSE)))),
#         list(
#           label = "Immunologi",
#           method = "update",
#           args = list(list(visible = c(FALSE,
#                                        TRUE, 
#                                        FALSE)))),
#         list(
#           label = "Kemi",
#           method = "update",
#           args = list(list(visible = c(FALSE,
#                                        FALSE,
#                                        TRUE))))))))
# 

harvesttime
toc()
