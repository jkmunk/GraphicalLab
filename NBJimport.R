# Import from SP as delivered by Nils Bruun Jørgensen

# Data is in one column in Excel file

# Read in as a simple vector at first

setwd("H:/Ongoing projects/Graphical Lab/v0.1/")
library(readxl)
library(dplyr)

#RawIn <- read_excel("Ongoing projects/Graphical Lab/v0.1/Prøvesvar fra NBJ.xlsx", 
#                    col_names = FALSE)

RawIn <- unlist(read_xlsx("Prøvesvar fra NBJ.xlsx", 
                    col_names = FALSE))

# Collect time stamps

Times <- !apply(as.matrix(as.numeric(RawIn)), 
                FUN = is.na, 
                MARGIN = 1)

# Collect time stamp line numbers
TimesIndexes <- which(Times)

Times <- RawIn[which(Times)] #%>% unique()

Times <- as.POSIXct(as.Date(as.numeric(Times), 
                            origin = "1899-12-30"))


# Make a list of analyses carried out
counter1 <- counter2 <- counter3 <- 0
Names <- ""
NameVal <- list()
for (e in RawIn) {
  counter1 <- counter1 + 1
  if (counter1 %in% TimesIndexes) {
    counter3 <- counter3 + 1
    counter4 <- 0
    NameVal[counter3] <- Times[counter3]
  }
  else if (is.na(e)) {
    
  }
  else {
    counter2 <- counter2 + 1
    Names[counter2] <- strsplit(e,":")[[1]][1]
    counter4 <- counter4 + 1
    NameVal[[counter3]][counter4] <- list(strsplit(e,":")[[1]])
  }
}

names(NameVal) <- Times

Names <- Names %>% 
  unique()

# Find possible duplicate times
DupTimes <- names(table(Times)[which(as.logical(table(Times) - 1))])

# Merge lists with identical times

