# Import from SP as delivered by Nils Bruun Jørgensen

# Data is in one column in Excel file

# Read in as a simple vector at first

setwd("H:/Ongoing projects/Graphical Lab/v0.1/SP/")
library(readxl)
library(dplyr)

RawIn <- unlist(read_xlsx("Proevesvar fra NBJ 2 prolaktinom.xlsx", 
                    col_names = FALSE))


#read_excel("~/Ongoing projects/Graphical Lab/v0.1/SP/Proevesvar fra NBJ 2.xlsx", 
#                                   sheet = "Prolaktinom")

# Collect time stamps

Times <- !apply(as.matrix(as.numeric(RawIn)), 
                FUN = is.na, 
                MARGIN = 1)

# Collect time stamp line numbers
TimesIndexes <- which(Times)

Times <- RawIn[which(Times)]

Times <- as.numeric(Times) - 2

# Duplicate times
# Find possible duplicate times
#DupTimes <- names(table(Times)[which(as.logical(table(Times) - 1))])

# Make a list of lists of the data

LineCounter <- TimesCounter <- AnalyteCounter <- 0
TimesList <- AnalyteNames <- list()
# For each line in RawIn
for (e in RawIn) {
  LineCounter <- LineCounter + 1
  # If it's not NA
  if (!(is.na(e))) {
    # If it's a date (because the line number is in TimesIndexes)
    if (LineCounter %in% TimesIndexes) {
      TimesCounter <- TimesCounter + 1
      AnalyteCounter <- 0
      TimesList[TimesCounter] <- TimesCounter
    }
    # Else it could be something we should ignore
    else if (grepl("Gruppe",
                   e)) {
      
    }
    else if (grepl("REFLEKS",
                   e)) {
      
    }
    else if (grepl("Erstattet",
                   e)) {
      
    }
    else if (grepl("cancelled",
                   e)) {
      
    }
    else if (grepl("\\*\\*\\*\\*\\*",
                   e)) {
      
    }
    else if (grepl("Ventrikelfrekvens",
                   e)) {
      
    }
    else if (grepl("PR interval",
                   e)) {
      
    }
    else if (grepl("QRS interval",
                   e)) {
      
    }
    else if (grepl("QT interval",
                   e)) {
      
    }
    else if (grepl("Fridericia",
                   e)) {
      
    }
    else if (grepl("Bassett",
                   e)) {
      
    }
    else if (grepl("P akse",
                   e)) {
      
    }
    else if (grepl("T akse",
                   e)) {
      
    }
    else if (grepl("R akse",
                   e)) {
      
    }
    else if (grepl("P taks varighed",
                   e)) {
      
    }
    else if (grepl("RR interval",
                   e)) {
      
    }
    else if (grepl("Autofortolkning",
                   e)) {
      
    }
    else if (grepl("ELEKTROKARDIOGRAFI",
                   e)) {
      
    }
    else if (grepl("RADIOLOGI CT APOPLEKSI",
                   e)) {
      
    }
    else if (grepl("RADIOLOGI MR HALSANGIO",
                   e)) {
      
    }
    else if (grepl("RADIOLOGI CT NYRER",
                   e)) {
      
    }
    else if (grepl("VARICELLA-ZOSTER VIRUS",
                   e)) {
      
    }
    else if (grepl("Særaftale",
                   e)) {
      
    }
    else if (grepl("Afbestilt",
                   e)) {
      
    }
    
    # Else, it's analyte data
    else {
      AnalyteCounter <- AnalyteCounter + 1
      # Split into list with name and result
      TimesList[[TimesCounter]][AnalyteCounter] <- list(strsplit(e,
                                                                 ":")[[1]])
      # Remove (H)
      TimesList[[TimesCounter]][[AnalyteCounter]][2] <- str_remove(TimesList[[TimesCounter]][[AnalyteCounter]][2],
                                                                   "\\(H\\)")
      # Remove (L)
      TimesList[[TimesCounter]][[AnalyteCounter]][2] <- str_remove(TimesList[[TimesCounter]][[AnalyteCounter]][2],
                                                                   "\\(L\\)")
      # Replace "Udført" with NA
      TimesList[[TimesCounter]][[AnalyteCounter]][2] <- na_if(TimesList[[TimesCounter]][[AnalyteCounter]][2], 
                                                              " Udført")
      # Replace "Ikke påvist" with "0"
      TimesList[[TimesCounter]][[AnalyteCounter]][2] <- str_replace(TimesList[[TimesCounter]][[AnalyteCounter]][2],
                                                                   "Ikke påvist",
                                                                   "0")
      
      # If the analyte name suggests that this analysis if of the form (0 1), convert nonzero results to 1
      if (grepl(pattern = "0 1", x = TimesList[[TimesCounter]][[AnalyteCounter]][1])) {
        if (!((is.na(TimesList[[TimesCounter]][[AnalyteCounter]][2])) || (TimesList[[TimesCounter]][[AnalyteCounter]][2] == " 0"))) {
          TimesList[[TimesCounter]][[AnalyteCounter]][2] = " 1"
        }
      }
    }
  }
}

# Remove TimesList[[]] that are all cancelled
for (T in length(TimesList):1) {
  if (is.numeric(TimesList[[T]])) {
    TimesList[[T]] <- NULL
    Times <- Times[-T]
  }
}

Data <- as_tibble(do.call(rbind,TimesList[[1]]))
colnames(Data) <- c("Analyse", Times[1])
for (Ti in 2:length(TimesList)) {
  tmp <- as_tibble(do.call(rbind,TimesList[[Ti]]))
  colnames(tmp) <- c("Analyse", Times[Ti])
  Data <- full_join(Data,
                    tmp,
                    by = c("Analyse" = "Analyse"))
}

# Clean up in text replies
rm(AnalyteNames,AnalyteCounter,e,LineCounter,RawIn,Times,TimesCounter,TimesIndexes,tmp,TimesList,Ti)
