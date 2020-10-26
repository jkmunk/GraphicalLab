
# analyses <- read.csv("~/Ongoing projects/Graphical Lab/v0.1/SP/analysesSP2.csv",
#                      sep = ";", 
#                      encoding = "UTF-8")

analyses <- read.csv2("~/Ongoing projects/Graphical Lab/v0.1/SP/analysesSP2.csv",
                      encoding="UTF-8",
                      stringsAsFactors=FALSE)

colnames(analyses) <- c("SPnavn", colnames(analyses[,2:ncol(analyses)]))
#analyses$AgeToY <- as.numeric(as.character(analyses$AgeToY))

# analyses <- analyses[,
#                      !(names(analyses) %in% "X")] # Drop 1st column with numbers

ptSex <- dlgInput("Indtast patientens køn (M eller F): ")$res
ptAge <- as.numeric(dlgInput("Indtast patientens alder i år: ")$res)

# Get only those analysis list rows that are relevant to this patient's age and sex

ThisPtAnalyses <- analyses %>% 
  select(SPnavn,
         Enhed,
         Lnorm,
         Hnorm,
         Cdpct,
         LL,
         UL,
         Group,
         Class,
         Sex,
         AgeToY,
         NPU) %>% 
  filter(Sex %in% c(ptSex,"B")) %>% 
  filter(AgeToY >= ptAge) %>% 
  group_by(SPnavn) %>% 
  slice(which.min(AgeToY))

rm(analyses)

