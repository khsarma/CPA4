#
best <- function(state=NA, outcome=NA) {
      #Read outcome csv file
      outcomefile <- read.csv("outcome-of-care-measures.csv")
      
      #Subset outcome data frame to get required columns
      bestset <- outcomefile[,c(2,7,11,17,23)]
      colnames(bestset) <- c("HospName","State","HA","HF","PN")
      
      outcomeset <- c("heart attack","heart failure","pneumonia")
      
      if (!any(state == bestset$State)) {
          print("Invalid State")
      } else if (!any(outcome == outcomeset)) {
          print("Invalid Outcome")
      } else if (outcome == 'heart attack') {
          ss <- subset(bestset,select = c(HospName,HA),
                     subset = (State == state))
          ss <- ss[order(ss$HA,ss$HospName),]
          paste(ss[1,1])
      } else if (outcome == 'heart failure') {
          ss <- subset(bestset,select = c(HospName,HF),
                     subset = (State == state))
          ss <- ss[order(ss$HF,ss$HospName),]
          paste(ss[1,1])
      } else if (outcome == 'pneumonia') {
          ss <- subset(bestset,select = c(HospName,PN),
                     subset = (State == state))
          ss <- ss[order(ss$PN,ss$HospName),]
          paste(ss[1,1])
      }
}