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
      } else {
          ss <- subset(bestset,select = c(HospName,HA),
                     subset = (State == state))
          ss <- ss[order(ss$HA),]
          print(ss[1,1])
      }
        
}