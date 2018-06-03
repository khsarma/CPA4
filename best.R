#
best <- function(state=NA, outcome=NA) {
      #Read outcome csv file
      outcomefile <- read.csv("outcome-of-care-measures.csv"
                              ,colClasses = "character")
      
      #Subset outcome data frame to get required columns
      bestset <- outcomefile[,c(2,7,11,17,23)]
      colnames(bestset) <- c("hospital","States","heart attack"
                             ,"heart failure","pneumonia")
      
      if (!any(state == bestset$States)) {
          print("Invalid State")
      } else if (!any(outcome == colnames(bestset)[3:5])) { 
            print("Invalid Outcome")
      } else  {
          ss <- subset(bestset,select = c(hospital,as.character(outcome)),
                     subset = (States == state))
          ss <- ss[order(ss$outcome,ss$hospital),]
          paste(ss[1,1])
      }
}