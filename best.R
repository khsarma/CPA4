#
best <- function(state, outcome) {
      #Read outcome csv file
      outcomefile <- read.csv("outcome-of-care-measures.csv"
                              ,colClasses = "character")
      
      #Subset outcome data frame to get required columns
      bestset <- outcomefile[,c(2,7,11,17,23)]
      colnames(bestset) <- c("hospital","States","ha"
                             ,"hf","pn")
      if (outcome == 'heart attack') { col <- 3 }
      if (outcome == 'heart failure') { col <- 4 }
      if (outcome == 'pneumonia') { col <- 5 }
      
      if (!any(state == bestset$States)) {
          print("Invalid State")
      } else if (!any(col %in% c(3,4,5))) { 
            print("Invalid Outcome")
      } else  {
          ss <- bestset[bestset$States == state,c(1,col)]
          ss[,2] <- suppressWarnings(as.numeric(ss[,2]))
          ss <- ss[order(ss[,2],ss[,1], na.last = NA),]
          paste(ss[1,1])
      }
}