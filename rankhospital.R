#
rankhospital <- function(state, outcome, num='best') {
  #Read csv file
  outcomefile <- read.csv('outcome-of-care-measures.csv', 
                          colClasses = 'character')
  outcomeset <- outcomefile[,c(2,7,11,17,23)]
  colnames(outcomeset) <- c('HospitalName','State','HA','HF','PN')
  if (outcome == 'heart attack') {colnumber <- 3}
  if (outcome == 'heart failure') {colnumber <- 4}
  if (outcome == 'pneumonia') {colnumber <- 5}
  
  if (!state %in% outcomeset$State) {
      print("invalid state")
  } else if (!outcome %in% c('heart attack','heart failure','pneumonia')) {
      print("invalid outcome")
  } else {
      rankset <- subset(outcomeset, select = c(HospitalName,colnumber),
                        subset = (State == state))
      rankset[,2] <- suppressWarnings(as.numeric(rankset[,2]))
      rankset.sort <- rankset[order(rankset[,2],rankset[,1],na.last = NA),]
      rankset.sort[,'Rank'] <- c(1:nrow(rankset.sort))
      
      if (num == 'best') { num <- 1}
      if (num == 'worst') { num <- nrow(rankset.sort)}
      if (num > nrow(rankset.sort)) { stop("NA", call. = T)}
      
      print(rankset.sort[rankset.sort$Rank == num,1])
  }
  
}