#
library(dplyr)

rankall <- function(outcome, num = 'best') {
        outcomefile <- read.csv('outcome-of-care-measures.csv',
                                colClasses = 'character')
        outcomeset <- outcomefile[, c(2,7,11,17,23)]
        colnames(outcomeset) <- c('hospital','state','heart attack',
                                  'heart failure','pneumonia')
        
        if (outcome == 'heart attack') {colname <- 'ha'}
        if (outcome == 'heart failure') {colname <- 'hf'}
        if (outcome == 'pneumonia') {colname <- 'pn'}
        if (!colname %in% c('ha','hf','pn')) { 
              print("Invalid outcome") 
        } else {
              outcomesubset <- outcomeset[c('hospital','state',colname)]     
              outcomesubset[,3] <- suppressWarnings(as.numeric(
                                        outcomesubset[,3]))
              outcomesubset <-  outcomesubset %>%
                                group_by(state) %>% 
                                mutate(rankoc = order(colname,hospital))
              if (num == 'best') { num <- 1 }
              if (num == 'worst') { num <- nrow(outcomesubset) }
              print(head(outcomesubset))
              print(outcomesubset[outcomesubset$rankoc == num,c(1,2)])
          }
}