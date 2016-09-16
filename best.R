best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  
  
  ## Read Data
  detail <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check argument validity
  chkstate <- detail$State
  chkoutcome <- c("heart attack", "heart failure", "pneumonia")
  if(sum(chkstate == state) == 0) { stop("invalid state") }
  if(sum(chkoutcome == outcome) == 0) {stop("invalid outcome")}
  state_detail <- detail[detail[,7] == state,]
  raw_outcome <- c(11,17,23)
  col_outcome <- raw_outcome[chkoutcome == outcome]
  lowest <- min(as.numeric(state_detail[,col_outcome]),na.rm=TRUE)
  print(lowest)
  outcome_col <- as.numeric(state_detail[,col_outcome])
  hospital <- state_detail[outcome_col == lowest,2]
    hospital<-sort(hospital[!is.na(hospital)])
    hospital[1]
    }

