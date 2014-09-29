outcome_to_index <- function(outcome) {
  
  if (outcome == "heart attack") {
    index <- 11
  } else if (outcome == "heart failure") {
    index <- 17
  } else if (outcome == "pneumonia") {
    index <- 23
  } else {
    stop("invalid outcome")
    index <- 0
  }
  
  index
}

get_min <- function(vect) {
  min_val <- 1233000
  for(q in 1:length(vect)) {
    if(!is.na(vect[q])) {
      if (min_val > vect[q]) {
        min_val <- vect[q]
      }
    }
  }
  
  min_val
}

best <- function(state, outcome) {
  outcome_idx <- outcome_to_index(outcome)
  
  data <- read.csv("C:\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses='character')
  
  data_by_state <- split(data, data[,7])[[state]]
  
  if (is.null(data_by_state)) stop("invalid state")
  
  data_filtered <- as.numeric(data_by_state[,outcome_idx])
  
#  print(data_filtered)
  
  minim_value <- get_min(data_filtered)
  
#  print(minim_value)
  
  filtered_hospitals <- character()
  
  for(q in 1:length(data_filtered)) {
#    print(data_filtered[q])
    if(!is.na(data_filtered[q])) {
      if (data_filtered[q] <= minim_value) {
#        print(data_by_state[,2][q])
        filtered_hospitals <- c(data_by_state[,2][q])
      }
    }
  }
  
  filtered_hospitals <- sort(filtered_hospitals)
  
#  filtered_hospitals <- sort(split(data_by_state, data_by_state[,outcome_idx])[[minim_value]][,2])
  
  filtered_hospitals
  
## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death ## rate
}

