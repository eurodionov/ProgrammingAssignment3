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

get_ranked <- function(dat_by_state, num, outcome_idx) {
  dat_filtered <- as.numeric(dat_by_state[,outcome_idx])
  
  bad = is.na(dat_filtered)
  
  hospital_names <- dat_by_state[,2][!bad]
  dat_filtered <- dat_filtered[!bad]
  
  if(num == "best") {
    idx <- 1
  } else if(num == "worst") {
    idx <- length(dat_filtered)
  } else if (is.numeric(num)) {
    idx <- num
  } else {
    print(num)
    stop("invalid input value num")
    
  }
    
  
  if (length(dat_filtered) < idx) {
    result <- NA
  } else {
    #  print(cbind(hospital_names,data_filtered))
    
    names_perm <- order(hospital_names)
    hospital_names <- hospital_names[names_perm]
    dat_filtered <- dat_filtered[names_perm]
    #  print(cbind(hospital_names,data_filtered))
    
    res_ord <- order(dat_filtered, hospital_names)
    result <- hospital_names[res_ord[idx]]
  }
  result
}

rankall <- function(outcome, num = "best") { 
  outcome_idx <- outcome_to_index(outcome)
  
  data <- read.csv("C:\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses='character')
  
  data_by_states <- split(data, data[,7])
  states = levels(factor(data[,7]))
  
  ranked <- character()
  
  for (state in states) {
    data_by_state <- data_by_states[[state]]
    hosp_name <- get_ranked(data_by_state, num, outcome_idx)
    print(hosp_name)
    ranked <- c(ranked, hosp_name)
  }
  
  data.frame(hospital=ranked, state=states)
  
}

#tail(rankall("heart failure", 1),10)
