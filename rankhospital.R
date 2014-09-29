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

rankhospital <- function(state, outcome, num = "best") { 
  outcome_idx <- outcome_to_index(outcome)
  
  data <- read.csv("C:\\rprog-data-ProgAssignment3-data\\outcome-of-care-measures.csv", colClasses='character')
  
  data_by_state <- split(data, data[,7])[[state]]
  
  if (is.null(data_by_state)) stop("invalid state")
  
  data_filtered <- as.numeric(data_by_state[,outcome_idx])
  
  bad = is.na(data_filtered)
  
#  print(data_by_state[,2])

  hospital_names <- data_by_state[,2][!bad]
  data_filtered <- data_filtered[!bad]

  if(num == "best") {
    idx <- 1
  } else if(num == "worst") {
    idx <- length(data_filtered)
  } else if (is.numeric(num)) {
    idx <- num
  } else {
    print(num)
    stop("invalid input value num")
    
  }

  
  if (length(data_filtered) < idx) {
    res_val <- NA
  } else {


  names_perm <- order(hospital_names)
  hospital_names <- hospital_names[names_perm]

  data_filtered <- data_filtered[names_perm]

  
  res_ord <- order(data_filtered, hospital_names)


#  print(hospital_names[res_ord[idx]])
  res_val <- hospital_names[res_ord[idx]]
  }
  
  res_val
  
}

#rankhospital('CA', "heart attack", "wdorst")
