#This function returns the hospital with the "best" (i.e. lowest mortality rate) performance on the specified outcome in the specified state.

best <- function(state, outcome) {
  #Read all data from the file. "Not Available" is analogous to "NA" in the dataset, so na.strings converts this string to NA. 
  #stringsAsFactors ensures that strings are not converted to factors
        all_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
  #Limit the data scope to just the relevant columns
        data <- as.data.frame(cbind(all_data[,2], all_data[, 7], all_data[, 11], all_data[, 17], all_data[, 23]))
  #Rename the columns to be more user-friendly
        colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
  #Check that the state abbreviation passed in the arguments is valid
        if(!state %in% all_data[, 7]) {
          stop("Invalid state")
  #Check that the outcome passed in the arguments is valid
        } else {
          if(!outcome %in% c("heart attack", "heart failure", "pneumonia")){
            stop("Invalid outcome")
          } else {
  #If both arguments are valid, subset the rows of the "data" object where the "state" value is equal to the state passed as an argument to the function
            state_subset <- which(data[, "state"] == state)
  #Assign to a new object only the rows for the selected state
            state_extract <- data[state_subset, ]
  #Create a numeric vector containing all the values for the selected outcome within the selected state
            rate <- as.numeric(as.vector(state_extract[, outcome]))
  #Create a vector containing just the minimum value from the above vector
            minrate <- min(rate, na.rm = TRUE)
  #Create a vector containing the name of any hospitals where the value of the selected outcome is equal to the minimum value
            minhosp <- as.vector(state_extract[rate == minrate, 1])
  #In case there is a tie, the sort function combined with [1] will pull out the earliest name by alphabetical order
            sort(minhosp)[1]
          }
        }
}