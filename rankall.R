#This function pulls out the "num"th hospital in each state by performance on the selected outcome

rankall <- function(outcome, num = "best") {
#Pull data and created smaller frame containing only the useful columns
        all_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        data <- as.data.frame(all_data[ ,c(2, 7, 11, 17, 23)])
        colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        
#Check validity of num argument
        if(!(num == "best" || num == "worst" || is.numeric(num))) {
            stop("Invalid num")
          }

#Create even smaller data frame containing only the relevant outcome. Also checks validity of selected outcome.
        outcomes <- if(outcome == "heart attack") {
          data[, c(1,2,3)]
        } else if(outcome == "heart failure") {
          data[, c(1,2,4)]
        } else if(outcome == "pneumonia") {
          data[, c(1,2,5)]
        } else stop("Invalid outcome")

#Remove NAs        
        complete <- subset(outcomes, !is.na(outcomes[, 3]))

#Create vector containing all states        
        states <- levels(complete[, 2])
        
#Create empty vector to be filled later with outputs        
        output <- vector()

#Loop through each state ordering hospitals by outcome performance, then alphabetically        
        for (i in 1:length(states)){
          statedata <- complete[grep(states[i], complete$state), ]
          stateorder <- statedata[order(statedata[, 3], statedata[, 1]), ]

#Convert "best" and "worst" to numeric values and give "NA" if num is higher than number of ranked hospitals in state
          num2 <- if(num == "best"){
            1
          } else if(num == "worst"){
            nrow(statedata)
          } else if(is.numeric(num) && num > nrow(statedata)) {
            NA
          } else num
          
#Fills empty vector with the name of the hospital with the selected rank in each state, as well as the state abbreviation
          output <- append(output, as.character(stateorder[num2, 1]))
          output <- append(output, as.character(stateorder[1, 2]))
          
#Vector is converted into a matrix, then a data frame (byrow = TRUE ensures matrix is populated in the correct order)
        }
        frame <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
        colnames(frame) <- c("hospital","state")
        return(frame)
}
