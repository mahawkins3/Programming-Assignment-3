rankhospital <- function(outcome, num = "best") {

        all_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available")
        data <- as.data.frame(all_data[ ,c(2, 7, 11, 17, 23)])
        colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        if(!(num == "best" || num == "worst" || is.numeric(num))) {
            stop("Invalid num")
          }

        outcomes <- if(outcome == "heart attack") {
          data[, c(1,2,3)]
        } else if(outcome == "heart failure") {
          data[, c(1,2,4)]
        } else if(outcome == "pneumonia") {
          data[, c(1,2,5)]
        } else stop("Invalid outcome")
        
        complete <- subset(outcomes, !is.na(outcomes[, 3]))
        
        states <- levels(complete[, 2])
        output <- vector()
        
        for (i in 1:length(states)){
          statedata <- complete[grep(states[i], complete$state), ]
          stateorder <- statedata[order(statedata[, 3], statedata[, 1]), ]
          num2 <- if(num == "best"){
            1
          } else if(num == "worst"){
            nrow(statedata)
          } else if(is.numeric(num) && num > nrow(statedata)) {
            NA
          } else num
          output <- append(output, as.character(stateorder[num2, 1]))
          output <- append(output, as.character(stateorder[1, 2]))
        }
        frame <- as.data.frame(matrix(output, length(states), 2, byrow = TRUE))
        colnames(frame) <- c("hospital","state")
        return(frame)
}
