rankhospital <- function(state, outcome, num) {

        all_data <- read.csv("outcome-of-care-measures.csv", na.strings = "Not Available", stringsAsFactors = FALSE)
        data <- as.data.frame(all_data[ ,c(2, 7, 11, 17, 23)])
        colnames(data) <- c("hospital", "state", "heart attack", "heart failure", "pneumonia")
        if(!state %in% data[, 2]) {
          stop("Invalid state")
        } else
          if(!(num == "best" || num == "worst" || is.numeric(num))) {
            stop("Invalid num")
          }
        
        state_extract <- data[which(data$state == state), ]
        
        outcomes <- if(outcome == "heart attack") {
          state_extract[, c(1,3)]
        } else if(outcome == "heart failure") {
          state_extract[, c(1,4)]
        } else if(outcome == "pneumonia") {
          state_extract[, c(1,5)]
        } else stop("Invalid outcome")
        
        complete <- subset(outcomes, !is.na(outcomes[, 2]))
        
        sorted <- complete[order(complete[, 2], complete[, 1]), 1]
        
        count <- length(sorted)
        
        ranked <- if(is.numeric(num) && num > count) {
          NA
        } else if (num == "best") {
          sorted[1]
        } else if (num == "worst") {
          sorted[count]
        } else sorted[num]
        
        return(as.character(as.vector(ranked)))
}
