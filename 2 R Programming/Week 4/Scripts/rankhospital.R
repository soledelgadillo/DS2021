rankhospital <- function(state, outcome, num) {
        # Read the outcome data
        df_outcome <- read.csv("Data/ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        # Check that state and outcome are valid
        if (!(state %in% df_outcome$State)) stop("invalid state")
        possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!(outcome %in% possible_outcomes)) stop("invalid outcome")
        
        # Return hospital name in that state with the given rank 30-day death rate
        
        df_outcome <- split(df_outcome, df_outcome$State)
        df_outcome <- df_outcome[[state]]
        df_outcome <- df_outcome[order(df_outcome$Hospital.Name), ]
        
        if (outcome == "heart attack") {
                df_outcome <- df_outcome[, c(2, 7, 11)]
                names(df_outcome) <- c("Hospital.Name", "State", "heart.attack")
                df_outcome <- df_outcome[order(df_outcome$heart.attack), ]
        } else if (outcome == "heart failure") {
                df_outcome <- df_outcome[, c(2, 7, 17)]
                names(df_outcome) <- c("Hospital.Name", "State", "heart.failure")
                df_outcome <- df_outcome[order(df_outcome$heart.failure), ]
        } else {
                df_outcome <- df_outcome[, c(2, 7, 23)]
                names(df_outcome) <- c("Hospital.Name", "State", "pneumonia")
                df_outcome <- df_outcome[order(df_outcome$pneumonia), ]
        }
        
        df_outcome <- df_outcome[complete.cases(df_outcome[, 3]), ]
        if (num == "best"){
                num <- 1
        } else if (num == "worst") {
                num <- dim(df_outcome)[1]
        } else {
                num <- num
        }
        print(df_outcome[num, 1])
}