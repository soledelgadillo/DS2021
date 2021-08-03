best <- function(state, outcome){
        
        # Read the outcome data
        df_outcome <- read.csv("Data/ProgAssignment3-data/outcome-of-care-measures.csv", stringsAsFactors = FALSE, na.strings = "Not Available")
        
        # Check that state and outcome are valid
        if (!(state %in% df_outcome$State)) stop("invalid state")
        possible_outcomes <- c("heart attack", "heart failure", "pneumonia")
        if (!(outcome %in% possible_outcomes)) stop("invalid outcome")
        
        # Return the hospital name in that state with lowest 30-day death rate
        

        df_outcome <- split(df_outcome, df_outcome$State)
        df_outcome <- df_outcome[[state]]
        df_outcome <- df_outcome[order(df_outcome$Hospital.Name), ]
        

        if (outcome == possible_outcomes[1]) {
                df_outcome <- df_outcome[, c(2, 7, 11)]
                names(df_outcome) <- c("Hospital.Name", "State", "heart.attack")
                df_outcome <- df_outcome[order(df_outcome$heart.attack), ]
        } else if (outcome == possible_outcomes[2]) {
                df_outcome <- df_outcome[, c(2, 7, 17)]
                names(df_outcome) <- c("Hospital.Name", "State", "heart.failure")
                df_outcome <- df_outcome[order(df_outcome$heart.failure), ]
        } else {
                df_outcome <- df_outcome[, c(2, 7, 23)]
                names(df_outcome) <- c("Hospital.Name", "State", "pneumonia")
                df_outcome <- df_outcome[order(df_outcome$pneumonia), ]
        }
        
        df_outcome <- df_outcome[!is.na(df_outcome), ]
        
       
 
       print(df_outcome[1,1])

}