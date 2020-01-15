rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## Return hospital name in that state with the given rank
    ## 30-day death rate
    index=0
    measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomes <- c("heart attack","heart failure","pneumonia")
    states <- measures$State #subset State variable to its own vector
    uniqueStates <- unique(states) #extract each unique value to its own vector so that we can iterate through each state
    stateresult <- match(c(state),uniqueStates)
    if(is.na(stateresult)){
        stop("invalid state")
    } 
    else if(is.na(match(c(outcome),outcomes))){
        stop("invalid outcome")
    } 
    else{
        if(outcome=="heart attack"){
            index <- 11
        }
        else if(outcome=="heart failure"){
            index <- 17
        }
        else if (outcome=="pneumonia"){
            index <- 23
        }
        
        #coerce the measured column to numeric so that it can be evaluated
        measures[,index] <- as.numeric(measures[,index]) 
        
        #return only rows of specified state to a new variable called newdata. 
        #Take hospital name, state and column of specified outcome with it
        newdata <- measures[measures$State==state,c(2,7,index)]
        names(newdata)[3] <- "Rate" #Rename column to more usable format
        newdata <- newdata[complete.cases(newdata[,3]),] #remove NAs from evaluated column
        ordered <- newdata[order(newdata$Rate,newdata$Hospital.Name),]
        
        result=0
        if(num=="worst"){
           result <- ordered[ordered$Rate==max(ordered$Rate),]
        }
        else if(num=="best"){
            result <- ordered[1,]
        }
        else{
            result <- ordered[num,]
        }
        print(result[,1])
    }
}