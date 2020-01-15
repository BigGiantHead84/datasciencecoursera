rankall <- function(outcome, num = "best") {
    ## Read outcome data
    ## Check that state and outcome are valid
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
    
    index=0
    measures <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    outcomes <- c("heart attack","heart failure","pneumonia")
    states <- measures$State #subset State variable to its own vector
    uniqueStates <- unique(states) #extract each unique value to its own vector so that we can iterate through each state
    
    #let's first validate that the outcome parameter is in valid ranges and assign colNumber based on that value
    if(is.na(match(c(outcome),outcomes))){ 
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
        
        #Take hospital name, state and column of specified outcome with it
        newdata <- measures[,c(2,7,index)]
        names(newdata)[3] <- "Rate" #Rename column to more usable format
        #create a final ranking dataframe and apply colnames
        ranking <- data.frame("EKS","WY")
        names(ranking) <- c("Hospital.Name","State")
        ranking <- ranking[-c(1),]
        
        ind=0 # declare helper variable which we can use in the for-loop
        
        #loop over states and add result into final dataframe called 'ranking'
        for(state in uniqueStates){
            number=0 #helper-variable to store the number which we are going to use to find the hospital(s) in each state
            
            ind <- newdata[["State"]]==state #get boolen vector (which is true when a value is matching the current state in State variable)
            sortedRatesofState <- sort(newdata[["Rate"]][ind]);  #get all rates of the state and sort them
            #print(sortedRatesofState)
            #Now, we make sure that the num variable passed to the function is valid and then act accordingly
            if(num=="worst"){
                number <- max(sortedRatesofState,na.rm = TRUE)
            }
            else if(num=="best"){
                number <- min(sortedRatesofState,na.rm = TRUE)
            }
            else if(num!="best" & num!="worst"){
                number <- sortedRatesofState[num] #find the value based on the position in the sorted rate result of the state
            }
            
            hospital <- newdata[which(newdata$Rate==number & newdata$State==state),] #get value where the rate is same as selected and state is the current state over which we are looping
            hospital <- hospital[order(hospital$Hospital.Name),] #sort the the outcome the hospital df based on the hospital names
            
            rows <- nrow(hospital) #count number of returned rows of each state so that we can remove duplicate values and add NAs for missing values
            
            #if there's more than one result, pick the first one (as based on alphabetical order)
            if(rows > 1){
                hospital <- hospital[1,]
            }
            #if there's no result, add NA value to Hospital Name variable
            if(rows == 0){
                hospital <- c(NA,state)
            }
            
            #add the result of the current state to final results dataframe
            ranking <- rbind(ranking,hospital[1:2])
        }
        #order the final result based on state
        ranking <- ranking[order(ranking$State),]
        print("printing ranking")
        return(ranking)
    }
}