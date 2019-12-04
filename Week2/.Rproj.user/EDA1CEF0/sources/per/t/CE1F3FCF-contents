complete <- function(directory,id=1:332){
    if(!isValidDirectory(directory)){
        print("Please input correct folder")
    }
    else{
        directory <- paste(directory,"/",sep="")
    }
    
    if(!isValidIdRange(id)){
        print("Please specify a valid range (1:332)")
    }
    else{
        fileNames <- printSelectedFiles(id, directory)
        contents <- readFilestoDF(fileNames)
        newDF <- returnCompleteCases(contents)
        nobs <- calculateNOBSv2(newDF,id)
        return(nobs)
    }
}

#returns a dataframe containing only fully completed cases
returnCompleteCases <- function(df){
    newDF <- df[complete.cases(df),]
    return(newDF)
}

#calculates the number of complete cases by using a subset function for a given dataframe and then using number 
#of rows to calculate the number of CC. NOTE! Requires a dataframe that is cleaned of all missing values
calculateNOBSv2 <- function(df,ids){
    nobs_id <- data.frame(id=numeric(),nobs=numeric())
    for(ide in ids){
        new_data <- subset(df,ID==ide)
        nobs_id <- rbind(nobs_id,data.frame(id=as.numeric(ide),nobs=nrow(new_data)))
    }
    return(nobs_id)
    
}