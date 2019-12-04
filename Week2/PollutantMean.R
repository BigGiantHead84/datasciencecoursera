pollutantmean <- function (directory, pollutant, id=1:332){
    if(!isValidDirectory(directory)){
        print("Please input correct folder")
    }
    else{
        directory <- paste(directory,"/",sep="")
    }
    selection <- character()
    if(!isValidPollutant(pollutant)){
        print("Please specify a correct pollutant")
    }
    else{
        selection <- pollutant
    }
    
    if(!isValidIdRange(id)){
        print("Please specify a valid range (1:332)")
    }
    else{
        fileNames <- printSelectedFiles(id, directory)
        contents <- readFilestoDF(fileNames)
        contents <- removeMissingValues(contents,selection)
        
        print(paste("Pollutant",selection,"mean is ",calculatePMMean(selection,contents)))
    }
}

#checks that the inserted direcotry is valid one
isValidDirectory <- function(directory){
    isValid <- logical()
    if(grepl("specdata",directory)){
        isValid <- T
    }else{
        isValid <- F
    }
    return(isValid)
}

#Checks that the inserted pollutant character vector is valid 
isValidPollutant <- function(pollutant){
    selection <- logical()
    if(pollutant=="sulfate" || pollutant=="nitrate"){
        selection <- TRUE
    }else{
        selection <- FALSE
    }
    return(selection)
}

#Checks that the inserted ID range is within a permitted range
isValidIdRange <- function(id){
    insider <- ""
    if(length(id)>0 && length(id)<333){
        for (i in length(id)) {
            if(inside.range(id[i],c(1,332)) == TRUE){
                insider <- TRUE   
            }else{
                insider <- FALSE
            }
        }
    }else{
        insider <- FALSE
    }
    #print(paste("isValidRange:",insider))
    return(insider)
}

#reads a list of files into a variable within specified directory and returns the variable
printSelectedFiles <- function(id,directory){
    id <- sort(id)
    files <- character(length = length(id))
    for (i in 1:length(id)) {
        if(id[i]<10){
            files[i] <- paste0(directory,"00",id[i],".csv")
        }else if (id[i]>=10 && id[i]<100){
            files[i] <- paste0(directory,"0",id[i],".csv")
        }
        else if(id[i]>=100){
            files[i] <- paste0(directory,id[i],".csv")
        }
    }
    return(files)
}

#reads the contents of files to a dataframe and returns the dataframe
readFilestoDF <- function(fileNames){
    contents <- data.frame(Date=character(),sulfate=numeric(),nitrate=numeric(),ID=integer(),stringsAsFactors = FALSE)
    for(i in 1:length(fileNames)){
        newDF <- read.csv(fileNames[i],header=T,colClasses = c("character","numeric","numeric","integer"))
        contents <- rbind(contents,newDF)
    }
    return(contents)
}

#removes missing values from dataframe df of selected column and returns dataframe without NAs
removeMissingValues <- function(df,pm){ 
    newDF <- df[complete.cases(df[,pm]),] #select only specific column 
    print(newDF)
    return(newDF)
}
#calculates pollutant's mean from cleaned dataframe and returns the mean
calculatePMMean <- function(pm,df){
    pmMean <- mean(df[[pm]])
    return(pmMean)
}



