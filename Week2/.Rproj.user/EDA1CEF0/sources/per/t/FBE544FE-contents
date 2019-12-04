corr <- function(directory, treshold=0){
    if(!isValidDirectory(directory)){
        print("Please specify a corrent directory")
    }else{
        directory <- paste(directory,"/",sep="")
    }
    fileNames <- printSelectedFiles(1:332, directory)
    contents <- readFilestoDF(fileNames)
    newDF <- returnCompleteCases(contents)
    nobs <- calculateNOBSv2(newDF,1:332)
    onlyAboveTreshold <- returnAboveTreshold(nobs,treshold)
    result <- calculateCorr(newDF,onlyAboveTreshold)
    return(result)
}

returnAboveTreshold <- function(df,treshold){
    newDF <- subset(df,nobs>treshold)
    return(newDF)
}
calculateCorr <- function(corrDF, tresholdDF){
    ids <- tresholdDF$id #let's take out the id values in id column and save them in their own variable
    eachIdCor <- data.frame()
    for(idi in ids){
        indIdCor <- subset(corrDF,ID==idi)
        eachIdCor <- rbind(eachIdCor,c((cor(indIdCor[,2],indIdCor[,3]))))
    }
    print(head(eachIdCor))
    print(class(eachIdCor))
    return(eachIdCor)
}