##Created for R-Programming Course organized by John Hopkins University via Coursera
##Author: Jukka Hilvonen

## This function first initializes a variable called m and then introduces functions set, get, 
##setInverse and getInverse that handle getting and setting x and its inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(xinverse) m <<- xinverse
    getInverse <- function () m
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## this function tries to first get the cached matrix. If unsuccessful, it calculates inverse 
##using sample() function and then stores the matrix m and returns the matrix m
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()
    ## If m is not null, return m
    if(!is.null(m)){
        message("Getting cached data")
        return(m)
    }
    ##otherwise get the data to data-variable and use solve() to calculate the inverse of the matrix
    data <- x$get()
    m <- solve(data)  %*% data
    x$setInverse(m)
    m
}
