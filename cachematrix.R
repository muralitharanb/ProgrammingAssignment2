## Below two functions can be used to cache & retrieve the Inverse of Martrix
## Test/Execution Steps
## Step1 : x <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2,ncol=2))
## Step2 : cacheSolve(x) -> This step compute the inverse
## Step3 : cacheSolve(x) -> This step retrieve from cache
## Assumption: Argument matrix is always invertible


# This function creates a matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    imat <- NULL
    set <- function(y) {
        x <<- y
        imat <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) 
    imat <<- solve
    getsolve <- function() imat
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix
## If the inverse has already been calculated then cacheSolve retrieves 
## the inverse from the cache.
## If the inverse has already been not calculated then cacheSolve compute 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    imat <- x$getsolve()
    if(!is.null(imat)) {
        message("Retrieving Inverse of x from Cache")
        return(imat)
    }
    message("Computing Inverse of x")
    data <- x$get()
    imat <- solve(data, ...)
    x$setsolve(imat)
    imat
}
