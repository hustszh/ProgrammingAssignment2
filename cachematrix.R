## The calculation of inverse matrix is usually time-consuming,
## cache inverse matrix may be more favorable than the repeated
## calculation. The following two functions are used to cache
## on inverse matrix.


## This function is used to create a special "matrix" object
## to cache an inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    
    ## used to store the inverse matrix
    s <- NULL
    
    ## used to set a new matrix
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    
    ## used to get the stored matrix
    get <- function() x
    
    ## used to set the inverse matrix
    setSolve <- function(solve) s <<- solve
    
    ## used to get the cache inverse matrix
    getSolve <- function() s
    
    ## return a list containing the above functions
    list(set = set,
         get = get,
         setSolve = setSolve,
         getSolve = getSolve)
}


## This function is used to calculate the inverse matrix of
## the special "matrix" returned by the makeCacheMatrix function.
## If you have computed the inverse matrix (and the matrix has not 
## been changed ), then cachesolve will return the inverse matrix
## in the cache.
cacheSolve <- function(x, ...) {
    
    ## try to get the cache inverse matrix
    m <- x$getSolve()
    
    ## if cache data found, return it
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    ## get the matrix data
    data <- x$get()
    
    ## calcuate the inverse matrix
    m <- solve(data, ...)
    
    ## cache the inverse matrix
    x$setSolve(m)
    
    ## return a matrix that is the inverse of 'x'
    m
}