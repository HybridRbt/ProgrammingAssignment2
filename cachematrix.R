## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## Will follow the pattern of the example makeVector funtion
## Note to self: try to understand this syntax as a class definition

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## inverse of matrix x
  
    set <- function(y) {  ## "constructor"
        x <<- y
        i <<- NULL
    }
    get <- function() x ## return x
    
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    
    ## since the matrix will always be invertible, thus nrow(x) == ncol(x) 
    ## == nrow(i) == ncol(i) 
    matrix(c(x, i), nrow = nrow(x), ncol = ncol(x) + ncol(x))  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmean()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
}
