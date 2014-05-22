## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.
## Will follow the pattern of the example makeVector funtion
## Note to self: try to understand this syntax as a class definition
## PS: should also return a list of functions, not a matrix.
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## inverse of matrix x
  
    set <- function(y) {  ## "constructor"
        x <<- y
        i <<- NULL
    }
    get <- function() x ## return x
    
    setinv <- function(solve) i <<- solve ## save inverse of x
    getinv <- function() i ## return inverse of x
    
    # return the list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)  
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## in which x is a special "matrix" object returned by makeCacheMatrix
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
    }
    
    ## if i == NULL, need to calc i, store into x, and return i
    xm <- x$get()
    i <- solve(xm,...)
    x$setinv(i)
    i
}
