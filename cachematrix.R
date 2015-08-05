## This set of functions allows us to cache the computationally expensive
## function to invert a matrix, it only needs to be executed once per matrix.

## makeCacheMatrix creates a wrapper around the argument x, a matrix, 
## in which getter and setter functions get and set the matrix itself,
## as well as its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    # setter function for the matrix, also resets the inverse
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    # getter function for the matrix
    get <- function() x
        
    # setter function for the inverse
    setInverse <- function(y = matrix()) {
        inverse <<- y
    }
    
    # getter function for the inverse
    getInverse <- function() inverse
    
    # returns a list containing all the functions
    list(set=set, get=get, 
         setInverse=setInverse, 
         getInverse=getInverse)
}


## cacheSolve checks if the inverse of the matrix has already been
## executed, and returns it if this is the case. Else, it computes
## the inverse, saves it inside the matrix wrapper, and returns it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        # check if cached version exists
        if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
        }
        
        # cache does not exist, so compute, save and return the inverse
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}
