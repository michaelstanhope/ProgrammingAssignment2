## makeCacheMatrix()
##
## Returns a matrix object that can be used with the cacheSolve() function
## The object (list) contains the following:
##  x               : The matrix values
##  inv             : The inverse matrix values
##  set()           : Function to set the matrix values
##  get()           : Function to get the matrix values
##  setInverse()    : Function to set the inverse matrix values
##  getInverse()    : Function to get the inverse matrix values
## The inv matrix is accessed and set by both makeCacheMatrix() and cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             ## Upon creation, set the inverse to NULL
    set <- function(y) {    
        x <<- y             ## Set the (global) matrix values
        inv <<- NULL        ## When matrix values are set, NULL the (global) inverse
    }
    get <- function() x     ## Return the matrix values
    setInverse <- function(inverse) inv <<- inverse ## Set the (global) inv matrix values
    getInverse <- function() inv ## Return the matrix values
    list(set = set, get = get,   ## Output the list of functions 
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve()
##
## Returns the inverse of a matrix created by makeCacheMatrix()
## Once the inverse is calculated, the results are cached
## If the matrix is unchanged, future results are retreived from the cache
## If the matrix is changed, results are recalculated and cached again

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()   ## Get the cached results
    if(!is.null(inv)) {     ## If cached results are available, return them
        message("getting cached data")
        return(inv)
    }
    data <- x$get()         ## If there are no cached results, get the matrix...
    inv <- solve(data, ...) ## ... find the inverse...
    x$setInverse(inv)       ## ... cache the results ...
    inv                     ## ... and return the results
}