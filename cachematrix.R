# Library of functions to create a special matrix and to
# calculate its inverse.


# makeCacheMatrix cretes a "special" matrix which is a list of 
# function for getting ant setting the matrix and its inverse.
# functions get() and set() operates on the matrix, getinverse()
# and setinverse() on its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve calculates the inverse of the special matrix x.
# The special matrix has to be created with makeCacheMatrix().
# If the inverse of the matrix was previously calculated, it
# returns it from a cache.
cacheSolve <- function(x) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(x)
    x$setinverse(inv)
    inv
}
