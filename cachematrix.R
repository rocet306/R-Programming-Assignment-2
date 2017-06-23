## Put comments here that give an overall description of what your
## functions do

## This function takes a matrix and returns a special "matrix" object
## that allows users to cache a matrix and its inverse and to retrieve or 
## replace that cached data. (The "matrix" object is really a list of
## functions that can be used for these different actions.)

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function takes the special "matrix" object made by makeCacheMatrix
## and returns the inverse of the given matrix. If that inverse has not 
## already been cached, this function will solve for the inverse, cache it,
## and return it. If the inverse has already been cached, then this function
## simply retrieves and returns it, adding a message that this data is 
## being pulled from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
