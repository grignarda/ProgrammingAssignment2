## makeCacheMatrix create a matrix and cache its reverse
## cacheSolve computes the inverse if nor already in cache

## This function creates a list of functions for set, get, set inverse and get inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## This function inverse the matrix 'x' only if not already in 'makeCacheMatrix'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'  
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
