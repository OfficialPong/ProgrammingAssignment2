## A pair of functions that are used for calculating matrix inversion and
## caching results.

## Creates a special vector that returns a list of functions that can be
## called to allow:
## 1) Setting the value of the matrix
## 2) Getting the value of the matrix
## 3) Setting the inverse of the matrix
## 4) Getting the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Solves a matrix inverse calculation when passed in a CacheMatrix list as created by 
## makeCacheMatrix. If the result exists in the CacheMatrix list,
## the cached result is return instead of performing the actual calculation.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
