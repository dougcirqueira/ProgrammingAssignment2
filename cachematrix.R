
## Matrix inversion is usually a costly computation. These functions work
## for computing and caching the inverse of a matrix, for avoiding redundant
## computations.


## This function creates a special matrix, that 
## can have its inverse cached.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## This function computes the inverse of the special matrix
## if it has not been computed before or the matrix changed, then sets its cache.
## If the inverse has already been computed, it retrieves this result
## from the cache.

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
