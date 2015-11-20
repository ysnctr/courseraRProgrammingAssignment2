@@ -0,0 +1,41 @@
##
##Caching the Inverse of a Matrix
## The function makeCacheMatrix makeCacheMatrix creates a special "matrix" ,
## object that can cache its inverse.
##
## The function cacheSolve computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been
## calculated (and the matrix has not changed), then cacheSolve should
## retrieve the inverse from the cache.
## Student : Yasin Cotur

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Computing the inverse of a square matrix can be done with the solve
## function in R. For example, if X is a square invertible matrix, then solve(X) returns its inverse.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
} 
