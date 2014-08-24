## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## This first funcion, makeCacheMatrix(), create a special "matrix" that can
## cache its inverse. The second function, cacheSolve(), returns the inverse of
## the special "matrix" by using its cache value, if exits and the matrix has
## not changed, or computes the inverse, otherwise.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function() inverse <<- solve(x)
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
        inverseIsNull <- is.null(x$getinverse())
        message("Is inverse null? ", inverseIsNull)
        if (!inverseIsNull)  {
                message("Getting cached data for inverse...")
                
        } else {
                message("Calculating the inverse...")
                x$setinverse()
        }
        return(x$getinverse())    
}
