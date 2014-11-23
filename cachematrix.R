## The makeCacheMatrix() function creates a special "matrix" 'x',
## and computes it's inverse 'I'.
## The cacheSolve() computes the inverse of the special "matrix" 'x'
## returned by makeCacheMatrix() above.
## If the inverse 'I' has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## This function creates a special "matrix" 'x' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) I <<- solve
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


 
## This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I
}
