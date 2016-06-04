## This pair of functions cache the inverse of a matrix
## Written by khychan on 4 JUN 2016

## This function creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        # calculate the inverse of the matrix.
        setinverse <- function(solve) m <<- solve
        # get the inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function uses the output of the makeCacheMatrix and computes the inverse. If the matrix and the inverse
## has not changed, cacheSolve retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        # if the inverse has been calculated, it is retrieved.
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # otherwise, calculate and retrieve the inverse.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
}
