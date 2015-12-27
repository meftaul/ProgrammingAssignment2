# makeCacheMatrix and cacheSolve - attempt to find the inverse of a given matrix and cache the solution
# makeCacheMatrix stores a list of functions: 
# 'set' stores a matrix
# 'get' retrieves matrix
# 'setinvmatrix' sets the solution for the matrix and informs cacheSolve to
#  retrieve this matrix from cache.
# 'getinvmatrix' retrieves the matrix cached by setinvmatrix.
# 
# Subset the object containing makeCacheMatrix with 'set' to store a matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


# CacheSolve either 1) Returns the cached inverse matrix set by setinvmatrix
# or (else) 2) calculates the inverse matrix with a solve function and 
# passes the argument to setinvmatrix (to cache the solution).

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    
    ## Return a matrix that is the inverse of 'x'
    x$setinverse(m)
    m
}
