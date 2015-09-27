## This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatrix is a function that returns a list of functions
## matrix. Contains the following functions:
## setMatrix      set the value of a matrix
## getMatrix      get the value of a matrix
## cacheInverse   get the cahced value (inverse of the matrix)
## getInverse     get the cahced value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) i <<- inv
    getinverse <- function() i
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
    )
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    m <- x$get()
    i <- solve(m, ...)
    x$setinverse(i)
    i
}
