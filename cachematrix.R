## Put comments here that give an overall description of what your
## functions do

## Creates a wrapper of a matrix to cache computation of reverrse function

makeCacheMatrix <- function(x = matrix()) {
    
    mInv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    get <- function() x
    setInv <- function(inv) mInv <<- inv
    getInv <- function() mInv
    list(set = set, get = get,
        setInv = setInv,
        getInv = getInv)
}


## Calculates inverse matrix, cached value is returned if already calculated

cacheSolve <- function(x, ...) {
    inv <- x$getInv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv
}