## makeCacheMatrix is for making a matrix that can store the inverse of a matrix so that other can use it convenient
## cacheSolve is to get the inverse of a matrix (if it is not stored in cache, then calculate, otherwise we just get data from cache)

## a list of function including get, set, getInv, setInv (i.e. getMatrix, setMatrix, getInverseOfMatrix, setInverseOfMatrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInv <- function(i) inv <<- i
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## getInverse of a special matrix (if its inverse is calculated, just return. otherwise calculate it)

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInv()
    if (!is.null(inv)) {
        message("getting cached matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInv(inv)
    inv    
}
