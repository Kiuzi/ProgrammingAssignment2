## Following the cashmeanexample, this makeCacheMatrix function is actually a list of four simple functions that are for storing and returning the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y=matrix()) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv)
}


## CacheSolve is the function that either retrieve the inverse of the matric from the 'makeCacheMatrix', if it was stored in that function, or calculate the inverse if the getinv result of the previous function is NUll.

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
        ## Return a matrix that is the inverse of 'x'
}
