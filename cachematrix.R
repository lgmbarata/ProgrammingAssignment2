## Put comments here that give an overall description of what your
## functions do

## This function creates an object that caches a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inversex) inv <<- inversex
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Calculates the inverse of ˜x˜ (must be an invertible matrix), if the
## inverse matrix is already calculated then output it otherwise execute 
## calculation using SOLVE

cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
