## These sets of function provide an api to calculate and cache
## an inverse of a given matrix

## This function creates a "special matrix" object that can cache its inverse
## The returned value is a list consists of 4 functions
## 1. set(y) - sets a new matrix y into the field x and reset the inv field
## 2. get() - retrieves the current matrix (saved in field x)
## 3. setinverse(inverse) - sets a new inversed matrix
## 4. getinverse() - retrieves the cached inversed matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)   
}

## This function contains the logic to calculate, store, and retrieve the
## inverse. It gets a "special matrix" object as a parameter and whenever
## and returned the inverse of the matrix it encapsules. If it doesn't have
## a calculated inverse, this function calcultes it and cache it for later.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}