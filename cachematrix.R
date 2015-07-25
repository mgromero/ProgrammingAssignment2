## Functions for caching the inverse of a matrix in order to save time avoiding 
## recalculation

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a special "vector", which is really a list containing
## functions for getting and setting the matrix and the inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
    inv <<- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <<- function(solve) inv <<- solve
    getinverse <<- function() inv
    list(set = set, get=get, 
         setinverse = setinverse, 
         getinverse = getinverse)
}

## cacheSolve returns the inverse of a matrix, which is calculated with the help
## of the functions contained in the special "vector" returned by
## makeCacheMatrix. It first checks if the mean has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## 
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
