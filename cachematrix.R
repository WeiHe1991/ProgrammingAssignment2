## cachematrix.R
## A pair of functions that cache the inverse of a matrix.
## Author: Wei He
## Date: 2018-03-18

## A factory function that creates a list of functions that handle a matrix object with cache functionality
makeCacheMatrix <- function(x = matrix()) {
  cache <- NULL
  x
  set <- function(y) {
    if ( !all(y == x) ) {
      print(x)
      message("Flushing cache")
      x <<- y
      cache <<- NULL
    }
  }
  get <- function() x
  setCache <- function(newCache) cache <<- newCache
  getCache <- function() cache
  list(set = set, get = get,
       setCache = setCache,
       getCache = getCache)
}


## Solver that computes the inverse of the special "matrix" returned by makeCacheMatrix 
cacheSolve <- function(x) {
  cache <- x$getCache()
  if ( !is.null(cache) ) {
    message("Using cached value")
    return(cache)
  }
  message("Computing the inverse")
  mat <- x$get()
  inv = solve(mat)
  x$setCache(inv)
  inv
}
