
## In this example, two functions illustrate how compute intensive results
## can be cached, so that cached results can be reused without repeating
## the computation.

## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  setInverse <- function(i) x_inv <<- i
  getInverse <- function() x_inv
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## This function computes the inverse of the special
## "matrix" returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getInverse()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setInverse(x_inv)
  x_inv
}
