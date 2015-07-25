## Solution of programming assingment #2 - creating a matrix type that caches its inverse

## Creates a special matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL # Clearing the cache whenever the matrix changes
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the input matrix x's inverse, or returns the cached inverse immediately if already solved

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  inv <- solve(x$get(), ...)
  x$setInverse(inv)
  inv
}
