## This set handles creating a matrix and calculating its inverse
## It uses cache to avoid duplicate calculations for the same matrix.

## This function creates a special matrix object
## that can cache its inverse.
## set() to set the matrix which will be used for calculations
## get() to get the currently set matrix
## getInverse() to get the inverse from cache (NULL if none was set)
## setInverse() to set the inverse input from solve()
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    x <<- newMatrix
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(newInv) inv <<- newInv
  getInverse <- function() inv
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## This function returns the inverse of a matrix from cache
## or calculates it when needed
## cacheSolve() will require the matrix environment created by makeCacheMatrix() as input
cacheSolve <- function(x) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m)
  x$setInverse(inv)
  inv
}
