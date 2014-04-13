## This script provides some utils to memoize the inversion of a matrix

## This function is responsible to hold the reference to the current
## matrix and to the potentially (hence lazily memoized) computed inverse

makeCacheMatrix <- function(x = matrix()) {
  .inv <- NULL
  set <- function(y) {
    x <<- y
    .inv <<- NULL
  }
  get <- function() x
  set.inv <- function(inv) .inv <<- inv
  get.inv <- function() .inv
  list(set = set, get = get,
       set.inv = set.inv,
       get.inv = get.inv)
}


## This function is actually performing the lazy memoization by checking
## if there is a memoized value in x (begin a list returned by makeCacheMatrix)
## If none (NULL), the inverse of x is computed and memoized in x.
cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  if(is.null(inv)) {
    data <- x$get()
    inv <- solve(data, ...)
    x$set.inv(inv)
  } else {
    message("getting cached inverse matrix")
  }
  inv
}
