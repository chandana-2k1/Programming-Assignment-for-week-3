##The code had two functions, makeCacheMatrix and cacheSolve
##the above 2 functions are used for caching the inverse of a matrix

## the makeCache matrix is used for setting, getting matrix and also for setting and getting the inverse of the given matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## the following function is used for caching the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(x, ...)
  x$setinv(inv)
  inv
}
