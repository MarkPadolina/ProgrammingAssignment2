## The function makeCacheMatrix creates a special matrix object
## and the function cacheSolve computes the inverse of the matrix.

## In the function makeCacheMatrix,
## if the inverse of the matrix has already been calculated,
## then this function will find it in the cache and return it,
## instead of calculating it again.
makeCacheMatrix <- function(x = matrix()) {
  inverse_of_x <- NULL
  set <- function(y) {
    x <<- y
    inverse_of_x <<- NULL
  }
  get <- function() x
  setinverse<- function(inverse) inverse_of_x <<- inverse
  getinverse <- function() inverse_of_x
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## If the cached inverse matrix was created in the function makeCacheMatrix,
## then the function cacheSolve retrieves it instead of recomputing the inverse.
## if the cached inverse matrix is not available,
## then then function cacheSolve computes, caches, and returns it.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse_of_x <- x$getinverse()
  if (!is.null(inverse_of_x)) {
    message("getting cached inversed matrix")
    return(inverse_of_x)
  } else {
    inverse_of_x <- solve(x$get())
    x$setinverse(inverse_of_x)
    return(inverse_of_x)
  }
}
