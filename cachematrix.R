## Caching the Inverse of a Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <- inv
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix". 
##If the inverse has already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("returning cashed inverse of matrix")
    return(inverse)
  }
  temp <- x$get()
  inv <- solve(temp, ...)
  x$setinverse(inv)
  inv
}

## test dataset
##data=matrix(c(2, 1, 5, 2),2,2)