## The functions below cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse. Is essentially
## a list containing functions to set and get the value of the matrix, and set
## and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix above. If the
## inverse has already been calculated (and the matrix has not changed), then
## retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
