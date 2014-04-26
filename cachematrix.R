## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix as an arg, and sets or gets the inverse of
## that matrix.

makeCacheMatrix <- function(x = matrix(nrow = 2, ncol = 2)) {
  m <- NULL
  set <- function(y) {
     x <<- y
     m <<- NULL
  }

  get <- function() x
  setInverse <- function(solve) m <<- solve(x)
  getInverse <- function() m
  list(set = set, get = get,
     setInverse = setInverse,
     getInverse = getInverse)
}

## cacheSolve will cache the results of the inversion for a given matrix

cacheSolve <- function(x, ...) {
   m <- x$getInverse()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setInverse(m)
   m
   ## Return a matrix that is the inverse of 'x'
}
