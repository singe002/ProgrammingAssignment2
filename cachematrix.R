## makeCacheMatrix and cacheSolve together iteratively find the inverse
## of a matrix, or provide a message that it isn't invertible

## makeCacheMatrix is a function that makes a special "matrix" that can
# cache its inverse.
# makeCacheMatrix does the following: 1) set the matrix value, 2) get the matrix
# value, 3) set the value of the inverse, 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
             x <<- y
             m <<- NULL
         }
       get <- function() x
       setsolve <- function(solve) m <<- solve
       getsolve <- function() m
       list(set = set, get = get,
            setsolve = setsolve,
            getsolve = getsolve)
}


## cacheSolve computes the inverse of the special "matrix" from makeCacheMatrix.
# If the inverse has been calculated (and the "matrix" hasn't changed), then
# cacheSolve returns the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
