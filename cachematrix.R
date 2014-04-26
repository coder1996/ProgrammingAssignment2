## The following functions let you cache a potentially expensive operation
## of calculating the inverse of a matrix using a special matrix
## objet.

## This function accepts a matrix and wraps it into a special object which can
## cache the value of it's inverse. 
## The special matrix has four functions:
## get, set, getinverse, setinverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This is a function that can be invoked instead of "solve" to calculate 
## the inverse of a special matrix. This function will first check if the inverse of 
## a given special matrix has been calculated.
## If yes, then the cached value is returned
## If no, then the inverse is calculated, cached for the next time, and then returned.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
