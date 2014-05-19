## This file contains two functions, makeCacheMatrix and cacheSolve.
## The first, makeCacheMatrix, takes as an argument a matrix (x) and creates a list
## of four function objects. The second, cacheSolve, takes the list resulting from
## makeCacheMatrix and computes or returns the inverse of x.

## makeCacheMatrix: argument is a matrix object. The function returns a list of
## four functions. They are (1) set: takes a matrix as an argument and assigns it
## to x, (2) get: returns its argument (x) when called, (3) setInverse: takes the
## inverse matrix as an argument and sets it (I), and (4) getInverse: returns I.

makeCacheMatrix <- function(x = matrix()) {
  I <- NULL
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) I <<- inverse
  getInverse <- function() I
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve takes a list created by makeCacheMatrix and pulls the inverse matrix
## using the getInverse() function. If inverse (I) is NOT null, it returns I from
## the cache. Otherwise it gets the original matrix (x) using the get() function,
## computes the inverse with solve() and assigns it to the list using the
## setInverse() function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  I <- x$getInverse()
  if(!is.null(I)) {
    message("getting cached data")
    return(I)
  }
  data <- x$get()
  I <- solve(data, ...)
  x$setInverse(I)
  I
}
