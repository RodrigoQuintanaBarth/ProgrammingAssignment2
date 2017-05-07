## Both functions are meant to be used together. makeCacheMatrix() creates a "special matrix" object, with four methods
## to get and set "raw invertible matrices" and to invert them and then retrieve that value. CacheSolve() manipulates
## that type of object, retreving the inverse of a "special matrix" passed to it, if it already was computed, and computing it
## if it wasn't already.

## This whole process' main goal is to use computing power more efficiently, taking advantage of pre-computed values. 




##  First function creates a special matrix (that is really a list, internally) that contains two main variables 
## (x=special matrix and i=inverted special matrix) and four methods to process them. Get the value of x, set a new one,
## set the value of the inverted matrix and get it.
## The matrix passed to it is assumed to be invertible, therefore
## no methods are introduced to handle non-invertible matrices
makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL 
  set <- function(y) { 
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## This function inverts a "special matrix" passed to it, but before doing it from scratch, checks to see if it's
## already stored in cache. If it is, then it retrieves it, if not, it computes it.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i     ## Return a matrix that is the inverse of 'x'
}
