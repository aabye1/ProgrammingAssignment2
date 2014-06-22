## This set of functions takes in a matrix and then creates a makeCacheMatrix,
## which can be cached, since inverting a matrix is a relatively expensive
## computation. makeCacheMatrix makes the matrix cache-able, and cacheSolve
## returns the inverse of the matrix.

## This function takes an invertible matrix x and returns a list of functions. 
## The functions are get, set, getinv and setinv, which gets and set the 
## matrix and gets and sets inverse of the matrix, respectively

makeCacheMatrix <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinv <- function(inv) m<<- inv
  getinv <- function() m
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes a MakeCacheMatrix element and returns the inverse of
## the matrix used in the makeCacheMatrix. If the calculation has been done
## earlier, the function will instead take the cached version, to save 
## computing costs.

cacheSolve <- function(x){
## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if (! is.null(m)) {
    message("Getting from cache")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m
}