## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix takes a matrix and returns a list of 4 functions 
## which are used to stored de inverted matrix in a variable m which may
## have two different values in two nested environments. If the original
## matrix has changed, m is set to null in the parent environment.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
  }
  get <- function() {x}
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
## This function takes the list given by the above function,
## check if the inverted matrix has been already calculated
## and, if so, returns the value stored in the variable m 
## which is the inverted matrix.  It also informes it has used
## the cache.  In any other case, calculates
## and returns the inverted matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
