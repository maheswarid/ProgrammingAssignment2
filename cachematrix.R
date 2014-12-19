## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  This function creates a list(get,set) of functions to 
##  create a cache matrix. Using the <<- operator makes
##  this function object cached in the environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  # initializes the m to null
  set <- function(y) { # set function which stores the mat inverse
    x <<- y
    m <<- NULL
  }
  get <- function() x   
  setsolve <- function(solve) m <<- solve #stores the mat inv in the cached matix
  getsolve <- function() m
  list(set = set, get = get,  #function list for getting and setting the cached matix
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function
## This function checks if there is a cached matrix inversion already present
## if present returns the cached value or else calculates the inverse matrix
## and returns the value. Input to this function is a list objected generated
## using the makeCacheMatrix function

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
