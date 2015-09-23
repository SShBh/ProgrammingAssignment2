## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initialize the inverse
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x
  ## set the inverse
  setsolve <- function(solve) inverse <<- solve
  
  ##get the inverse
  getsolve <- function() inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## This function computes the inverse of the special "matrix".
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getsolve()
  
  ## check if it already has been computed and exists in cache. If so return that
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  ## if inverse does not exist in cache. compute it and add to cache.
  data <- x$get()
  inverse <- solve(data, ...)
  x$setsolve(inverse)
  
  ## return inverse
  inverse
}

