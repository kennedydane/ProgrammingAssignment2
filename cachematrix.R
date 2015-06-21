## This set of functions is used to create a matrix with a cached value for
## the inverse. The first function (makeCacheMatrix) is used to initialise the
## matrix, as well as to gets its value. If the inverse is wanted then the
## "cacheSolve" function is called with the matrix created by the first function
## as its only parameter. This will return the inverse either by calculating it
## or if it has been called before, by returning the cached value.

## This creates a new matrix variable which can potentially cache time-consuming
## operations -- in this case the inverse of the matrix. It makes use of the
## "<<-" assignment operator inside of sub-functions so that the parent environment's
## variable is set (in this case the main function itself).
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    ## important to note that the inverse is reset to NULL in the event
    ## that a new value for x is assigned.
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() return(inverse)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function is used to find the inverse of the matrix created by the above
## function. If the inverse has already been calculated then it is returned,
## otherwise it is calculated and saved.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  return(inv)
}