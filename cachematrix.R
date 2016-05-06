## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  invMatrix <- NULL
  set <- function(y) {
    x <<- y
    invMatrix <<- NULL
  }
  get <- function() x
  setInverse = function(inv) invMatrix <<- inv 
  getInverse = function() invMatrix
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}

## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get the Inverse from the Environments
  inverse <- x$getInverse()
  
  # Does the invese exist? if true then get the inverse
  if (!is.null(inverse)){
    return(inverse)
  }
  
  # Calculate the Inverse
  inverse <- solve(x$get(), ...)
  
  # Cache the Inverse
  x$setInverse(inverse)
  
  return(inverse)
}

# Test Functions
# Create the Matrix
# r = rnorm(100)
# mat1 = matrix(r, nrow=10, ncol=10)
#
#
# temp = makeCacheMatrix(mat1)
# cacheSolve(temp)
