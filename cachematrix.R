## Functions to implement a Matrix whose Inverse is cached.

## Creates an instance of the Cached-Matrix with wrapper functions 
## that can access the data and cached inverse value
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Set function
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Get function
  get <- function()
  {
    x
  }
  
  # Set Inverse function
  setInverse <- function(matrixInverse)
  {
    inverse <<- matrixInverse
  }
  
  # Get Inverse function
  getInverse <- function()
  {
    inverse
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## This function returns the cached inverse value of the input cache-matrix
## if available or computes the same if absent and adds it to the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if (!is.null(inverse)){
    message("Geting Cached Inverse value")
    return(inverse)
  }
  
  
  inputMatrix <- x$get()
  inverse <- solve(inputMatrix, ...)
  x$setInverse(inverse)
  inverse  
}
