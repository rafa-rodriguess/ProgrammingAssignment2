## This function creates a special "matrix" 
## object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
  inversedMatrix <- NULL
  setMatrix <- function(originalMatrix) {
    x <<- originalMatrix
    inversedMatrix <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(passedInversedMartix) m <<- passedInversedMartix
  getInverse <- function() inversedMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## This function computes the inverse of the 
## special "matrix" returned by makeCacheMatrix 
## above. If the inverse has already been 
## calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}
