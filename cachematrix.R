## These two functions compute and cache the inverse of a square matrix

## This function creates a matrix object that can cache it's inverse
## Includes setter and getter functions for both the original matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <- NULL
  getMatrix <- function () x
  setMatrix <- function (y){
    x <<- y
    inverseMatrix <<- NULL
  }
  getInverse <- function() inverseMatrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  list(getMatrix = getMatrix, setMatrix = setMatrix, getInverse = getInverse, setInverse = setInverse)
}


## Calculates the inverse of a matrix if it isn't already cached
## Reads the cached inverse matrix if it is already cached

cacheSolve <- function(x, ...) {      
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    return(inverseMatrix)
  }
  matrix_x <- x$getMatrix()
  inverseMatrix <- solve(matrix_x)
  x$setInverse(inverseMatrix)
  return (inverseMatrix)
}

