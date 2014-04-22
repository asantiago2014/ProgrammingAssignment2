## Put comments here that give an overall description of what your
## functions do
## Peer Assessment / Programming Assignment 2 / R Programming course

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(matrix = matrix()) {
  ## Return a special "matrix" that is the inverse of 'x', which is really a list containing a function to
  ## 1. set the value of the matrix
  ## 2. set the value of another matrix (is inverse)
  ## 3. get the value of the matrix
  ## 4. get the value of the matrix inverse

  inverse <- NULL

  setmatrix <- function(y) {
    matrix <<- y
    inverse <<- NULL
  }

  setinverse <- function(y) {
    inverse <<- y
  }

  getmatrix <- function () matrix

  getinverse <- function () inverse

  list(setmatrix=setmatrix, setinverse=setinverse,
       getmatrix=getmatrix, getinverse=getinverse)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  matrix <- x$getmatrix()
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  inverse <- solve(matrix)
  x$setmatrix(matrix)
  x$setinverse(inverse)
  inverse
}
