library(matlib)

## Below are two function that allow to compute the inverse of a matrix in an efficient manner
## Please use as follows:
## source("path_to_this_r_script/cachematrix.R")

##        A <- matrix( c(5, 1, 0,
##        3,-1, 2,
##        4, 0,-1), nrow=3, byrow=TRUE)

##        mymatrix <- makeCacheMatrix(A)

##        cacheSolve(mymatrix)
##        cacheSolve(mymatrix) # repeat the execution in order to verify that the cached result is returned.




## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set =set, get = get, setinv = setinv, getinv = getinv)
}


## A function that computes the inverse of a given matrix (created via makeCacheMatrix) by caching previously computed values
## cacheSolve assumes that it will receive an invertible matrix as the first parameter.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
