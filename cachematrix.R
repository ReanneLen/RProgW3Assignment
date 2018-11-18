## This is my solution to the R Programming Week 3 Assignment about caching a matrix
## My solutions has two parts: (1) makeCacheMatrix - function that creates a special "matrix" object that can
## cache the matrix's inverse; and (2) cacheSolve - function that computes the inverse of the special "matrix"
## returned by the first function where the inverse of the matrix could be retrieved if the inverse has 
## already been calculated given that the matrix has not changed.

##NOTE: For this assignment, it was assumed that the matrix supplied is always invertible.

## First function which creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  matinv <- NULL
  setmat <- function(y) {
    x <<- y
    matinv <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverse) matinv <<- inverse
  getinv <- function() matinv
  list(setmat = setmat, getmat = getmat,
       setinv = setinv,
       getinv = getinv)
}


## Second function - computes the inverse and retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  matinv <- x$getinv()
  if(!is.null(matinv)) {
    message("For a moment. Getting cached data on the matrix's inverse...")
    return(matinv)
  }
  data <- x$getmat()
  matinv <- solve(data, ...)
  x$setinv(matinv)
  matinv  ## Return a matrix that is the inverse of 'x'
}
