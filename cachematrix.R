## This script contains two functions, makeCacheMatrix, which creates a
## special "cache matrix" that can be used with the second function,
## cacheSolve, to find the inverse of the matrix.
## An example of usage:
## x <- matrix(runif(9), nrow=3, ncol=3)
## cm <- cacheMatrix(x)
## im <- cacheSolve(cm)
## im is the inverse of x, i.e., all.equal(x%*%im, diag(c(1,1,1)))
## returns TRUE.
## The attraction of using cacheSolve over R's built-in "solve" to find the inverse is that
## if the inverse has already been calculated, it is "cached" - if it is
## requested again, then the stored version is returned without performing
## the calculation again.
## NOTE: it is assumed that the matrix is invertible (i.e. of full rank)
##   no checks are performed!


## makeCacheMatrix is called with an (invertible) matrix.  It returns a list
## of functions that can be used with cacheSolve to find the inverse of the
## matrix.
## Notes:
## 1. It is assumed the matrix is invertible - no checking is performed!
## 2. While using with cacheSolve doesn't require the user to explicitly
##    use the functions in the returned list, the user may:
##    (a) use "set" to change the underlying matrix
##    (b) use "get" to return the underlying matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function()
    x
  set_inv <- function(the_inv)
    inv_mat <<- the_inv
  get_inv <- function()
    inv_mat
  list(
    set = set,
    get = get,
    set_inv = set_inv,
    get_inv = get_inv
  )
}

## cacheSolve must be called with a "cache matrix" (actually a list) as created
## by makeCacheMatrix.  It returns the inverse of the matrix represented by
## the cache matrix.  e.g. if x is an invertible matrix and the following
## code is entered:
## cm <- makeCacheMatrix(x)
## im <- cacheSolve(cm)
## then im is the inverse of x.
## The first time cacheSolve is called with a cache matrix, R's built-in "solve"
## function is used.  If the function is called on the same cache matrix then
## the cached inverse is used - i.e., it does not get calculated again.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_mat <- x$get_inv()
  if (!is.null(inv_mat)) {
    message("getting cached inverse")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data)
  x$set_inv(inv_mat)
  inv_mat
}

