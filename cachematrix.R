## Creates a list object(which will be referred to as a CacheMatrix), which stores
## a given matrix as well as its inverse once computed

## makeCacheMatrix 
##  Input: Matrix
##  Output: CacheMatrix
##  Description: Creates a CacheMatrix from from the Matrix provided
##  A CacheMatrix is a list object which contains the following functionality: 
##   set(Matrix)
##    Input: Matrix
##    Output: None
##    Description: stores the given matrix
##   get() 
##    Input: None
##    Output: Matrix
##    Description: returns the stored matrix
##   setinv(Matrix)
##    Input: Matrix
##    Output: None
##    Description: stores the inverse of the matrix (should not be used directly, rather use the function cacheSolve)
##   getinv() 
##    Input: None
##    Output: Matrix
##    Description: returns the stored inverse matrix value, or null if it has not been computed
##     *if the given matrix is not invertible, will return an error
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve
##  Input: CacheMatrix
##  Output: Matrix (inverse)
##  Description: If the CacheMatrix provided already has the inverse stored that value is returned
##   otherwise computes the inverse of matrix stored in the CacheMatrix, stores the value in the CacheMatrix and retuns the inverted Matrix
##   *if the matrix stored in the CacheMatrix is not invertible, it will return an error
cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
