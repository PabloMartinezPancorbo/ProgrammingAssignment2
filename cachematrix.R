## This code creates a special "matrix" object that can cache its inverse 
## by using the function makeCacheMatrix.
## Also, this code computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and not changed), then the cacheSolve should retrieve  the inverse 
## from the cache.

## makeCacheMatrix caches the inverse of a square matrix, see and full example with cacheSolve:
## > source('cachematrix.R')
## > m <- makeCacheMatrix(matrix(c(1, 2, 3, 4, 5, 1, 6, 5, 2), c(3, 3)))
## > cacheSolve(m)
##           [,1]        [,2]       [,3]
## [1,] -0.1724138  0.06896552  0.3448276
## [2,] -0.3793103  0.55172414 -0.2413793
## [3,]  0.4482759 -0.37931034  0.1034483

## It creates a special "matrix", which is a list containing a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invmatrix <- NULL
  set <- function(y) {
    x <<- y
    invmatrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) invmatrix <<- inv
  getinverse <- function() invmatrix
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## cacheSolve calculates the inverse of the special "matrix" created by 
## makeCacheMatrix, checking cached result if it is available and using it.

cacheSolve <- function(x, ...) {
  invmatrix <- x$getinverse()
  if(!is.null(invmatrix)) {
    message("getting cached data")
    return(invmatrix)
  }
  m <- x$get()
  invmatrix <- solve(m, ...)
  x$setinverse(invmatrix)
  invmatrix
}