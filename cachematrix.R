## 12-08-2016
## Programming Assignment 2: Lexical Scoping
## makeCacheMatrix sets inverse matrix (invX) to Null, return function to
## set matrix, get matrix, set inverse matrix, and get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(y) {
    x <<- y
    invX <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) invX <<- inverse
  getInverse <- function() invX
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve gets inverse matrix from makeCacheMatrix.
## If inverse matrix is not null, it will get inverse matrix from cahce.
## Otherwise compute inverse matrix and cache it.
## Note: x is a square invertible matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return(xInv)
  }
  m <- x$get()
  ##if inverse matrix is null, compute inverse matrix and set inverse in cache
  mInv <- solve(m)
  x$setInverse(mInv)
  mInv
}
