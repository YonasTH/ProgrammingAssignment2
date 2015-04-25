## This code is able to cache matrix inversion computaions and
## shortens the computaion time

## The function, makeCacheMatrix, creates a list of four functions that
## set the value of the matrix, get the value of the matrix, set the value of the inverse matrix
## and get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## The function cacheSolve computes inverse of the matrix returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve 
## retrieves the inverse from the cache.

##input to cacheSolve is the object where makeCacheMatrix is stored
##for example cacheSolve(a) where a <- makeCacheMatrix(matrix(1:4,2,2))
## a is a list of four functions which can be subset as a$get, a$set, a$getinverse and a$setinverse
## the four functions operate on the matrix defined with matrix(1:4,2,2)

cacheSolve <- function(x, ...) {
      
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i<- solve(data, ...)
  x$setinverse(i)
  i
}
