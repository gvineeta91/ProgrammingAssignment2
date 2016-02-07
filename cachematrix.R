## Matrix inversion is usually a costly computation
## Instead of computing it repeatedly, there is some benefit to caching it
## The following two functions create a matrix and cache the inverse of it

## Creates a matrix object that caches its own inverse
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<- NULL
  }
  get<-function() x
  setinverse<-function(invse) inv<<-invse
  getinverse<-function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve returns the inverse of the matrix created by the function:makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv<-x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix<-x$get()
  inv<-solve(matrix, ...)
  x$setInverse(inv)
  inv
}
