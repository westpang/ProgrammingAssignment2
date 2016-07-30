## Matrix inversion is a costly computation and 
## there are benefit to cache the inverse of a 
## matrix rather to compute it repeatedly
## Below are two functions that cache the inverse
## a matrix and return the matrix inversion

## This function creates a special "matrix" object 
## that can cache its inverse.
## Consist of 4 functions
## set: set the value of the matrix
## get: get the value of the matrix
## setInv: set the value of the inverse matrix
## getInv: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

  ##reset inverse matrix
  cachedInverseMatrix <- NULL
  
  ##Function 1: Set the value of new matrix to the cache & set the cached of inverse Matrix to null
  set <- function(newMatrix) {
    x <<- newMatrix
    cachedInverseMatrix <<- NULL
  }
  
  ## Function 2: get the value of current matrix that stored in cached
  get <- function() x
  
  ## Function 3: Store the inverse matrix of the current matrix to the cached
  setInv <- function(invMatrix) cachedInverseMatrix <<- invMatrix
  
  ## Function 4: Get the value of inverse matrix
  getInv <- function() cachedInverseMatrix
  
  ##Update the list
  list(set=set, get=get, setInv=setInv, getInv=getInv)
  
}


## This function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix above. If the 
## inverse has already been calculated (and the matrix 
## has not changed), then the cachesolve should 
## get the inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  ##Get the inverse matrix value from the cache
  inv <- x$getInv()
  
  ##If there is an inverse matrix in the cache, return the cached inverse matrix
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ##If there is no inverse matrix in the cache, then calculate the inverse matrix
  
  m <- x$get() ##get the matrix value in cached
  inv <- solve(m) ##calculate inverse matrix of the matrix value
  x$setInv(inv) ##Use setinverse() function to cache the new inverse matrix value
  message("new cached data")
  inv ##return the new inverse matrix
}
