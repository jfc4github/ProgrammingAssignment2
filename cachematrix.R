##############################################################
## 
## makeCacheMatrix
##  x: a matrix, optional
##
## it defines 4 methods:
##  set: to assign a new matrix
##  get: to get the current matrix
##  setinverse: to set the new inverse matrix
##  getinverse: to get the inverse matrix, already stored
##
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  ## set: it stores a matrix
  ## 
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  ## get: it returns the stored matrix
  ##
  get <- function() x
  
  ## setinverse: it stores a matrix as the inverse one
  ##
  setinverse <- function(inv) inverse <<- inv
  
  ## getinverse: it returns the stored inverse matrix 
  ##
  getinverse <- function() inverse
  
  ## return the list of methods
  ##
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}
##
##############################################################
## 
## cacheSolve: to return the inverse of a matrix
##  x: the matrix
## 
## It verifies if the matrix was already stored before
## If it is identical to the stored version, then the stored inverse
## matrix is returned.
## If it wasn't stored before, or is different, then the inverse
## matrix is calculated, stored and returned.
##
cacheSolve <- function(x, ...) {
  im <- x$getinverse() # try to recover the inverse matrix
  ## if it was stored before
  if(!is.null(im)) { 
    ## message("getting cached data authorized")
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setinverse(im)
  im
}
