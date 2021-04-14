#set - sets value of the matrix
#get -  gets the value of the matrix
#setInverese - sets the inverse of the matrix object
#getInverse - retrieves the inverse value of the matrix object.

## This function creates a matrix object and caches it's inverse.   It creates a list object at the end that has all the cached information needed for recall.

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){
    x <<- y
    j <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) j <<- inverse
  getInverse <- function() j 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}


## This computes the inverse matrix returned by makeCacheMatrix and also checks to see if the inverse matrix is already cached.  If it is it returns the cached matrix.

cacheSolve <- function(x, ...) {
  j <- x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat <- x$get()
  j <- solve(mat,...)
  x$setInverse(j)
  j
}
