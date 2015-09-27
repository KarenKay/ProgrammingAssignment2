## These functions invert a matrix and then cache the result.


## This function creates a matrix object that can cache its inverse


makeCacheMatrix <- function(x = matrix()) {
   
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix.  If the 
## inverse has already been calculated and the matrix has not changed, then this function
## will retrieve the inverse from the cache instead of calculating it.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
    
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
