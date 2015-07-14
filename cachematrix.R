## Assignmet 2 - R Programming
## Below are functions that create a cacheMatrix.  The cacheMatrix makes inversion more effience by
## caching the inverse of the matrix so users do not have to calculate the inverse matrix multiple 
## times.

## This function takes a matrix 'x' and provides get / set functions used to store and get the cached
## inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a value x.  It solves for the inverse and cacnes the value.
##  If the value is already solved, it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
