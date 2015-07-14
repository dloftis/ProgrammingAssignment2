## Assignmet 2 - R Programming
## Below are functions that create a cacheMatrix.  The cacheMatrix makes inversion more effience by
## caching the inverse of the matrix so users do not have to calculate the inverse matrix multiple 
## times.

## This function takes a matrix 'x' and provides get / set functions used to store and get the cached
## inverse of 'x'
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ## $set stores the original Matrix value and clears the cached invers
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  ## Returns the original matrix
  get <- function() x
  
  ## Caches the inverse of the matrix
  setinverse <- function(inverse) inv <<- inverse
  
  ## Returns the cached value of the matrix inverse.  Returns NULL if no cached value exists.
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function takes a value x.  It solves for the inverse and cacnes the value.
##  If the value is already solved, it returns the cached value.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  ## Gets the cached inverse of the matrix.  NULL if no cached value exists
  inv <- x$getinverse()
  
  ## If the cached value is not null, return it.  Do not calculate inverse again.
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  ## If cached value is NULL, call solve(matrix) to determine the inverse
  data <- x$get()
  inv <- solve(data)
  
  ## Cache the value of the inverse
  x$setinverse(inv)
  
  ## Return the inverse
  inv
}
