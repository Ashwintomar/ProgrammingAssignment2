## Create a new "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Compute the inverse of the matrix stored in the "matrix" object
cacheSolve <- function(x, ...) {
  ## Return the inverse if it has already been calculated
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## Compute the inverse using solve() function
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
