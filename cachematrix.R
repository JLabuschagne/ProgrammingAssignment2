## Create and Cache the inverse of a matrix

## Create matrix object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Computes inverse of the special "matrix" returned by makeCacheMatrix function.  
## If inverse has already been calculated and the matrix has not changed, 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
