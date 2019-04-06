## cacheSolve uses makeCacheMatrix to cache the inverse of the input matrix 
## so thatif called several times, the inverse is only calculated once.

## If called from cacheSolve, makeCacheMatrix caches the inverse of the input matrix
## so that we won't need to invert the matrix again if called another time

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}


## Takes a matrix as input. If the inverse has not already been calculated, then the function
## calculates the inverse matrix and caches it (using the solve function, which gives the 
## inverse of a matrix)
## If the inverse has already been calculated, the cached matrix is returned

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}