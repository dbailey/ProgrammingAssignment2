## Data structure for holding a matrix value with a cached inverse of that matrix.

## Function to create a CacheMatrix. Accepts an optional initial value for the matrix
## otherwise defaults to matrix()

makeCacheMatrix <- function(m = matrix()) {
  i <- NULL
  set <- function(n) {
    m <<- n
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(j) i <<- j
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Function that takes a CacheMatrix and returns the inverse. This will either use the cached
## inverse value or compute the inverse and cache the value for future use. 

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (is.null(i)) {
    i <- solve(x$get())
    x$setinverse(i)
  }
  i
}
