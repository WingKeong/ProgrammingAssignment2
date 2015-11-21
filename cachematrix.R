## The makeCacheMatrix function creates a matrix object together with a list containing 4 functions
## 1. set - Sets the cache matrix to the function parameter values, and the cache inverse matrix to NULL
## 2. get - Retrieves the cache matrix values
## 3. setinverse - Sets the cache inverse matrix to the function parameter values
## 4. getinverse - Retrives the cache inverse matrix values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(invm) m <<- invm
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the cache matrix set by the makeCacheMatrix function
## It first checks whether the cache inverse matrix (m) is NULL - set either by the makeCacheMatrix function when it was called or by the set function
## If the cache inverse matrix is not NULL, that cache inverse matrix will be returned, otherwise, it  will be calculated (using the solve function) and cache using the setinverse function

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mtx <- x$get()
  m <- solve(mtx, ...)
  x$setinverse(m)
  m
}
