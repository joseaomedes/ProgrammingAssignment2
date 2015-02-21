
## Creates a new matrix type (based on a regular R matrix) and the associated 
## functions required to operate with the object. In particular, 
## to cache the inverse of the R matrix used during the object creation 

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Calculates the inverse matrix of a matrix defined using the "CacheMatrix" 
## object type. If the inverse matrix was already cached, gets the result 
## from cache. If not, calculates the inverse matrix, caches the result
## and returns the calculation

cacheSolve <- function(x, ...) {
  
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinv(m)
  m 
}
