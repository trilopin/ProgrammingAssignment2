# Given a squared matrix return four shortcut functions 
# for cache inverse matrix by  environment operator <<-
#   set: stores matrix
#   get: returns matrix
#   setinv: stores inverse matrix
#   getinv: returns inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inv
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


# Returns inverse matrix with cache support
# Input matrix must be passed to makeCacheMatrix function
# before call cacheSolve.
cacheSolve <- function(x, ...) {
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
