## function to create a special "matrix" object that can cache its inverse.It shall be Null at the begin. 
## with <<- operator we'll assign a value to an object in other environment different from the current environment.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(solve) m <<- solve
  getmat <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getmat = getmat)
}


## The following function calculates the inverse of the special "matrix" created with the above function.
## When the function was calculated, it can take that inverse from the cache.

cacheSolve <- function(x, ...)  {
  m <- x$getmat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setmat(m)
  m
}