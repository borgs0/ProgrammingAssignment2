## Creates a data structure allowing caching of previous results
## when computing the inverse of a matrix.

## Take a matrix and returns a list of functions that allow 
## caching of the solve() function.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) inverse <<- solve
  getsolve <- function () inverse
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Uses the makeCacheMatrix function to cache previous results
## and intelligently save work.

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setsolve(inverse)
  inverse
}
