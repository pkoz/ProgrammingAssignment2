## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  som <- NULL
  set <- function(y) {
    x <<- y
    som <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) som <<- solve
  getsolve <- function() som
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  som <- x$getsolve()
  if(!is.null(som)) {
    message("getting cached matrix")
    return(som)
  }
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  sol
}
