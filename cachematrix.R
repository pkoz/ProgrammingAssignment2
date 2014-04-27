
## the function takes the argument (the matrix) and store it internally 
## along with computed result of solve()
makeCacheMatrix <- function(x = matrix()) {
  # initialization of internal result of solve()
  som <- NULL
  # set not only assigns the argument internally but also reset som vaiable
  set <- function(y) {
    x <<- y
    som <<- NULL
  }
  # returns internal data stored 
  get <- function() x
  # setter and getter for som variable
  setsolve <- function(solve) som <<- solve
  getsolve <- function() som
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## the function takes a matrix createed with makeCacheMatrix
## and then checks internal cache for the solve() result
## if the cache is empty, computes it and store in cache
## in any case the result of solve() computation is returned
cacheSolve <- function(x, ...) {
  # check whether solve() has been already computed
  som <- x$getsolve()
  # if yes, just return it
  if(!is.null(som)) {
    message("getting cached matrix")
    return(som)
  }
  # of no, get data, compute solve() and store the result for the future
  data <- x$get()
  sol <- solve(data, ...)
  x$setsolve(sol)
  # after all return computed solve() result
  sol
}
