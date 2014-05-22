## These functions allow you to solve the inverse of a square matrix. 
## The data is stored within a cache, so that the computer can 
## retrieve an answer quickly, if it had already been solved earlier.

## The function makeCacheMatrix creates a list of 4 functions, that 
## will then be used within the function cacheSolve.

makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      set <- function(y) {
            x <<- y
            s <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) s <<- solve
      getsolve <- function() s
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)

}


## The function cacheSolve calculates the inverse of a square matrix. 
## It first checks to see if it has not already been calculated. 
## If this is the case, the answer is given from the cache. 

cacheSolve <- function(x, ...) {
      s <- x$getsolve()
      if(!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      data <- x$get()
      s <- solve(data, ...)
      x$setsolve(s)
      s
}
