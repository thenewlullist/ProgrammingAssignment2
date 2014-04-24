##These functions assume that the input is always a square invertible matrix

##makeCacheMatrix() takes a matrix input, calls the solve() function on it and outputs a list which sets/gets the value of the matrix object,
##then sets/gets the value of its inverse. 

makeCacheMatrix <- function(x = matrix()) {
      mat <- NULL
      set <- function(y) {
            x <<- y
            mat <<- NULL
      }
      get <- function() x
      setsolve <- function(solve) mat <<- solve
      getsolve <- function() mat
      list(set = set, get = get,
           setsolve = setsolve,
           getsolve = getsolve)
}


##cacheSolve() recieves the object output of makeCacheMatrix() above. If the matrix is able to be inverted, and the matrix hasn't changed,
##then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
      mat <- x$getsolve()
      if(!is.null(mat)) {
            message("getting cached data")
            return(mat)
      }
      data <- x$get()
      mat <- solve(data, ...)
      x$setsolve(mat)
      mat
}
