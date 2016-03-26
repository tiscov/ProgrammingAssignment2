## Pair of functions that cache the inverse of a given matrix.


## Function that creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m <<- inv
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## Function that first check if the inverse matrix have already 
## been computed (in which case it retrieves the inverse from
## the cache and return the result), and if not it computes
## the inverse matrix and return the result.

cacheSolve <- function(x, ...) {
      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      else{ data <- x$get()
      m <- solve(data)
      x$setinv(m)
      m}
}
