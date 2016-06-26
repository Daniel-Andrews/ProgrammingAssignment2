# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The
# following two functions are used to cache the inverse of a matrix.

# makeCacheMatrix creates a list containing a function to
# set and get the value and inverse of a matrix if the cacheSolve
# function cannot find the value already stored in memory.

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invs <<- inverse
  getinverse <- function() invs
  list( 
        set=set, 
        get=get, 
        setinverse=setinverse, 
        getinverse=getinverse
        )
}

# cacheSolve returns the inverse of the matrix. If we have
# already computed this inverse then it retrieves the value from the 
# cache. If this is a new inverse, the value is calculated and stored
# in the cache for future use.

cacheSolve <- function(x, ...) {
  invs <- x$getinverse()
  if(!is.null(invs)) {
    message("getting cached data.")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinverse(invs)
  invs
}
