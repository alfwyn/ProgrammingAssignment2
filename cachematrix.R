# These functions receive a matrix and then return 
# its inverse as well as caching the inverse

# The makeCacheMatrix function creates a list of 
# two functions used to set and get the value of 
# a matrix and two functions to set and get the 
# inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list (set = set, get = get, 
        setinv = setinv, 
        getinv = getinv)
}

# The cacheSolve function returns the inverse of a matrix.
# It checks first if the inverse has already been computed.
# If yes, the function retrieves the result from the cache
# and skips to the end of the function. If no, the function
# calculates the inverse and stores it in the cache through 
# the setinv function

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
