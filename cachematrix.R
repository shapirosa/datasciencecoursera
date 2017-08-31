makeCacheMatrix <- 
  function(x = matrix()) {
## Sets X as a matrix
## Creates parameters for function variables
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
##Sets call functions for cached matrix
  get <- function() x
  setinv <- function(Inverse) Inv <<- Inverse
  getinv <- function() Inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}
cacheSolve <- function(x, ...) {
##Inverse of the MakeCacheMatrix function
##Sets logical function for inverse
  Inv <- x$getinv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
  }
##Sets calls functions for inverse
  data <- x$get()
  Inv <- solve(data, ...)
  x$setinv(Inv)
  Inv
}
