makeCacheMatrix <- function(x = matrix()) {
  Inv1 <- NULL
  set <- function(y) {
    x <<- y
    Inv1 <<- NULL
  }
  get <- function() x
  setinv <- function(Inverse) Inv1 <<- Inverse
  getinv <- function() Inv1
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}
cacheSolve <- function(x, ...) {
  Inv1 <- x$getinv()
  if(!is.null(Inv1)) {
    message("getting cached data")
    return(Inv1)
  }
  data <- x$get()
  Inv1 <- matrix(data, ...)
  x$setinv(Inv1)
  Inv1
}