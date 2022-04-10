makeCacheMatrix <- function(x = matrix()) {
  invm <- NULL
  set <- function(y) {
    x <<- y
    invm <<- NULL
  }
  get <- function() x
  setinv <- function(inversed) invm <<- inversed
  getinv <- function() invm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

cacheSolve <- function(x) {
  invm <- x$getinv()
  mat <- x$get()
  if(!is.null(invm)) {
    message("getting cached data")
    return(invm)
  }
  invm <- solve(mat)
  x$setinv(invm)
  invm
}