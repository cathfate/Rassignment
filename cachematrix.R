makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setInversed = function(inverse) inv <<- inverse 
  getInversed = function() inv
  list(set=set, get=get, setInversed=setInversed, getInversed=getInversed)
}

cacheSolve <- function(x, ...) {
  inv = x$getInversed()
  if (!is.null(inv)){
    message("Getting cached data...")
    return(inv)
  }
  
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  x$setInversed(inv)
  
  return(inv)
}
