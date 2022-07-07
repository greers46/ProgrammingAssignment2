## The following pair of functions are used to create an object to store a matrix and caches its inverse.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set = function(y) {
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse
  getinv = function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv= getinv)
  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv = x$getmean()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv = solve(mat.data, ...)
  x$setmean(inv)
  return(inv)}

