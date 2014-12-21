## The following functions allow you to create and manipulate an instance of matrix with it's inverse cached.
## 

## makeCacheMatrix creates a vector of methods to access a matrix instance with cached inverse.

makeCacheMatrix <- function(mat = matrix()) {
  
  inv <- NULL
  
  set <- function(x) {
    mat <<- x
    inv <<- NULL
  }
  get <- function() mat
  
  setinv <- function(x) inv <<- x
  getinv <- function() inv
  
  list(set = set, get = get,
        setinv = setinv, getinv = getinv)
    
}


## cacheSolve function calculates inverse of matrix instance produced by makeCacheMatrix and stores it into cache.
## If cache for the matrix is found calculation is not perforemd and cached data is returned instead.

cacheSolve <- function(mat, ...) {
  
  inv <- mat$getinv()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  data <- mat$get()
  inv <- solve(data, ...)
  mat$setinv(inv)
  
  inv
  
}
