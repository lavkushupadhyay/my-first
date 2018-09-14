## This function takes a square matrix and converts it to its inverse and cache the inverse of a matrix
## 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv_mat <- NULL
  set <- function(y) {
    x <<- y
    inv_mat <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv_mat <<- inverse
  getInverse <- function() inv_mat
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
  
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
  inv_mat <- x$getInverse()
  if (!is.null(inv_mat)) {
    message("getting cached data")
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setInverse(inv_mat)
  inv_mat
}
