## Put comments here that give an overall description of what your
## functions do cache the inverse of a matirx

## Write a short comment describing this function creates list set value of Matrix, get value of matirx set inverse of matrix get inverse of  matrix

makeCacheMatrix <- function(x = matrix()) {
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setInverse <- function() inve <<- solve(x)
  getInverse <- function() inve
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## Write a short comment describing this function Calculates the inverse of matrix created in above function if already calculated skips calculation and cache the value 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inve)) 
  {
    message("getting cached data")
    return(inve)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inve)
  inve   ## Return a matrix that is the inverse of 'x'
}
