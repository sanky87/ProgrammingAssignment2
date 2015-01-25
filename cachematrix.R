## Put comments here that give an overall description of what your
## functions do
##This function will get the matrix to be inversed and will set the value of m 
## variable to null.This m value when passed to the cacheSolve function will 
##indicated cacheSolve to calculate inverse of matrix.cacheSolve will then set 
#inverse of matrix in variable m. This will ensure that on subsequent calls
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
##This function checks if the inverse of matrix is present
## if the inverse of matrix is present it will return the cached value
##if the new matrix is passed it will inverse the matrix and will set the value 
##in makeCacheMatrix function.This value when set can then be cached in future to 
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
