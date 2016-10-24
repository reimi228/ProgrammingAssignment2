## Put comments here that give an overall description of what your
## functions do

# Caching the Inverse of a Matrix:
# Computing inverse of a matrix is usually a costly operation, and  
# there may be a benefit to caching the inverse of a matrix rather
# than computing it repeatedly.
# Functions below creates a special object that 
# stores a matrix and caches its inverse.

## Write a short comment describing this function
# makeCacheMatrix will create a special matrix object that can 
# cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
  
  get <- function() x 
  setinverse <- function(solve) m <<- solve 
  getinverse <- function() m 
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
# cacheSolve commputes the inverse of the special matrix returned 
# by makeCacheMatrix. If the inverse is already computed, 
# it retrives the inverse from teh cache.

cacheSolve <- function(x, ...) {
  
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

