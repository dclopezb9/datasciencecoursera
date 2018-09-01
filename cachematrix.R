## Welcome to my Programming Assignment 2: Lexical Scoping
##I am Diana López from Bogotá, Colombia


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setm <- function(y) {
    x <<- y

    m <<- NULL
  }
  getm <- function() x
  getm
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(setm = setm, getm = getm,
       setsolve = setsolve,
       getsolve = getsolve)
  getsolve
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$getm()
  m <- solve(mat, ...)
  x$setsolve(m)
  m
}