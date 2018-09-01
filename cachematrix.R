## Welcome to my Programming Assignment 2: Lexical Scoping
##I am Diana López from Bogotá, Colombia


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    n<<-NULL
  }
  get <- function() x
  setsolve <- function(solve) n <<- solve
  getsolve <- function() n
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  getsolve
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  n <- x$getsolve()
  if(!is.null(n)) {
    message("getting cached data")
    return(n)
  }
  mat <- x$get()
  n <- solve(mat, ...)
  x$setsolve(n)
  n
}