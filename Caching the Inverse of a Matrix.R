makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
  
cachemean <- function(x, ...) {
    m <- x$getmean()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- mean(data, ...)
    x$setmean(m)
    m
  }


## These, makeCacheMatrix and makeCacheMatrix, are modeled after
## makeVector and cachemean.
##
## This function, makeCacheMatrix, creates a special "matrix" objectthat can cache its inverse.  
## Initially "inv" is set to NULL
## It first checks to see if an inverse has already been calculated.
## If so, it gets the values from the cache and does not compute the inverse. 
## If not, it calculates an inverse for the matrix and builds a new matrix in the cache
## using the setInverse function.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates an inverse of the values created by 
## makeCacheMatrix above. If the inverse has already been calculated, and the 
## matrix is unchanged), then it gets the cached data.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
  