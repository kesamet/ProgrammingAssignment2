## The functions create a special object that stores a numeric matrix
## and caches its inverse.


## The first function, makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinvMat <- function(invMat) m <<- invMat
  getinvMat <- function() m
  list(set = set, get = get,
       setinvMat = setinvMat,
       getinvMat = getinvMat)
}


## The second function calculates the inverse of the matrix 
## created with the above function. However, it first checks 
## to see if the inverse has already been calculated. If so, 
## it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets 
## the value of the inverse in the cache via the setinvMat function.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinvMat()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinvMat(m)
  m
}
