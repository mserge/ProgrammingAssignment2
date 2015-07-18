## Matrix inversion is usually a costly computation 
## and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(sol) i <<- sol
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" 
##returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

## UNIT Test
## This test function will call  makeCacheMatrix and and then try to call several times
testf <- function(matVal) {
     temp = makeCacheMatrix(matVal)
     print(temp)
     print("Try to solve first time")
     print(cacheSolve(temp))
     print("Load from cache second time")
     cacheSolve(temp)
}

## To test - cases: simple 2x2 and random
## mat <- matrix(1:4, nrow=2, ncol = 2)
## n<- set size of test
## generate large mat
## mat <- matrix(runif(n^2),n)
## testf(matVal = mat)
