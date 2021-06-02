## The below functions will take a matrix and cache the inverse of this matrix


## The first function will cache the inverse of a matrix x.
## It contains different functions used for the following:
## set : used in case we want to set a new matrix as x
## get : to obtain the matrix x
## setinverse : cache the inverse of the matrix x
## getinverse : returns the value of the cache (inverse of matrix x)

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

  }


## This fuction will check the cache of the first function that stores the inverse of the matrix.
## If the cache already has a value, it will return this value.
## If the cache is empty(NULL), it will compute the inverse of the matrix, return the result and store the inverse in the cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
  
}
