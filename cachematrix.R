## Data structure which contains a matrix and also can have its inverse set,
## and a function which can operate over this data structure
## test in tests/test-cachematrix.R

## Create a cacheMatrix closure which can cache a matrix and its inverse
makeCacheMatrix <- function(x = matrix()) {
  #the cached inverse
  i <- NULL
  
  #set the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  
  #get the matrix
  get <- function() x
  
  #set the inverse
  setinverse <- function(inverse) i <<- inverse
  
  #get the inverse
  getinverse <- function() i
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


#returns the inverse of the cacheMatrix x, returning the cached value
#if it exists, otherwise calculates and sets it first
#this returns an actual R matrix, not a cacheMatrix
cacheSolve <- function(x, ...) {
  
  if(is.null(x$getinverse())) {
    inv <- solve(x$get())
    x$setinverse(inv)
  }

  x$getinverse()
}
