## Data structure which contains a matrix and also can have its inverse set,
## and a function which can operate over this data structure
## test in tests/test-cachematrix.R

## Create a cacheMatrix closure which can cache a matrix and its inverse
##
## Returns a list of functions which can be called:
## get - gets the internal matrix
## set - sets the internal matrix
## setinverse - set the inverse
## getinverse - get the inverse if one exists, NULL otherwise
##
## @param x constructor which should be a matrix, otherwise defaults to an empty matrix
##
## @examples
## myMatrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## myMatrix$get() returns the equivalent of matrix(1:4, 2, 2)
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


## returns the inverse of the cacheMatrix x, returning the cached value
## if it exists, otherwise calculates and caches it first
## this returns an actual R matrix, not a cacheMatrix
##
## @examples
## cacheSolve(makeCacheMatrix(myMatrix)) returns a result identical to solve(myMatrix)
cacheSolve <- function(x, ...) {
  
  if(is.null(x$getinverse())) {
    inv <- solve(x$get())
    x$setinverse(inv)
  }

  x$getinverse()
}
