source("../cachematrix.R")

simpleMatrix1 <- matrix(1:4, 2, 2)
simpleMatrix2 <- matrix(2:5, 2, 2)

context("makeCacheMatrix")

test_that("cacheMatrix returns matrix it was constructed with", {
  expect_equal(simpleMatrix1, makeCacheMatrix(simpleMatrix1)$get())
})

test_that("set overwrites the internal matrix", {
  myMatrix <- makeCacheMatrix(simpleMatrix1)
  myMatrix$set(simpleMatrix2)
  
  expect_equal(simpleMatrix2, myMatrix$get())
})

test_that("getinverse returns null initially", {
  expect_equal(NULL, makeCacheMatrix()$getinverse())
})

test_that("getinverse returns the value of setinverse", {
  myMatrix <- makeCacheMatrix()
  myMatrix$setinverse(simpleMatrix1)
  expect_equal(simpleMatrix1, myMatrix$getinverse())
})

test_that("set nulls inverse", {
  myMatrix <- makeCacheMatrix()
  myMatrix$setinverse(simpleMatrix1)
  myMatrix$set(simpleMatrix2)
  expect_equal(NULL, myMatrix$getinverse())
})


context("cacheSolve")

simpleMatrix1Inverse <- solve(simpleMatrix1)


test_that("cacheSolve returns the inverse of the matrix", {
  expect_equal(simpleMatrix1Inverse, cacheSolve(makeCacheMatrix(simpleMatrix1)))
})

test_that("cacheSolve tries to get the inverse", {
  myMatrix <- makeCacheMatrix(simpleMatrix1)
  
  numCalls <- 0
  myMatrix$getinverse <- function() {
    numCalls <<- numCalls+1
  }
  
  cacheSolve(myMatrix)
  
  expect_true(numCalls > 1)
  
})


test_that("cacheSolve calculates and sets the inverse only if null", {
  myMatrix <- makeCacheMatrix(simpleMatrix1)
  
  numCalls <- 0
  myMatrix$setinverse <- function(i) {
    numCalls <<- numCalls+1
  }
  
  cacheSolve(myMatrix)
  expect_equal(1, numCalls)
  
  myMatrix$getinverse <- function() simpleMatrix1Inverse  
  cacheSolve(myMatrix)
  #even though there were 2 calls of cacheSolve, there was still only 1 call
  expect_equal(1, numCalls)
  
})
