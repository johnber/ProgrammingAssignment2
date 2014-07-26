source("TestHelper.R")
source("cachematrix.R")

test_all <- function(){
  verifycached()
  verifymodified()
  verifyNULL()
}

verifycached <- function(){
  
  message("Test: verifycached")
  data <- matrix(runif(16,-1,1), nrow=4)
  m <- makeCacheMatrix(data)
  
  expected <- solve(data)
  
  result <- cacheSolve(m)
  
  verify_vectors(expected, result)
  
  cached_result <- cacheSolve(m)
  verify_vectors(expected, cached_result)
  
}

verifymodified <- function(){
  
  message("Test: verifyModified")
  data1 <- matrix(runif(16,-1,1), nrow=4)
  data2 <- matrix(runif(64,-1,1), nrow=8)
  
  # make a matrix with data1 and solve
  m <- makeCacheMatrix(data1)
  result1 <- cacheSolve(m)
  
  # modify the matrix value and solve
  m$set(data2)
  result2 <- cacheSolve(m)
  
  verify(length(result1) != length(result2))

}

verifyNULL <- function(){
  message("Test: verifyNULL")
  data <- NULL
  m <- makeCacheMatrix(data)
  
  verify_vectors(data, m)  
}