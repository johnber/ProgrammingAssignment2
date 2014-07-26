# Matrix inversion is usually a costly computation 
# and their may be some benefit to caching the inverse 
# of a matrix rather than compute it repeatedly 

# These function provide a way to cache the inverse of a square matrix 

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

  # initialize the cached value to NULL
  cachedInverse <- NULL
  
  # set a new value for the matrix
  set <- function(y) {
    x <<- y
    cachedInverse <<- NULL
  }
  
  # get the matrix value
  get <- function() x
  
  # set the value of the cached inverse
  setCachedInverse <- function(value) cachedInverse <<- value
  
  # get the value to the cached inverse
  getCachedInverse <- function() cachedInverse
  
  # define function mappings
  list(set = set, get = get,
       setCachedInverse = setCachedInverse,
       getCachedInverse = getCachedInverse)
  
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache
cacheSolve <- function(x, ...) {
  
  # initialize the return value to NULL
  v <- NULL
  
  # try to get the cached value
  cachedvalue <- x$getCachedInverse()
  
  # check if there is a cached value
  if(!is.null(cachedvalue)) {

    # return the cached value
    return(cachedvalue)
  }
  
  # Get the matrix to solve
  m <- x$get()
  
  # compute the inverse
  v <- try(solve(m))
  
  # Check if the result it valid
  if(is.null(v)){
    message("Error solving Matrix")
    return(NULL)
  }
    
  # save the computed value to the cache
  x$setCachedInverse(v)
  
  # Return a matrix that is the inverse of 'x'
  return(v)
}
