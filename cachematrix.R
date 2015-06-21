## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ##set the value of the vector
  set <- function(y) {
    x   <<- y
    i <<- NULL
  }
  
  ##get the value of the vector
  get <- function() x
  
  #set the value of inverse
  setinverse <- function(inverse) i <<- inverse
  ##get value of inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  ##If the inverse has already been calculated (and the matrix has not changed), 
  ##then the cachesolve should retrieve the inverse from the cache
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()
  ##Computing the inverse of a square matrix
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
