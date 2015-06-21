## Put comments here that give an overall description of what your
## functions do

## The following function create a special object that stores a numeric vector

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


## The following function calculates the inverse of the special "vector" created with the above function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

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
