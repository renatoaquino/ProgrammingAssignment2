## Programming Assignment 2
## The following functions are helpers to cache an inverse of a matrix.
## To use the function cacheSolve() you need to first wrap a matrix with the 
## function makeCacheMatrix(). Afeter that, everytime you make a change to the
## matrix you need to reassign it to the wrapper.
## Example:
## mx <- matrix(c(1,2,3,4),nrow=2,ncol=2)
## cache_mx <- makeCacheMatrix(mx)
## inverse_mx <- cacheSolve(cache_mx)
## ... do something with the inverse matrix...
## mx[1,2] <- 10
## cache_mx$set(mx)
## ... grab the new inverse ...
## inverse_mx <- cacheSolve(cache_mx)


## This function wrapps a matrix so it could be used with the 
## cacheSolve function.
## The supplied matrix should be invertible or the cacheSolve function
## would not be able to perform the invertion.
##
## This function makes a special "matrix" which is really a list with functions to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)      
}

## Calculates the inverse of a special matrix created by the makeCacheMatrix() function.
## However, it first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse in the cache via the setinverse function.
## Assumes that the matrix supplied is always invertible.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
}
