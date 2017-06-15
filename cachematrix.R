## The function cachematrix can make a take a user-defined square matrix and calculate the 
## of the square matrix. It can also take the user-defined inverse matrix and return the 
## the same value.

## The first function, makeCacheMatrix creates a special square "matrix", 
## which is really a list containing a function to

## set the value of the square matrix
## get the value of the square matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## The following function calculates the inverse of the square "matrix" created with the above 
## function. However, it first checks to see if the inverse matrix has already been calculated. 
##If so, it gets the inverse matrix from the cache and skips the computation. Otherwise, it 
## will repeat the calculation and sets the value of the inverse matrix in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data,...)
  x$setinverse(m)
  m
}
