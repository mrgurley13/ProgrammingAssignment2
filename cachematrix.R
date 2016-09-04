## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x=matrix()) {                                       ##Creates a matrix that will be cached in memory
  m<- NULL                                                                      ##Clears all values in the matrix                   
  set <- function(y){                                                           ##Sets the values of the matrix
      x <<- y
      m <<- NULL
      }
  get <- function() x                                                           ##Calls the values of the matrix
  setinverse <- function(solve) m <<- solve                                     ##Solves and sets inverse values of the matrix
  getinverse <- function() m                                                    ##Calls the values of inverse calculation
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()                                                           ##Calls and assigns inverse values in a matrix
  if(!is.null(m)){                                                              ##Checks to see if data was cached
    message("getting cached data")
    return(m)                                                                   
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)                                                               ##Fills in the matrix with the cached information
  m                                                                             ##Prints data onscreen
}
a <- diag(10, 4)                                                                ##Gives starting values for the function
CachedMatrix <- makeCacheMatrix(a)
cacheSolve(CachedMatrix)
