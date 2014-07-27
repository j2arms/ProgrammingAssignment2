## Caching a matrix and it's inverse
## Functions: 
##    makeCacheMatrix()
##    cacheSolve()
## Purpose:
##    Caches a matrix and it's inverse to recall the inverse instead of calculuting it each time.
## Usage: 
##    First Create a list with makeCacheMatrix. The function makeCacheMatrix expects a matrix as Input
##    Use the previuos created list with CacheSolve for getting the inverse matrix. First time it calculates the matrix, 
##    in subsequent calls it will just recall thze value without need to recalculate the inverse. 



##  Function makeCacheMatrix()
##        Input: A matrix
##        output: A function list for setting/getting the values of the matrix and iverse matrix


makeCacheMatrix <- function(x = matrix()) {

  
  s <- NULL ## initial local variable
  
## initial setting. s is created in the global environment
  set <- function(y) {
    x <<- y
    s <<- NULL 
  }

  get <- function() x ## get matrix
  setinverse <- function(solve) s <<- solve ## set inverse
  getinverse <- function() s ## get inverse
## return list of functions
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

 

## Caches a matrix and returns the inverse
##  Input: List created by makeCacheMatrix
##  Output: The inbverse of the matrix

cacheSolve <- function(x, ...) {
  s <- x$getinverse() ## try to get the inverse of the matrix
  if(!is.null(s)) {   ## if it's returned
      message("getting cached data") ## print message to show that it was precaclulated (some sort of debugging)
      return(s)   ## and return the inverted matrix
  }
  data <- x$get() ## otherwise get the matrix
  s <- solve(data, ...) ## create the inverse
  x$setinverse(s)  ## cache the inverse so it can be recalled later
  s        ## Return a matrix that is the inverse of 'x'
}
