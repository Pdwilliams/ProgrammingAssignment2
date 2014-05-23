## Put comments here that give an overall description of what your
## functions do

## This function takes an ordinary matrix and augments it with behavior in the
## form or functions that get and set the matrix and get
## and set the inverse of the matrix. The "augmented" matrix is in the
## form of a list. This makes the matrix appear to have "state"

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinverse <- function(inv) m <<- inv
      getinverse <- function() m
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function takes a special "cache matrix" list created by the 
## makeCacheMatrix function and calculates the inverse of the matrix on the
## first call to "getinverse" and returns the cached inverse for all subsequent
## calls to "getinverse".

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
      ## get the currently stored (cached) inverse
      i <- x$getinverse()
      
      ## check if the inverse is null (i.e. we haven't calculated it yet)
      if(!is.null(i)){
            
            ## the inverse is not null, so we have already calculated it
            ## write a message and return the cached value
            message("getting cached data")
            return(i)
      }
      
      ## if we're here we need to do the solve calculation
      ## get the "internal" or "original" matrix
      data <- x$get()
      
      ## get the inverse using solve
      i <- solve(data)
      
      ## cache that inverse for next time
      x$setinverse(i)
      
      ## return the inverse we calculated
      i
}
