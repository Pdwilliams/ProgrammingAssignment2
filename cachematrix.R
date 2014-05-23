## this code has two functions. One makes a special 'caching' matrix that will
## cache the result of calling "solve" to inverte the matrix
## the second function tests the "caching" mechanism by calling solve and
## printing a message that tells when the result is coming from the cache
## the second time it's called on the came 'cache matrix' is should use the
## cached result from the first call

## This function takes an ordinary matrix and augments it with behavior in the
## form or functions that get and set the matrix and get
## and set the inverse of the matrix. The "augmented" matrix is in the
## form of a list. This makes the matrix appear to have "state"

makeCacheMatrix <- function(x = matrix()) {
      
      ## initialize m to NULL
      m <- NULL
      
      ## function to set the matrix (an reinitialize m to null so we don't
      ## use the previously cached result)
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      
      ## getter function to return the "internal" matrix
      get <- function() x
      
      ## function to set the inverse (assign to m)
      setinverse <- function(inv) m <<- inv
      
      ## function to get the current inverse value
      getinverse <- function() m
      
      ## return a list that exposes our functions
      ## sort of reminds me of the "revealing module" pattern in Javascript
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
