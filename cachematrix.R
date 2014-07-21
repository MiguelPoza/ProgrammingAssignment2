## These two functions, used together, allow us to calculate the inverse
## of a matrix and store it in the cache. Thus, the inverse calculation is 
## only done once. 

## The function makeCacheMatrix creates an object with the original matrix
## and other functions to cache the inverse and get it.
## It receives a matrix and returns a list

makeCacheMatrix <- function(x = matrix()) {
     inverse <- NULL
     set <- function(y) {
          x <<- y
          inverse <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse_matrix) inverse <<- inverse_matrix
     getinverse <- function() inverse
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## The function cacheSolve receive a matrix and computes its inverse or
## if the inverse of that matrix has already been calculated, the function
## gets the inverse from the cache
## It receives a matrix and returns the inverse of that matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     inverse <- x$getinverse()
     if(!is.null(inverse)) {
          message("getting cached data")
          return(inverse)
     }
     data <- x$get()
     inverse <- solve(data, ...)
     x$setinverse(inverse)
     inverse
}

