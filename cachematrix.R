## The 'makeCacheMatrix' function is used to create a special matrix
## which is a list of 4 functions that are used to set the value of matrix
## ,get the value, set the inverse input through arguments and get the value
## of the inverse.'cachesolve' function calculates the inverse of the matrix 
## and stores it if not present already, else it gets the cached value of
## the inverse.

## makeCachematrix - creates special matrix with 4 functions. 

makeCacheMatrix <- function(x = matrix()) {
              i <- NULL
              set <- function(y) {
                x <<- y
                i <<- NULL
                }
            get <- function() x
            setinverse <- function(inverse) i <<- inverse
            getinverse <- function() i
            list(set = set, get = get,
                  setinverse = setinverse,
                  getinverse = getinverse)
}


## cacheSolve - calculates the matrix inverse and caches it for next call.

cacheSolve <- function(x, ...) {

        i <- x$getinverse()
        if(!is.null(i)) {
          message("getting cached data")
          return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
