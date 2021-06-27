## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

library(MASS)
  makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL  #initializing inverse as NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x #function to get matrix x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function()
      {
      inver <- ginv(x)
      inver%*%x
    }
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
  }


## changed "mean" to "solve" and "m" to "s"

  cacheSolve <- function(x, ...) ##gets chace data
    {
    inv <- x$getinv()
    if(!is.null(inv)) {    #checking whether inverse is Null
      message("getting cached data")
      return(inv) #returns inverse value
    }
    data <- x$get()
    inv <- solve(data, ...)  #calculates inverse value
    x$setinv(inv)
    inv  # Returns a matrix that is inverse of x
  }
