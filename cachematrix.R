# R Programming 
# Coursera, 5/5/2014 Session

# makeCacheMatrix is a list of functions that do the following
# 1.  set the value of a matrix x
# 2.  get the value of a matrix x
# 3.  set the value of the inverse of x 
# 4.  get the value of the inverse of x 

makeCacheMatrix <- function(x = matrix()) {
    inverseOfx <- NULL
    set <- function(y)
    {
      x <<- y
      inverseOfx <<- NULL
    }
    
    get <- function()
    {
      x
    }
    
    setinverse <- function(inverse)
    {
      inverseOfx <<- inverse
    }

    getinverse <- function()
    {
      inverseOfx
    }

    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    
}


## cacheSolve(matrix x)
## Uses funnction list in makeCacheMatrix for get/set operations
## Checks if the input matrix, x, has stored value of its inverse
## if yes, returns that, otherwise computes the inverse of x, stores
## it, and then returns the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverseOfx <- x$getinverse()

  if(!is.null(inverseOfx))
  {
    message("getting cached matrix")
    return(inverseOfx)    
  }

  # cached data is null, let's compute the inverse and store it
  data <- x$get()
  inverseOfx <- solve(data, ...)
  x$setinverse(inverseOfx)
  inverseOfx
}
