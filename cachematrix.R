## Put comments here that give an overall description of what your
## functions do

## Compute the inverse of a matrix and cache it

makeCacheMatrix <- function(x = matrix()) 
  {
      m <- NULL
      set <- function(y) 
        {
          x <<- y
          m <<- NULL
        }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
  }


## Returns the inverse of a matrix. If the solution is cached, return the cached solution.
## If the solution is not cached, compute the inverse and cache it.

cacheSolve <- function(x, ...) 
  {
        ## Return a matrix that is the inverse of 'x'

    m <- x$getinv()
    if(!is.null(m)) 
    {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
  }
