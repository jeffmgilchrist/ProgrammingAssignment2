## These 2 functions create a matrix as requested by the user, and 
## then calculate the inverse of the matrix and cache it.
## If the user asks to calculate the inverse of the matrix, and
## The inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

## this function has 4 functions:
## 1. Set a matrix
## 2. Get the matrix
## 3. Set the inverse of the matrix
## 4. Get the inverse the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## this function calculates the inverse of the matrix created by makeCacheMatrix
## It checks to see if the inverse has already been calculated
## If so, it gets the inverse from the cache and skips the calculation
## If not, it'll calculate the inverse via the setinverse function
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
