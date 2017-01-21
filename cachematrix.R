#Name: Igor Giordano Liberto

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL                             ## this variable will hold value of matrix inverse
  set <- function(y) {                   
    x <<- y                             ## with the <<- operator we can set the value of the matrix in parent environment
    inv <<- NULL                        ##Wil reset the value of inv in the case of a new matrix                       
  }
  get <- function() x                    
  
  setinv <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinv <- function() inv                     ## gets the value of inv
  
  ##The list bellow is necessary in order to refer to the functions using $
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Write a short comment describing this function:
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix but if the inverse has already been
## calculated the return value will be the inverse from the cache.
cachesolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
