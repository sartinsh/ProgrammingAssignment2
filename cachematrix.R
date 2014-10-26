## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { # function that sets the value of the matrix
    x <<- y
    m <<- NULL
  }
  get <- function() x # function that gets the value of the matrix
  setinv <- function(solve) m <<- solve # function that computes the value of the inverse matrix
  getinv <- function() m # function that gets the value of the cashed iverse matrix
  list(set = set, get = get, #all four functions are returned as list
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) { 
  m <- x$getinv() #
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m ## Return a matrix that is the inverse of 'x'
}
