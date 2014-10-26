## Functions makeCacheMatrix and cacheSolve are used for computing inverse matrix of a single sqare matrix
## and storing it. In case the inverse matrix is necessery again its cashed value is used.

## Function makeCacheMatrix takes single square matrix and stores it along with its inverse matrix when it is computed.
## Functions that are neccesery to manipulate cashed data are definde inside of makeCacheMatrix and returned as list.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {                       # function that updates the input matrix
    x <<- y                                  # input matrix is asigned to 'x'
    m <<- NULL                               # when new matrix is put in to cash the value of its inverse is set to NULL
  }
  get <- function() x                        # function that gets the value of the matrix (simply returns object 'x')
  setinv <- function(inverse) m <<- inverse  # function that globally asigns inverse matrix of 'x' to 'm'
  getinv <- function() m                     # function that returns the value of the cashed iverse matrix (returns object 'm')
  list(set = set, get = get,                 # all four functions mentioned above are returned as list
       setinv = setinv,
       getinv = getinv)

}

## cacheSolve uses data manipulation functions defined in makeCacheMatrix to acces and update cashed inverse matrix.

cacheSolve <- function(x, ...) { 
  m <- x$getinv()                           # cashed inverse matrix of 'x' or NULL is asigned to m
  if(!is.null(m)) {                         # checking if the inverse matrix is already computed (m is not NULL)
    message("getting cached data")
    return(m)                               # in case inverse of matrix 'x' has already been computed it is returned
  }
  data <- x$get()                           # input matrix is asigned to object 'data'
  m <- solve(data, ...)                     # inverse of the matrix 'x' is computed and locally asigned to 'm'
  x$setinv(m)                               # inverse matrix is put in to in cash (globally asigns inverse matrix to 'm')
  m                                         # matrix that is the inverse of 'x' is returned
}