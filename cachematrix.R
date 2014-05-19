## The following functions are based off the examples from "Caching the Mean of a Vector"
##   The functions instantiate and manipulate matrix objects (and their calculated inverse)
##   with associated functional attributes set,get, setinv, getinv

## makeCacheMatrix takes a matrix and returns an object containing the matrix
##  the inverse of the matrix is not initially calculated and is set to NULL
##  functional attributes of the matrix object are created using this function

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve is used to query and manipulate a matrix object created by makeCacheMatrix
##  If the inverse of the matrix object is not NULL, the cached matrix inverse is returned
##  If no calculated inverse exists, the inverse of the matrix is calculated and set
##    for future retrieval
## For this assignment, we assume that the matrix supplied is always invertible
##   a simple example to play with:
##   m=makeCacheMatrix(matrix(c(4,3,3,2),2,2))

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
