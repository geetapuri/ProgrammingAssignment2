## MakeCacheMatrix and cacheSolve will be used in tandem to compute the inverse of a Matrix and cache its inverse

## The following function makeCacheMatrix takes a matrix as an input.  
##Please note that only non singular matrices must be provided for test.
## makeCacheMatrix doesnt actually calculate the inverse.  It only stores it in cache.

makeCacheMatrix <- function(x=matrix()) {
     my.matrix <- NULL
     #my.matrix <- matrix(9:1,3,3)
     
     set <- function(y) {
          x <<- y
          my.matrix<<- NULL
     }
     get <- function() x
     setInverseMatrix <- function(solve) my.matrix <<- solve
     getInverseMatrix <- function() my.matrix
     list(set= set, get = get, setInverseMatrix = setInverseMatrix, 
          getInverseMatrix = getInverseMatrix)
}


## The following function cacheSolve checks for the matrix inverse in cache.  If not found, 
## then it calculates the inverse, using solve, and returns the inverse matrix.

cacheSolve <- function(x, ...){
     my.matrix <- x$getInverseMatrix()
     if(!is.null(my.matrix)) {
               message("getting cahced data")
               return(my.matrix)
     }
     data <- x$get()
     my.matrix <- solve(data, ...)
     x$setInverseMatrix(my.matrix)
     my.matrix
}
