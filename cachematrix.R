## These functions create a special "matrix" object and calculate its inverse.
## If the inverse calculation is already made, and the matrix is same, then
## the functions retrieve the inverse from the cache instead of creating again.

## makeCacheMatrix create a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL # inverse matrix
      
      setmatrix <-function(y){
            if (is.matrix(y) == TRUE) x<<-y # initialize a matrix object
            else print("Input is not a matrix")
            m<<-NULL
      }
      
      getmatrix<-function() x
      
      setmatrixinverse <-function(y){
            if(is.matrix(y) == TRUE) m <<- y
            else print("Inpput is not a matrix")
      }
      
      getmatrixinverse<- function() m 
      
      list (getmatrix = getmatrix, setmatrix = setmatrix, 
            getinverse = getmatrixinverse, setinverse = setmatrixinverse)
}


## cacheSolve function computes the inverse of the "matrix" object
## created in makeCacheMatrix. And then, the cacheSolve function retrives 
## the inverse from the cache only if this has been already calculated, 
## and the matrix has not been changed.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
      
      matrix_inverse<-x$getinverse() #Check if the inverse has been already calculated.
      if (!is.null(matrix_inverse)) {
            print("getting inverse from cache")
            return(matrix_inverse)
      }
      
      ## if the inverse is not already calcualted, then calculate it
      matrix_value = x$getmatrix()  
      matrix_inverse = solve(matrix_value)
      x$setinverse(matrix_inverse)
      matrix_inverse        ## Return a matrix that is the inverse of 'x'
}
