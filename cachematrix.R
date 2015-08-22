## makeCacheMatrix amd cacheSolve work together
## makeCacheMatrix takes a matrix and caches its inverse
## usage: a <- makeCacheMatrix(x) where x is a square matrix
##        if matrix is non-invertible, the function exits with en arror message
##
## cacheSolve takes the object returned by makeCacheMatrix and computes
##        the inverse if it does not altready exist
## usage: inv <- cacheSolve(a)

## Write a short comment describing this function
## Takes a quare matrix and checks it it is invertible
## If it is invertible it creates a list with four functions:
## setMatrix: sets the value of the matrix
## getMatrix: gets the value of the matrix
## setInv:    sets the inverse of the matrix
## getInv:    gets the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  
   # Check for invertibility
  if(det(x) == 0){
    print('Matrix is singular: Inverse does not exist! Please use another matrix')
    return
  }
  invMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function()x
  setInv <- function(inVerse) invMatrix <<- inVerse
  getInv <- function()invMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the object returned by makeCacheMatrix
## If the inverse exists: the cached value is returned
## If the inverse does not exist it is calculated and returned

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invMatrix <- x$getInv()
    if(!is.null(invMatrix)){
      message("getting cached data")
    return(invMatrix)
  }
  data <- x$getMatrix()
  print(data)
  invMatrix <- solve(data)  # Computes the inverse
  x$setInv(invMatrix)
  invMatrix
}
