## makeCacheMatrix amd cacheSolve work together
## makeCacheMatrix takes a matrix and caches its inverse
## usage: a <- makeCacheMatrix(x) where x is a square matrix
##        if matrix is non-invertible, the function exits with an error message
##
## cacheSolve takes the object returned by makeCacheMatrix and computes
##        the inverse if it does not already exist
## usage: inv <- cacheSolve(a)


## Takes a square matrix and checks it it is invertible
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
  
  # Getter and Setter Functions for the matrix x
  invMatrix <- NULL
  setMatrix <- function(y){
    x <<- y
    invMatrix <<- NULL
  }
  getMatrix <- function()x
  
  # Getter and Setter functions for matrix inverse
  setInv <- function(inVerse) invMatrix <<- inVerse
  getInv <- function()invMatrix
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInv = setInv, getInv = getInv)
}


## Write a short comment describing this function
## cacheSolve returns the inverse of the object returned by makeCacheMatrix
## If the inverse exists: the cached value is returned
## If the inverse does not exist it is calculated and returned

cacheSolve <- function(x, ...) {
    ## Return an inverse of the matrix that is in the object 'x' created by makeCacheMatrix
  
    invMatrix <- x$getInv()  # Get the inverse of the matrix
    
    if(!is.null(invMatrix)){
      message("getting cached inverse") # Get cached value if not NULL
    return(invMatrix)                   # Return the cached value
  }
  data <- x$getMatrix()     # Get the matrix to be inverted
  
  invMatrix <- solve(data)  # Computes the inverse
  x$setInv(invMatrix)       # Sets the inverse
  invMatrix                 # Returns the inverse
}
