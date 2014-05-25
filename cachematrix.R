## Put comments here that give an overall description of what your
## functions do
# In the makeCacheMatrix function, it is a function
# which has 4 subfunctions:
# setMat, getMat, saveMatrix, and getMatrix. 
# setMat purpose is to have a matrix passed 
# through it and then store it as x. It also
# clears the cache. getMat purpose is to grab 
# the matrix that might be stored in x and then
# return it.saveMatrix's purpose is to save 
# into cache the inversed matrix. getMatrix's
# purpose is to retrieve the cached inverse
# matrix.
# 
# In the cacheSolve funciton, its purpose 
# is to check to see if an inverse has already
# been calculated. If it has, then it will retrieve
# the matrix inverse from the cache. If not, it 
# will proceeed calculating the inverse of the matrix.


## Write a short comment describing this function
# This function creates a list of subfunctions which can
# define a matrix, retrieve a matrix,
# cache a matrix, and retrieve cached matrix

makeCacheMatrix <- function(x = matrix()) {
  #If object called without method
  cache <- NULL
  
  #Defines Matrix
  setMat <- function(y){
    x <<- y
    cache <<- NULL
  }
  
  #Retrieves Matrix
  getMat <- function()x
  
  #Saving into cache
  saveMatrix <- function(inverse) cache <<- inverse
  
  #Retrieving from cache
  getMatrix <- function()cache
  
  list(setMat = setMat, getMat = getMat,
       saveMatrix  = saveMatrix,
       getMatrix = getMatrix)
  
  
}


## Write a short comment describing this function
# This function is able to check to see if a inverse
# of a matrix has already been calculated and cached.
# If not, then it goes ahead and calculates the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  cache <- x$getMatrix()
  
  #checks to see if inverse already solved;
  #if so then returns cached inverse
  if(!is.null(cache)){
    message("Retrieving cached inverse")
    return(cache)
  }
  
  #Retrieves matrix
  theMatrix <- x$getMat()
  
  #solves for inverse
  cache <- solve(theMatrix, ...)
  
  #Store inverse into cache
  x$saveMatrix(cache)
  
  #Returns inverse to user
  cache
  
}

# #Test cases to confirm functions work
# amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2)) 
# amatrix$getMat() # Returns original matrix
# cacheSolve(amatrix) # Computes, caches, and returns matrix inverse
# amatrix$getMatrix() # Returns matrix inverse
# cacheSolve(amatrix) # Returns cached matrix inverse using previously computed matrix inverse
# 
# amatrix$set(matrix(c(0,5,99,66), nrow=2, ncol=2)) # Modify existing matrix
# cacheSolve(amatrix) # Computes, caches, and returns new matrix inverse
# amatrix$getMat() # Returns matrix
# amatrix$getMatrix() # Returns matrix inverse