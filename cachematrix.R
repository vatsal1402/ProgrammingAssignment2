## Put comments here that give an overall description of what your
## functions do

## The following function is used for creating a special matrix
## whose inverse is to be calculated...
## makeCacheMatrix returns a list containg functions to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse of that matrix
## get the value of the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  
  ## We initially set our inverse variable to null
  inverse_x<-NULL 
  
  ## The following function sets the value of the matrix
  set <- function(y) {
    x<<-y
    inverse_x<-NULL
  }
  
  ## The following function is used for getting the value of our matrix
  get<-function() {
    x
  }
  
  ## The following function is used for setting the value of the inverse 
  setinverse<-function(inverse) {
    inverse_x<-inverse
  }
  
  ## The following function is used for getting the value of the inverse
  getinverse<-function()
  {
    inverse_x
  }
  
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
  
}

## The following function calculates the inverse of the special matrix 
## created above...

cacheSolve <- function(x, ...) {
  
## Return a matrix that is the inverse of 'x'
  
    inverse_x<-x$getinverse()

## But first we check to see if the inverse has already been calculated

## if so get the inverse from the cache and skip the computation..    

    if(!is.null(inverse_x))
    {
      message("getting cached data")
      return(inverse_x)
    }
    
## Calculate the inverse of the matrix 'mat' and store in the cache
## via the 'setinverse' function...
    
## Lastly the function returns the inverse of the matrix...
    
    mat<-x$get()
    inverse_x <- solve(mat)
    x$setinverse(inverse_x)   
    inverse_x
}
