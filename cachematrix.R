## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#The "makeCacheMatrix" function takes a matrix as an argument.
# 
# A list is defined in the body of the function and its include
# a set,get,setinverse, and getinversion functions

# Testing - step 1 : mat <- makecachematrix() 
#           step 2 : mat$set(matrix(1:4,nrow=2,ncol=2))           
#           step 3 : cacheSolve(mat)

makeCacheMatrix <- function(x = matrix()) {
  
  # Set the numeric inverse to NULL  
    inverse <- NULL 
    
  # Set function takes a matrix as argument
    set <- function(y) {
      
      x <<- y  # the Special assignment "->>" is used and it allows to update the free variable x
      inverse <<- NULL
    }
   
  # get function
    
    get <- function() x
  
  #setinverse function
    
    setinverse <- function(solve) inverse <<- solve
    
    getinverse <- function() inverse
    
    list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
    
  }
  
}



## Write a short comment describing this function
# Matrix inversion is usually a costly computation and 
#their may be some benefit to caching the inverse of a 
#matrix rather than compute it repeatedly 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
    inverse <- x$getinverse()
    if(!is.null(inverse)){ # test if the inverse matrix is in the memory
      message("getting cached data")
      return (inverse) }
   # Matrix doesn't exist in the memory 
    matrix <- x$get() #get the matrix through the use of the get function
    inverse <- solve(matrix,...) # R function that create the Inverse Matrix
    x$setinverse(i)
    inverse
}
# Test the function

mat <- makecachematrix() 
mat
mat$set(matrix(1:4,nrow=2,ncol=2))
cacheSolve(mat)
