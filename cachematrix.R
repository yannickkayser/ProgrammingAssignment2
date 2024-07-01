
## the cache function that outputs a list of function to set the matrix, to get the matrix 
## and then to get and set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # location to store the inverse of the matrix
  
  # function to set the matix 
  set <- function(y) {
    x <<- y
    inv <<- NULL  # if we set a new matrix we have to reset the inverse to NULL
  }
  
  # function to get the matrix
  get <- function() x
  
  # function to set the inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # function to get the inverse
  getInverse <- function() inv
  
  # Return the list of functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## the cacheSolve function computes the inverse of the matrix created by "makeCacheMatrix",
## if the inverse is already cached, it retreives it from the cache, 
## otherwise it computes the inverse, caches it and returns it

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() # check if the inverse is cached already
        
        if(!is.null(inv)){
          print("getting cached inverse")
          return(inv) #returns the cached inverse
        }
        
        ## otherwise compute the inverse
        data <- x$get() #get the matrix
        inv <- solve(data,...) #compute the inverse
        x$setInverse(inv) #cache the calculated inverse
        inv # return the inverse
}
