## Here, I'm solving 2nd Programming assignment in the course "R PRogramming"
## I'm creating two functions 1. makeCacheMatrix and 2.cacheSolve

## Here, "makeCacheMatrix" function creates special "matrix" 
##object that can cache its inverse. For e.g. matrix that we will 
## pass through the function, will get stored & its inverse will 
## be cached


makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y){
    x <<- y
   inver <<- NULL 
    
  }
  
  get <- function() x
  setinver <- function(inverse) inver <<- inverse
  getinver <- function() inver
  list(set = set, get = get, setinver= setinver, getinver = getinver)
}


## cachesolve, takes the matrix as input. Then it checks whether
## inverse of the input matrix is already cached or not
## If inverse is already cached, then it'll retrive the inverse
## otherwise it'll calcualte the inverse and store it in cahce

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inver <- x$getinver()
  if (!is.null(inver)){
    message("Cached data will be shown")
    return(inver)
    
  }
  inp <- x$get()
  inver <- solve(inp,...)
  x$setinver(inver)
  inver
  
}
