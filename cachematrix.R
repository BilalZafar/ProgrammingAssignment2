## These functions can compute and cache the inverse of a matrix.

## ## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  ## This function stores the matrix in cache using <<- operator
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get and sets the matrix value
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse 
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## computes the inverse of the matrix returned by makeCacheMatrix(). 
## If the inverse has already been calculated and the matrix has not changed, 
## it will retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  ## returns the inverse of the original matrix input to makeCacheMatrix
  
  inv <- x$getinverse()
  
  # if you already cached inverse
  if (!is.null(inv)){
    # get it from the cache 
    message("getting cached data..")
    return(inv)
  }
  
  # in other case you have to calculate the inverse 
  mat.data <- x$get()
  inv <- solve(mat.data, ...)
  
  # sets the value of the inverse in the cache
  x$setinverse(inv)
  
  return(inv)
        
}
