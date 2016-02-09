## Caching the Inverse of a Matrix:
## Caching is ideally recommended solution where same operation is happening on same dataset
## Caching avoid repeating calculation and catched the result for future processing 
## Below function is for caching inverse of Matrix rather than doing repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## makeCacheMatrix function is for creating a matrix object for caching matrix
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  ##set the value of the matrix
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ##get the value of the matrix
  
  get <- function() x
  
  ##set the value of the inverse of the matrix
  
  setinv <- function(inverse) inv <<- inverse
  
  ## get the value of the inverse of the matrix
  
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

##  cacheSolve function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinv()
  
  ##checks to see if the inverse has already been calculated
  
  if(!is.null(inv)) {
    
    ##Return cache inverse data
    
    message("getting cached data")
    return(inv)
  }
  ##calculates the inverse of the matrix and sets the value of the inverse
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}