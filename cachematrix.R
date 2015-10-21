## Here are two functions. The first one creates a special "matrix" 
## object that can cache its inverse. The second function computes
## the inverse of the special "matrix" returned by makeCacheMatrix.


## The first function, makeCacheMatrix creates a special "matrix", 
## which is really a list containing a function to

## 1. set the value of the matrix
## 2. the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(Inverse) m <<- Inverse
  getinverse <- function() m
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
}


## The following function calculates the inverse matrix of the special
## "matrix" created with the above function. If the inverse matrix has
## been calculated, it gets the inverse matrix from the cache and skips
## the computation. Otherwise, it calculates the inverse matrix of the 
## data and sets the value of the inverse matrix in the cache via the 
## setinverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  
  m
}
