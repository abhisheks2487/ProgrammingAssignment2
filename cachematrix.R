## The functions below can be called to return the inverse of a matrix from the cache to reduce computational power.


## This function creates an object of matrix type that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  mtrx <- NULL
  set <- function(y) {
    x <<- y
    mtrx <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) mtrx <<- inverse
  getinverse <- function() mtrx
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## We run this function to compute the inverse of the matrix returned by the function above.
## If the inverse has already been calculated once and the matrix has not changed in the meanwhile, 
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  ## Return a matrix that is the inverse of 'x'
  
  mtrx <- x$getinverse()
  if(!is.null(mtrx)) {
    message("getting cached data")
    return(mtrx)
  }
  data <- x$get()
  mtrx <- solve(data, ...)
  x$setinverse(mtrx)
  mtrx
}

## Code below can be used to test the function above

my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix$get()
cacheSolve(my_matrix)
cacheSolve(my_matrix)
my_matrix$set(matrix(c(2, 2, 1, 4), 2, 2))
cacheSolve(my_matrix)
cacheSolve(my_matrix)
