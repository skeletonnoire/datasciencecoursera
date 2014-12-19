# In the first step we create functions that will create and use invertes matrices. For datasets with many vectors this will make it easier to make many simple computations, such as descriptive statistics.

# We proceeds as follows: first set and get the values of the matrices; second we do the same (get and set) the values for the inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inverse
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The above creates a matrix object which holds the inverse of a matrix.
# We now use 'CacheSolved' to see if a solution for the inverse matrix already exists. In the case it doesn't the function will create the inverse of the matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}