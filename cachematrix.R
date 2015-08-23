## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. The following two functions cache the inverse 
## of a matrix.

## The makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:
## - set the value of the vector
## - get the value of the vector
## - set the value of the mean
## - get the value of the mean

makeCacheMatrix <- function(mat = matrix()) {
  # setting the local variable minv to NULL
  minv <- NULL
  set <- function(y) {
    # setting the mat matrix to y
    mat <<- y 
    # caching the minv inverse of the matrix (as will be defined below) to null
    minv <<- NULL 
  }
  get <- function() mat
  # the inverse of the matrix is set with this function
  setinverse <- function(solve) minv <<- solve
  # the inverse is retrieved with this function
  getinverse <- function() minv
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)
}


## The following function calculates the inverse of the special "vector" created
## with the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and skips 
## the computation. Otherwise, it calculates the inverse of the data and sets 
## the inverse matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # we get here the inverse of the matrix and check whether it is NULL
  minv <- x$getinverse()
  if(!is.null(minv)) {
    # if not NULL (=cached) we return it with a warning
    message("getting cached data")
    return(minv)
  }
  # if it was NULL we retrieve the matrix
  data <- x$get()
  # and compute its inverse
  minv <- solve(data)
  # we set such computed inverse in the special "vector"
  x$setinverse(minv) 
  # and we return it
  minv
}
