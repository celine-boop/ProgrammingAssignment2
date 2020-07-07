## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  j <- NULL
  set <- function(y){ #set the value of the vector
    x <<- y
    j <<- NULL
  }
  get <- function()x #get the value of the vector
  setInverse <- function(inverse)j <<- inverse #set the value of the mean
  getInverse <- function()j #get the value of the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j <- x$getInverse
  if(!is.null(j)){ #checks to see if the inverse has already been calculated
    message ("getting cached data")
    return (j) #if not null, gets inverse from the cache and skips the computation
  }
  mat <- x$get() #if null, calculates inverse of the data 
  j <- solve(mat, ...)
  x$setInverse(j) #sets the value of the inverse in the cache
  j
}
