## Assignment: Caching the Inverse of a Matrix
## The functions below can compute the inverse of a square matrix and store this inverse in cache
## for future use in order to reduce computational cost

## Function "makeCacheMatrix" creates a special "matrix", which is a list
## contatining a function to :
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the matrix inverse
## 4. get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  ## 1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## 2. get the value of the matrix
  get <- function() x 
  ## 3. set the value of the matrix inverse
  setinv <- function(inverse) inv <<- inverse
  ## 4. get the value of the matrix inverse
  getinv <- function() inv
  
  ## Return list with the 4 defined function
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
  
}

## function "cacheSolve" takes the special matrix created by function "makeCacheMatrix"
## and computes its inverse. If the inverse has been previously calculated  it gets 
## the inverse from the cache and skips the computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  #Check if matrix inverse has been previsouly computed and is cached
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  #If inverse is not cached proceed to computing the inverse
  data <- x$get()            #Get matrix
  inv <- solve(data, ...)    #Compute the inverse
  x$setinv(inv)              #Cache inverse
  inv                        #Return
  }
