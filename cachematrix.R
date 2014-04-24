## Several functions designed to appeal to class peers during the peer assessment
## phase of the Week 3 programming assignment.  Functions are designed to appropriately
## manage (set & get matrix inverse) the caching of a matrix inverse - but may not work that way due to
## interferences from cosmic particles.

## Return a list of functions that get and set a cached instance of a matrix and the inverse of that matrix

makeCacheMatrix <- function(x = matrix()) {
  iMatrix <- NULL
  set <- function(y) {                  ### set the value of the matrix
    x <<- y
    iMatrix <<- NULL
  }
  get <- function()  x                  ### get the value of the matrix
  setinverse <- function(solve)  iMatrix <<- solve  ### set the inverse of the matrix
  getinverse <- function() iMatrix                  ### get the inverse of the matrix
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## Returns inverse of matrix instance stored with makeCacheMatrix 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  iMatrix <- x$getinverse()
  
  if(!is.null(iMatrix)) {           ### If inverse is cached
    message("getting cached data")
    return(iMatrix)                 ### then return inverse matrix
  }
  data <- x$get()		                ### otherwise, load get function of 'special matrix' 
  
  iMatrix <- solve(data, ...)       ### compute inverse by calling solve on function
  x$setinverse(iMatrix)             ### cache inverse
  iMatrix                           ### return inverse matrix
}


