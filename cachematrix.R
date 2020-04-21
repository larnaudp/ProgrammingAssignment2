## This couple of functions can be used to reduce the computational 
## effort to calculate the inverse of a determined matrix

## The first function, makeCacheMatrix, creates a list containing 4 
## functions with the following purposes:
##    1)	set: used to set the value of the matrix
##    2)	get: used to return the value of the matrix
##    3)	setinverse: used to set the matrix inverse
##    4)	getinverse: used to return the value of the matrix inverse


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    get <- function() x
    setinverse <- function(matrixinverse) i <<- matrixinverse
    getinverse <- function() i
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## The second function, cacheSolve, is used to calculate the inverse 
## of the matrix contained in the list created by the makeCacheMatrix 
## function, whose value can be returned using the element “get” of 
## the list. If this specific object has already an inverse calculated, 
## the function will simply return this existing value; if the value 
## is still NULL, the function will calculate it using the function 
## “solve”

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
