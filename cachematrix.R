## cacheSolve(Y) computes the inverse for the input matrix X
## Argument Y is a matrix object created by makeCacheMatrix(X,...)
## which is used to cache the inverse matrix for future use

## Create a matrix object for caching inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
        inverse <- NULL
  
        set <- function(y)  {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
  
        list(set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
}

## Compute inverse using the special object returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
       
        inverse <- x$getinverse()
  
        if (!is.null(inverse))  {
                message("getting chached inverse matrix")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
