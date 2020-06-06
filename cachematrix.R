## It is time consuming and costly to calculate compute inverse matrices in R. 
## Hence, below are two functions that are used to create a special object that stores a matrix
## and subsequently cache's its inverse to use for later analysis if required

## Caching the inverse of a square matrix

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
          x <<- y
          i <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) i <<- inverse
      getinverse <- function() i
      list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

    
## Returning a matrix that is the inverse of a matrix 'x'. 
## However, the code first checks whether there is an inverse matrix that has been already created

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
            if(!is.null(i)) {
            message("getting cached inverse matrix")
      return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}

## Checking the code 
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))  #a random 2x2 matrix
my_matrix$get()   #Printing the matrix
cacheSolve(my_matrix)   #Returning the inverse matrix
