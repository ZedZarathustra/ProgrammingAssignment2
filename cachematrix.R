## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The first function creates a matrix, including setting the value of the matrix,
## getting the value of the matrix, setting the value of solve, getting the value of solve

makeCacheMatrix <- function(x = matrix()) {
     ## Sample Run:
     ## >mymatrix <- makeCacheMatrix(matrix(sample(1:9,9), 3, 3, byrow=TRUE))
     ## > cacheSolve(mymatrix)
     ##          [,1]        [,2]       [,3]
     ## [1,] -5.500000  0.25000000  4.2500000
     ## [2,]  1.666667 -0.16666667 -1.1666667
     ## [3,]  1.166667  0.08333333 -0.9166667
     ## > cacheSolve(mymatrix)
     ## getting cached data
     ## [,1]        [,2]       [,3]
     ## [1,] -5.500000  0.25000000  4.2500000
     ## [2,]  1.666667 -0.16666667 -1.1666667
     ## [3,]  1.166667  0.08333333 -0.9166667
     
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setsolve <- function(solve) m <<- solve
     getsolve <- function() m
     list(set = set, get = get,
          setsolve = setsolve,
          getsolve = getsolve)
}


## Write a short comment describing this function

## The function calculates the inverse of a matrix created with the above function, after
## checking to see if the inverse has already been calculated. If so, it retrieves the inverted matrix
## from the cache rather than calculating. If not already cached, it calculates inverse of the matrix
## writes that to the cache

cacheSolve <- function(x, ...) {
     m <- x$getsolve()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setsolve(m)
     m
}
