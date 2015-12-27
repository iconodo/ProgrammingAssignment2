## 1.- Theory: a new instance of function makeCacheMatrix() is a closure, 
## and its enclosing environment is the environment created when it is run.  
## The closure maintains access to the environment (functions and data) 
## in which it was created. When makeCacheMatrix is fed with a valid 
## matrix object; it makes use of '<<-' operator. 
## 2.- Source http://adv-r.had.co.nz/Functional-programming.html#mutable-state
## 3.- Practice: just changed the object from numeric() to matrix(), 
## and names (from mean/m to solve/s) from the example provided
## 4. PS that declares a superficial/partial understanding of the assignment



## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     s <- NULL
     set <- function(y) {
          s <<- NULL
     }
     get <- function() x
     setinverse <- function(solve) s <<- solve
     getinverse <- function() s
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve retrieves 
## the inverse from the cache... preceded by message "getting cached data"

cacheSolve <- function(x, ...) {
     s <- x$getinverse()
     if(!is.null(s)) {
          message("getting cached data")
          return(s)
     }
     data <- x$get()
     s <- solve(data, ...)
     x$setinverse(s)
     s
}