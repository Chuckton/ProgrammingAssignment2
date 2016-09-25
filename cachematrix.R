## Put comments here that give an overall description of what your
## functions do

## Creates an special "object" which is of type MakeChaceMatrix
## This is the argument that the function cacheSolve() requires 
##1 intializes x and m 
##2 creates a list that has all the necessary referenced parts to 
## run cacheSolve()
##Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y){
                 x <<- y
                inv <<- NULL 
        }
        get <- function() x
        setinver <- function(inverse) inv <<- inverse
        getinver <- function() inv
        list(set = set, get = get, setinver = setinver, getinver = getinver)
}



## Write a short comment describing this function
##1 Takes an argument of type makeCacheMatrix
##2 Checks to see if the inverse of the matrix already exists
## If Yes tells user that it is getting the cached value and prints 
## "cached" value
##If No gets the matrix from the object of type makeCacheMatrix
##computes the inverse and sets the inverse to that value effectively
##caching the inverse

cacheSolve <- function(x, ...) {
              ## Return a matrix that is the inverse of 'x'
        inv <- x$getinver()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinver(inv)
        inv
}
