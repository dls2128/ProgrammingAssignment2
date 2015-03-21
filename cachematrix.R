## makeCacheMatrix creates a special "matrix" which is a list containing a function to: 
## 1. set the value of the vector
## 2. get the value of the vector
## 3. set the value of the mean
## 4. get the value of the mean

## cacheSolve calculates the inverse of the special "matrix" created with 
## makeCacheMatrix. However, it first checks to see if the inverse has already 
## been calculated. If so, it gets the inverse from the cache and skips the 
## computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinverse function.


## INSTRUCTIONS TO RUN THESE FUNCTIONS: 
## 1. At command line, define a matrix x (must be square, e.g. 3 x 3) for use 
## with makeCacheMatrix
## 2. To run makeCacheMatrix: type "mat <- makeCacheMatrix(x)" (without quotations)
## in the command line
## 3. To run cacheSolve: type "cacheSolve(mat)" (without quotes) in command line

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
        
cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- x$getinverse()
                if(!is.null(m)) {
                        message("getting cached inverse")
                        return(m)
                }
                inverse <- x$get()
                m <- solve(inverse, ...)
                x$setinverse(m)
                m
        }
                