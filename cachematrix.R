## Matrix inversion is a costly computation and to repeatedly do it will be even more.
## SO to cache the value for future computations make sense.

## makechache matrix creates a "matrix" which is a list containing
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of inverse
## 4. get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
        }
        get <- function()x
        setinverse <- function(Solve) m <<- solve
        getinverse <- function() m
        list(set =set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function cachesolve returns inverse of the matrix. It checks if the inverse is computed, if so it gets the inverse without computation 
## or else it computes the inverse and sets the value in cache. cachesolve assumes marix is invertible

cacheSolve <- function(x, ...) {
       m <- x$getinverse()
       if(!is.null(m)){
               message("getting cached data")
               return(m)
       }
         data <- x$get()
         m<- solve(data)
         x$setinverse(m)
         
         m
         
}
