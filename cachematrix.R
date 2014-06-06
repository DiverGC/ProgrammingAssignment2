## This script allows the user to create and cache the inverse of a matrix.

## The function makeCacheMatrix creates an object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     
     m<-NULL
     
     ##Sets the matrix
     set<-function(y){
          x<<-y
          m<<-NULL
     }
     
     ##Gets the matrix
     get<-function() x
     
     ##Sets the inverse of the matrix
     setinverse<-function(inverse) m<<-inverse
     
     ##Gets the inverse of the matrix
     getinverse<-function() m
     
     ##Stores all 4 previous matrices in a list.
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
     
}


## The function cacheSolve computes the inverse from makeCacheMatrix.  If an inverse exists
## and the matrix has not changed, the inverse will be retrieved from the cache.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     
     #Retrieves the inverse
     m<-x$getinverse()
     
     #Checks to see if the inverse exists and if the matrix has changed
     if(!is.null(m) & identical(m,x)){
          message("getting cached data")
          return(m)
     }
     
     #Gets the matrix
     data<-x$get()
     
     #Calculates the inverse
     m<-solve(data, ...)
     
     #Sets the inverse
     x$setinverse(m)
     m
}
