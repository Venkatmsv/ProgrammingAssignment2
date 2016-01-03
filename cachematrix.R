## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.

## The function makeCacheMatrix is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of inverse of the matrix
## get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
       
       matInverse <- NULL
       
       set <- function(y){
              x <<- y
              matInverse <<- NULL
       }
       
       get <- function() x
       
       setInverse <- function(inverse) matInverse <<- inverse
       getInverse <- function() matInverse
       
       list(set=set,get=get, setInverse=setInverse,getInverse=getInverse)
       
}


## This method has the inverse metrix object as the first parameter and get the inverse value of the object.
## If the object is null then it will do the computation for inverse and return the value. If the object is not null 
## then it will return the value from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
       
       invse <- x$getInverse()
       if(!is.null(invse)){
              message("getting cache data")
              return(invse)              
       }
       data <- x$get()
       invse <-  solve(data)
       x$setInverse(invse)
       return (invse)
       
}




