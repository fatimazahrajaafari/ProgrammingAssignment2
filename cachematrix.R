## Caching the Inverse of a Matrix

## The following function takes as argument a square invertible matrix m and returns a list of functions:set,get,setinverse,getinverse
## This list is used as the input to the other function cacheSolve()

makeCacheMatrix <- function(m = matrix()) {
        n <- NULL
        set <- function(p){
          m <<- p
          n <<- NULL
        }
        get <- function() m
        setinverse <- function(inverse) n <<- inverse
        getinverse <- function() n
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The following function computes the inverse of the special "matrix" returned makeCacheMatrix(). 
##If the inverse has already been calculated (and the matrix has not changed), then it retrieves the inverse from the cache.  

cacheSolve <- function(m, ...) {
        ## Return a matrix that is the inverse of 'm'
        n <- m$getinverse()
        if (!is.null(n)){
            message("getting cached data")
            return(n)
        }
        data <- m$get()
        n <- solve(data,...)
        m$setinverse(n)
        return(n)
}
