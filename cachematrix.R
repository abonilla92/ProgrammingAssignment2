# Overview

# The benefits of lexical scoping/scoping rules is that functions can be written  to avoid repeatedly 
# computing the same data element(s). It creates efficiencies in the code, but also on system performance.

# The following pair of functions aim to do this setting an inverse of a matrix and checking
# if the inverse has been set each time the code is run. If it has, then retrieve the inversed matrix from
# cache rather than computing it again.

## *****************************************************************************
## This function constructs a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        mtrx <- NULL #Set as null so can be used within function later
        
        # Take the matrix passed into makeCacheMatrix and assign a value to an object in an environment
        # that is different from the current environment through the <<- operator
        # This object will be referenced in the cacheSolve function below through the functions defined further below
        
        set <- function(y) {
                x <<- y
                mtrx <<- NULL

        }
        
        # The three functions below enables us to get the value of the matrix passed as an argument above
        # or provide the code to set and get the inverse values
        get <- function() x
        
        setinverse <- function(inverse) mtrx <<- inverse
        
        getinverse <- function() mtrx
        
        
        #Declaration of functions to make them availabe as objects in cacheSolve()
        #The first element is the method name that is used to access the function
        list(setmatrix=set, getmatrix=get, setinverse=setinverse, getinverse=getinverse)
}



## ******************************************************************************
# The following function is where the computations are performed to check whether
# the matrix was inversed. If so, get it from cache, otherwise invert it.

cacheSolve <- function(x, ...) {
        
        mtrx <- x$getinverse()
        
        # Check if matrix was previously inversed and return if not null
        if(!is.null(mtrx)) {
                
                message("...getting cached data.")
                
                return(mtrx)
        }
        
        # If matrix not inversed, get the matrix
        data <- x$getmatrix()
        
        # Invert the matrix via Solve
        mtrx <- solve(data)
        
        # Call setinvese from makeCacheMatrix
        x$setinverse(mtrx)
        
        # Return the result to the console
        mtrx
}
