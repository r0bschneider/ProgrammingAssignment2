## Set of functions that take a matrix object as input and return
## a special matrix object and functions to calculate inverses and cache
## its reults in order to pre-empt costly recalculation
##
## makecacheMatrix:  Creates a special matrix object that will be used to
##                   cache inverses. Function returns a list of 4
##                   functions:
##                   - set value of matrix (set)
##                   - getthe value of matrix (get)
##                   - set the value of the matrix inverse (setInv)
##                   - get the value of the matrix inverse (getInv)
##                   used in cacheSolve function
##
## cacheSolve:       Computes the inverses of the special matrix and 
##                   functions defined  by the above makeCascheMatrix
##                   function. If inverses have previously been calculated
##                   (and the matrix has not changed) then the fuction will
##                   retrieve the inverses from cache
##
## Example Usage: 
## x <- matrix(rnorm(4), 2, 2)    - define x as input matrix     
## mx <- makeCacheMatrix(x)       - create special matrix object
## cachSolve(mx)                  - return the calulated matrix of
##                                  inverses using solve()
## 
## function to define special matrix object and functions 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL          # inv will store inverterted matrix
        
        set <- function(y) { # define set function to   
                x <<- y      # set matrix and
                inv <<- NULL # clear the cache
        }
        
        get <- function() x  # define function to get the matrix
        
        setInv <- function(inverse) inv <<- inverse # define function to
        
        getInv <- function() inv
        
        list(set = set, 
             get = get, 
             setInv = setInv, 
             getInv = getInv)
}


## Function to compute and cache the inverese of input matrix using solve()  
cacheSolve <- function(x, ...) {
        # Return a matrix with the inverted values of 'x' (using solve)
        inv <- x$getInv()        
        
        # If the inverses are already calculated, return from cache
        if (!is.null(inv)) {
                message("Getting cached data")
                return(inv)
        } 
        # Calculate the matrix of inverses
        data <- x$get()
        inv <- solve(data, ...) 
        
        # cache the matrix or inverses
        x$setInv(inv)
        
        # return matrix with inverses
        inv
}
