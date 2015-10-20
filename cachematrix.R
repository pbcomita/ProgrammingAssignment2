## Put comments here that give an overall description of what your
## functions do

## This function will  creates a special "matrix" object that can cache its 
## inverse, which is an inverted matrix from the 
## function 'Solve()'. If there is no inverted matrix, the matrix object
## will return with a NULL vector 
## the function is defined for a matrix that has an inverse

makeCacheMatrix <- function(x = matrix()) {
 
        ##first set mat to NULL
        mat <- NULL
        set <- function(y) {
                x <<- y
                mat <<- NULL 
        }
        get <- function() x
        setinv <- function(solve) mat <<- solve
        getinv <- function() mat
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

## this function cacheSolve will return a matrix 'mat' that is the inverse of the input matrix 
## using the R-supplied function 'solve()'. The matrix 'mat' is 
## assumed to be invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## first check to see if the inverse of 'x' has already been calculated
        ##
        mat <- x$getinv()
        if(!is.null(mat)) {
                message("getting cached inverse matrix")
                return(mat)                              ##returns previous inverse matrix
        }
        ## otherwise calculate the inverted matrix and return it as mat
        newdata <- x$get()
        mat <- solve(a = newdata)
        x$setinv(mat)
        mat
}
