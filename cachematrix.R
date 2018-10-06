## Functions in this file use caching to save time in potentially time consuming matrix invertions calculations. A test 
## function (testCaching) is included at the end to demonstrate the way caching works.

## makeCacheMatrix uses function closure and lexical scoping to maintain the state of a matrix and it's inversion. Both
## matrices can be set and retrieved.
##
## The inner function's enclosing environment contains storedMatrix & invertedMatrix.  It returns four functions that
## work on these variables:
##  - setMatrix
##  - getMatrix
##  - setInvertedMatrix
##  - getInvertedMatrix
makeCacheMatrix <- function(x = matrix()) {
    storedMatrix <- x
    invertedMatrix <- NULL
    
    cacheMatrixMethodList <- function() {
        setMatrix <- function(x) {
            storedMatrix <<- x
            invertedMatrix <<- NULL
        }
        
        getMatrix <- function() {
            return(storedMatrix)
        }
        
        setInvertedMatrix <- function(x) {
            invertedMatrix <<- x
        }
        
        getInvertedMatrix <- function() {
            return(invertedMatrix)
        }
        
        return(
            list(
                setMatrix = setMatrix,
                getMatrix = getMatrix,
                setInvertedMatrix = setInvertedMatrix,
                getInvertedMatrix = getInvertedMatrix
            )
        )
    }
    return(cacheMatrixMethodList())
}

## cacheSolve is a driver function that uses makeCacheMatrix (formal argument x) to store the result of a matrix 
## inversion. Subsequest calls will check to see if the inversion has been done already.  If yes, will retrieved the 
## inverted matrix from x and return it.  If no, will calculate the inverted matrix, store the result in x and return 
## the result.
cacheSolve <- function(x, ...) {
    invertedMatrix <- x$getInvertedMatrix()
    if (!is.null(invertedMatrix)) {
        message("Not doing the matrix inversion, returning cached data")
    } else {
        message("Doing the matrix inversion")
        invertedMatrix <- solve(x$getMatrix())
        x$setInvertedMatrix(invertedMatrix)
    }
    return(invertedMatrix)
}

## A test method that demonstrates the ability of makeCacheMatrix to maintain the state of matrices
testCaching <- function() {
    # Two by two matrix 1:
    #      [,1] [,2]
    # [1,]    4    2
    # [2,]    7    6
    # The inverse of this matrix:
    #      [,1] [,2]
    # [1,]  0.6 -0.7
    # [2,] -0.2  0.4
    #
    # Two by two matrix 2:
    #      [,1] [,2]
    # [1,]    5    8
    # [2,]    3    7
    # The inverse of this matrix:
    #            [,1]       [,2]
    # [1,]  0.6363636 -0.7272727
    # [2,] -0.2727273  0.4545455
    
    message("Generating matrix 1")
    cacheMatrix <- makeCacheMatrix(matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2))
    print(cacheMatrix$getMatrix())
    message("Generating matrix 2")
    cacheMatrix2 <- makeCacheMatrix(matrix(c(5, 3, 8, 7), nrow = 2, ncol = 2))
    print(cacheMatrix2$getMatrix())
    
    # This should do the calculation
    print(cacheSolve(cacheMatrix))
    print(cacheSolve(cacheMatrix2))
    
    message()
    message()
    
    # This should use cached values
    print(cacheSolve(cacheMatrix))
    print(cacheSolve(cacheMatrix2))
}
