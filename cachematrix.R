# The functions store variables in the global environment, a matrix and its
# inverse. If a matrix inverse has already been calculated it is retrieved
# from the global environment rather than calculating it again which is
# computationally expensive.

# Create cached matrix in golbal environment. Create and return list of
# functions to set and get the two matrix variables.
makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    setMatrix <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    getMatrix <- function() x
    setInverseMatrix <- function(inverseMatrix) inverseMatrix <<- inverseMatrix
    getInverseMatrix <- function() inverseMatrix
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverseMatrix = setInverseMatrix,
         getInverseMatrix = getInverseMatrix)
}


# If the inverse has been previously calculated it is returned from a cache. 
# Otherwise the inverse is calculated, cached and returned.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # If cached inverse matrix exists it is returned
    inverseMatrix <- x$getInverseMatrix()
    if(!is.null(inverseMatrix)) {
        message("getting cached data")
        return(inverseMatrix)
    }
    # Create inverse matrix, store it in cache and return it
    data <- x$getMatrix()
    inverseMatrix <- solve(data, ...)
    x$setInverseMatrix(inverseMatrix)
    inverseMatrix
}