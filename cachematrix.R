## This function creates a special matrix object that can cache its inverse.
## This function is based on the example "makeVector" provided in week 3 of R course

makeCacheMatrix <- function(x = matrix()) 
{
        inverseMatrix <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverseMatrix <- function(inverse) inverseMatrix <<- inverse
        getinverseMatrix <- function() inverseMatrix
        list(set = set, get = get,setinverseMatrix = setinverseMatrix,
        getinverseMatrix = getinverseMatrix)
}
        


# This function provides the inverse of the special matrix 
# returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) 
{
        ## Return a matrix that is the inverse of 'x'
        inverseMatrix <- x$getinverseMatrix()
        if(!is.null(inverseMatrix)) {
                message("getting cached matrix data")
                return(inverseMatrix)
        }
        data <- x$get()
        inverseMatrix <- mean(data, ...)
        x$setinverseMatrix(m)
        inverseMatrix
}