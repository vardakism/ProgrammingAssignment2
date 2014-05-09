## Two functions to calculate a special matrix containing the cached inverse
## and to calculate or cache the inverse matrix.
## Usage example; create a random square matrix :
## set.seed(1234) 
## myMat<-matrix(rnorm(9),3)
## Create the special matrix object:
## smo<-makeCacheMatrix(myMat)
## Check your matix:
## smo$get()
## smo$getinv() Oh! That's right you havent't calculate the inverse first:
## cacheSolve(smo)
## Now try the same again:
## cacheSolve(smo)
## You see a message that the inverse is cached! No need to calculate it again!

## This function creates a special "matrix" object that can cache its inverse.
## It containts the following functions and returns them to a list object:
# 1. Set matrix
# 2. Get matix
# 3. Set the inverse matrix
# 4. Get the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        # inv stores the cached inverse matrix
        inv <- NULL
        # Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Get the matrix
        get <- function() x 
        # Set the inverse matrix
        setinv <- function(inverse) inv <<- inverse
        # Get the inverse matrix
        getinv <- function() inv
        # Return a list with the previously defined functions
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Calculates the inverse matrix when not cached, otherwise returns the cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        # Prompt a message if the inverse matrix is cached
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Calculate the inverse matrix if it is not calculated.
        data <- x$get()
        inv <- solve(data, ...)
        # Cache the inverse matrix
        x$setinv(inv)
        # Prompt it
        inv
}
