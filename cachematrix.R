## Functions that cache the inverse of a matrix
##
## Usage example:
##
## > source('cachematrix.R')
## > g <- matrix(c(5,0,0,0,5,0,0,0,5), 3, 3)
## > m <- makeCacheMatrix(g)
## > cacheSolve(m)
##      [,1] [,2]  [,3]
## [1,]  0.2  0.0   0.0
## [2,]  0.0  0.2   0.0
## [3,]  0.0  0.0   0.2
## Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

##makeCacheMatrix is a function that returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
        
        #nothing is cached initially, so set it to NULL
        cache <- NULL
        
        #Store a matrix
        set <- function(newmatrix) {
                x <<- newmatrix
                #since its a new matrix, flush out the old cache
                cache <<- NULL
        }
        
        #returns stored matrix
        get <- function() x
        
        #cache the given argument
        setinverse <- function(inverse) cache <<- inverse
        
        #get the cached value
        getinverse <- function() cache
        
        #returns a list of all the functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

## Calculate the inverse of the special "matrix" created with the above
## function, reusing cached result if it is available

cacheSolve <- function(x, ...) {
        
        #get cached value
        inverse <- x$getinverse()
        
        #check if there is cache value, if yes, then return
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        
        #return the inverse
        inverse
}
