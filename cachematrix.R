## This function creates a List object that can contain
## a matrix and its cacheable inverse
makeCacheMatrix <- function(x = matrix()) {
    
    mat_inverse <- NULL
    set <- function(y) {
        x           <<- y
        mat_inverse <<- NULL
    }
    get <- function() x
    set_matinverse <- function(matinverse) mat_inverse <<- matinverse
    get_matinverse <- function() mat_inverse
    list(set = set, get = get,
         set_matinverse = set_matinverse,
         get_matinverse = get_matinverse)

}


## This function computes the inverse of the matrix list element created by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then cacheSolve should retrieve the inverse
## from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    mat_inverse <- x$get_matinverse()
    if(!is.null(mat_inverse)) {
        message("getting cached data")
        return(mat_inverse)
    }
    data <- x$get()
    minverse <- solve(data)
    x$set_matinverse(minverse)
    minverse
}
