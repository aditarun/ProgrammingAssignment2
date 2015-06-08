## Computing the inverse of is an expensive especially when use it repeatedly.
## We may use the concept of caching to save this computational resourse.
##Two functions are written here to implement caching the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.
##When initiated it does not contain the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {x}
    
    set_inv <- function(mat_inv) {inv <<- mat_inv}
    
    get_inv <- function() {inv}
    
    list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)

}


## This function computes the inverse of the special "matrix" returned by
##`makeCacheMatrix` above.If the inverse has already been calculated 
##(and the matrix has not changed), then `cacheSolve` retrieves the 
##inverse from the cache.It return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$get_inv()
        if(!is.null(inv))
        {
            message("getting inverse from cache")
            return(inv)
        }
        
        temp <- x$get()
        message("computing")
        inv <- solve(temp, ...)
        x$set_inv(inv)
        
        inv
}
