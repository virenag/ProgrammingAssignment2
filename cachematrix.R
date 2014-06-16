
# makeCacheMatrix: return a list of functions to:
# 1. Set function sets value of the matrix variable
# 2. Get function returns the value of the matrix variable
# 3. Seti function sets the matrix inverse variable
# 4. Geti function returns the value of inverse matrix variable

makeCacheMatrix <- function(x = matrix()) {
        # Initialize matrix inverse to NULL and save in variable inv 
        inv <- NULL
        
        # Setter for the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # Getter for the matrix
        get <- function() x
        
        # Setter for the inverse
        seti <- function(inverse) inv <<- inverse
        # Getter for the inverse
        geti <- function() inv
        
        # Return list of matrix functions
        list(set = set, get = get, seti = seti, geti = geti)
}



# cacheSolve: returns inverse of the matrix passed.
#             If the inverse is already cached, returns cached value
#             else calculates inverse and returns it

cacheSolve <- function(x, ...) {
        ## Get inverse of matrix x from cache
        inv <- x$geti()
        
        if (!is.null(inv)) {
                # Inverse exists in Cache , return cached value.       
                return(inv)
        } else {
                # get Matrix data
                data <- x$get()
                # Calculate inverse 
                inv <- solve(data, ...)
                # save the inverse in cache
                x$seti(inv)
                # return calculated matrix inverse
                return(inv)
        }
}
