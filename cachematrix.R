## Example functions which show how to store 
## potentially computation intensive procedures
## into local function environment for quick
## retrieval


## Function used to make a list of functions which
## invert a matrix and cache the results
makeCacheMatrix <- function(x = matrix()) 
{
        inv = NULL
        set = function(y) 
		{
                x <<- y
                inv <<- NULL
        }
		#function used to get matrix
        get = function() x
		#function used to set matrix
        setinv = function(inverse) inv <<- inverse 
        getinv = function() inv
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## function that calculates the inverse of a matrix 
## using previously cached value if available which speeds
## execution
cacheSolve <- function(x, ...) 
{
        inv = x$getinv()
        # if the inverse has already been calculated
        if (!is.null(inv))
		{
                # return from function environment (cache) 
				# and skip re-computation. 
                return(inv)
        }
        
        # else, calculate inverse and store in cache before returning
		# value
        mat.data = x$get()
        inv = solve(mat.data, ...)
        
        # set the value of the inverse matrix into function (cache) environment
        x$setinv(inv)
        
        return(inv)
}
