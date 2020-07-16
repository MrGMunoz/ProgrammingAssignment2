## Following example, a I made the function taking 'x' as a 2-dim 4x4 matrix
## Changed 'mean' names to 'Inversed' to keep how function works as clear as posible
makeCacheMatrix <- function(x = matrix(sample(100:1000 , 9) , nrow = 4 , ncol = 4)) {      
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInversed <- function(solve) m <<- solve
        getInversed <- function() m
        list(
                set = set, 
                get = get,
                setInversed = setInversed,
                getInversed = getInversed)
}

## I continued following the example. I changed 'data' for 'receivedMatrix'.
cacheSolve <- function(x, ...) {
        ## next line calls getInversed function nested inside makeCacheMatrix function
        m <- x$getInversed()
        if(!is.null(m)) {
                message("full matrix received")
                return(m)
        }
        receivedMatrix <- x$get()
        m <- solve(receivedMatrix, ...)
        x$setInversed(m)
        return m
}
