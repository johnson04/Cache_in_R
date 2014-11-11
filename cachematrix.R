## The function makeCacheMatrix() take a non-singular matrix as input,
## and assign the matrix and four functions to an "object" as the 
## output. As the default setting, the function makeCacheMatrix() sets
## the inverse of the input matrix as NULL.
##
## The function cacheSolve() take an "object" created by the function
## makeCacheMatrix() as input, and search in the cache for the inverse
## of the matrix stored in the "object". If there is not inverse in 
## the cache, it calculates the inverse, and store in the "object". 
## Last, the function cacheSolve() output the found or calculated 
## inverse of the matrix stored in the input "object".
##
## Example:
##      Generate a 5x5 matrix, then calculate its inverse:
##          A <- matrix(rbinom(25,size=10,prob=0.5),nrow=5,ncol=5)
##          a <- makeCacheMatrix(A)
##          b <- cacheSolve(a)
##
##      Now, we can verify whether we get correct result or not:
##          B <- a$get()
##          BInv <- a$getInv()
##          BInv %*% B 
##          B %*% BInv
##
## Please input an invertible matrix! Otherwise, you will have problem.

makeCacheMatrix <- function(x = matrix()) 
{
    xInv <- NULL
    set <- function(y)
    {
        x <<- y
        xInv <<- NULL
    }
    get <- function() x
    setInv <- function(Inverse) xInv <<- Inverse
    getInv <- function() xInv
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)    
}


## Before calling this function cacheSolve(a), please use the function
## makeCacheMatrix(A) to produce an "object", a, in which an 
## invertible matrix, A, and four functions are stored.

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    xInv <- x$getInv()
    if(!is.null(xInv)) 
    {
        message("getting cached data")
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data, ...)
    x$setInv(xInv)
    xInv
}
