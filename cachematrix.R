## Caching the Inverse of a Matrix
### Variables: x,y: matrix objects
### CacInv: Cached Inverse
### CalInv: Calculated Inverse

## (i)          makeCacheMatrix: creates a special "matrix" object 
##              that can cache its inverse

## (ii)         set: re-initializes matrix object by
##              setting a different matrix value and setting CacInv NULL
## (iii)        get: Gets the value of matrix object
## (iv)         setInv: Sets CalInv as CacInv
## (v)          getInv: Gets the current CacInv value
## (vi)         cacheSolve: Computes the inverse of the special "matrix" 
##              returned by makeCacheMatrix above. If the inverse has 
##              already been calculated (and the matrix has not changed),
##              then the cachesolve will retrieve the inverse from the cache

makeCacheMatrix <- function( x = matrix() ) {
        CacInv <- NULL
        set <- function( y ) {
                x <<- y
                CacInv <<- NULL
        } 
        get <- function() { x }
        setInv <- function( CalInv ) {
                CacInv <<- CalInv
        }
        getInv <- function() { CacInv }
        list( set = set , get = get , setInv = setInv , getInv = getInv )
}

cacheSolve <- function( x = list() ) {
        Inv <- x$getInv()
        if( !is.null( Inv ) ) {
                message("Getting cached Inverse")
                return( Inv )
        }       ## In case the matrix values are modified, 
                ## the inverse it set to NULL automatically by the set() function
        mat <- x$get()
        Inv <- solve( mat )
        x$setInv( Inv )
        Inv
}
## The code was run and the outputs were as expected