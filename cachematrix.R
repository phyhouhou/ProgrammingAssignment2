##Cache the inverse of a matrix
##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than compute it repeatedly.
##Below are a pair of functions that store a matrix and cache its inverse.

## This makeCacheMatrix function creates a special "matrix" object that can
##cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        matrix_inverse<-NULL
        set<-function(y){
                x<<-y
                matrix_inverse<<-NULL
        }
        get<-function() x
        setInverse<-function(inverse) matrix_inverse<<-inverse
        getInverse<-function() matrix_inverse
        list(set=set,get=get,setInverse=setInverse,
             getInverse=getInverse)
}


## The cacheSolve function calculates the inverse of the 'matrix' produced by
##makeCacheMatrix function above. If the inverse has been calculated and the 
##matrix has not changed, it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        matrix_inverse<-x$getInverse()
        if(!is.null(matrix_inverse)){
                message('getting cached data')
                return(matrix_inverse)
        }
        mdat<-x$get()
        matrix_inverse<-solve(mdat,...)
        x$setInverse(matrix_inverse)
        matrix_inverse
}
