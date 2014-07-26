## The functions below implements a way to save computing resources by implementing a caching strategy, 
## when computing inverse of matrices. The implementation is done by creating a special matrix type called
## 'makeCacheMatrix' that acts as wrapper for a given matrix and its inverse.
##
## The other function cacheSolve that that actually takes in this special matrix and computes its inverse and stores
## inside the special matrix. If the inverse matrix already is computed and stored, on subsequent function calls 
## the function simply returns the pre-computed inverse matrix.


## This function constructs a wrapper around standard matrix object, to hold the given matrix and its inverse matrix.
## The function provides sub functions that enable caller to set and get original matrix and also set and get cached
## Inverse of the matrix.
## N.B; 1)This method does not contain any validation logic to determine if the inverse matrix set from the caller
##      is indeed a valid inverse. The onus is on the caller of this function to maintain those validation rules.
##      2)This function errors if the passed in  matrix is null or if its not a matrix type.
makeCacheMatrix <- function(pMatrixObject = matrix()) {
        
        #check if the passed in object is a valid matrix
        if(!is.null(pMatrixObject) && class(pMatrixObject)=="matrix"){
                
                #initialixe a varaible that holds cached result
                cachedInverseMatrix <- NULL
                
                #clear the cached inverse matrix when a new matrix is set
                set <- function(y) {
                        if(!is.null(y) && class(y)=="matrix"){
                                x <<- y
                                cachedInverseMatrix <<- NULL
                        }else{
                                stop("passed in matrix parameter is invalid or null. Original values contained are untouched");  
                        }
                }
                
                #gets the original matrix object
                get <- function() {
                        pMatrixObject      
                } 
                
                #sets the precalculated inverse matrix to a cahced variable
                setInverseMatrix <- function(pInverseMatrix) {
                        cachedInverseMatrix <<- pInverseMatrix
                }
                
                #gets the cached instance of inverse matrix
                getInverseMatrix <- function() {
                        cachedInverseMatrix
                }
                
                #getters and setters for matrix and its cached instances
                list(set = set, get = get,
                     setInverseMatrix = setInverseMatrix,
                     getInverseMatrix = getInverseMatrix,                     
                     funcType = "makeCacheMatrix")                
        }else{ 
                #stop when passed in matrix is invalid
                stop("passed in matrix parameter is invalid or null"); 
        }        
}


## This function takes in an argument of type 'makeCacheMatrix' a special matrix and returns the pre-computed and cached inverse
## matrix if exists contained in the special matrix. If there is no existing cached instance, this function computes and returns the inverse matrix after storing
## the inverse matrix in the special matrix.
cacheSolve <- function(x, ...) {
        # check if the type of x is of special matrix type 'makeCacheMatrix'
        if(!is.null(x) && !is.atomic(x) &&x$funcType=="makeCacheMatrix"){
                # check if pre-computed inverse already is cached
                inverseMatrix <- x$getInverseMatrix()
                if(!is.null(inverseMatrix)) {
                        message("getting cached data")
                        return(inverseMatrix)
                }
                #if the cached object is null, compute, set and return inverse matrix
                matrixObject <- x$get()
                inverseMatrix <- solve(matrixObject, ...)
                x$setInverseMatrix(inverseMatrix)
                inverseMatrix   
        }else{
              stop("Expected parameter: object of type 'makeCacheMatrix'")  
        }
        
        
}
