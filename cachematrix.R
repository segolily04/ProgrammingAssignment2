## This project is to allow a large matrix to be cached (set) and retrievable (get)
## using the makeCacheMatrix. The cached matrix will then be accessible to the 
## cacheSolve to return the inverse of the matrix. 

## This function will allow a cached matrix to be settable and gettable
## param: x is an invertable matrix
## return: a list containing functions to
##          1. set the matrix
##          2. get the matrix
##          3. set the inverse
##          4. get the inverse
##         the list is used as the input to cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
    invertedX <- NULL
    
    set <- function(y){
        x <<- y
        invertedX <<- NULL
    } 
    
    get <- function(){ 
        x
    }
    
    setInv <- function(inverse){
        invertedX <<- inverse
    }
    
    getInv <- function(){
        invertedX
    }
    
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## This function will return the inverse of the cached matrix
## set by makeCacheMatrix
## param: x output (list of functions) of makeCacheMatrix()
## return: inverse of the original matrix input to makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    invertedX <- x$getInv()
    
    #check to see if the inverse has already been calculated
    if(!is.null(invertedX)){
        #return the cached inverse and end 
        message("getting cached data")
        return(invertedX)
    }
    
    #Calculate the inverse of x
    mData <- x$get()
    invertedX <- solve(mData, ...)
    
    #set the value of the invers in cache using setInv function
    x$setInv(invertedX)
    
    #return the inversed matrix
    return(invertedX)
    
}
