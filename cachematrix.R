## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## function makeCacheMatrix
##
##    arg: x class(x)= "matrix" (default: empty matrix())
##
##    return a list with the following entries  
##         $get()          : function returns matrix x
##         $set(new_x)     : function set x to new_x and x_inv to NULL
##         $getinv         : function returns x_inv if computed NULL otherwise 
##         $setinv(new_inv): function set x_inv to new_inv 
makeCacheMatrix <- function(x = matrix()) {
    # x & x_inv exist only in make CacheMatrix environement
    x_inv <- NULL
    if(!checkMatrix(x)){
        # print warning if there is an issue with the matrix
        warning("Matrix won't be inverted with 'cacheSolve'")
    } 
    set <- function(y) {
        if(!checkMatrix(y)){
            # print warning if there is an issue with the matrix
            # the function still return a list
            # but it is not displayed.
            warning("Matrix won't be inverted with 'cacheSolve'")
        } 
        print("Updating matrix in cache")
        x <<- y
        x_inv <<- NULL
    }
    get <- function() x
    setinv <- function(couldbeanything) x_inv <<- couldbeanything
    getinv <- function() x_inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## Write a short comment describing this function
## cacheSolve:
##
##    Arg 1: xl (class(xl="list"))
##           xl must have be created using makeCacheMatrix
##
##    Returns the inverse of the xl$get()
##          if the inverse of matrix xl$get() was already computed
##            the result is taken form xl
##          otherwise:
##            if the matrix has bad dimensions or na save and return a matrix of NA
##            if the matrix has good dimensions save and return the inverse
cacheSolve <- function(xl, ...) {
    ## Return a matrix that is the inverse of 'x'
    invx <- xl$getinv()
    if(!is.null(invx)){
        # x$setinv(...) has been used 
        # Meaning the inverse of x$get() has already been computed
        # Unless someone used function x$setinv(...) outside this function
        message("getting cached data")
        return(invx)
    }
    xmat <- xl$get()
    # Here I took a little initiative
    # I check whether solve can be applied
    # if yes I run solve 
    if(checkMatrix(xmat)){
        # matrix is good enough to inverse it
        invx <- solve(xmat)
        # save invx into list xl
        xl$setinv(invx)
    } else {
        # create an empty matrix of NA
        # could save it into xl but the NA
        # but reusing cachesolve would not tell me why I don't get anything
        message("Can't invert matrix as is.")
        message("Make sure the matrix in arg is square and contains no NA")
        invx <- NULL
    }
    # return result
    invx
}

# checkMatrix:
#    arg: x class(x)= "matrix" (default: empty matrix())
# return TRUE if the argument is a square matrix of dim > c(0,0) with no NA
# return FALSE otherwise
# if I did this corrctly, when solve is applyed to the matrix
# an error message would appear only if the entry cannot be inverted
checkMatrix <- function(x=matrix()){
    # check class of the argument
    if(class(x)!="matrix"){
        message("argument is not a matrix")
        message(paste("found a ",class(x)))
        return(FALSE)
    }
    # check that the matrix is square
    if(nrow(x)!=ncol(x)){
        message("matrix is not square")
        message(paste("nrow:",as.character(nrow(x))))
        message(paste("ncol:",as.character(ncol(x))))
        return(FALSE)
    }
    # check that matrix has dimension > 0
    if(nrow(x)<=0){
        message("Matrix in has dimension 0")
        return(FALSE)
    }
    # check for na 
    if(sum(is.na(x))!=0){w
        message(paste("Matrix contains",as.character(sum(is.na(x))),"NAs"))
        return(FALSE)
    } 
    # Since we are here all tests were passed 
    # return TRUE
    TRUE
}