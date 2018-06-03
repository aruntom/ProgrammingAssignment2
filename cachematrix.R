#R programming - coursera

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv<-NULL
        set<- function(y) { # setting data to x and inverse to null
                x<<-y
                inv<<-NULL
        }
        get <- function() x # getting data
        
        setinv <- function(inverse) inv <<-inverse # setting inverse to inv variable
        getinv <- function() inv # getting inverse
        
        list(set=set, get=get, setinv=setinv,getinv=getinv) # special matrix object with functions to cache inverse
}


## cacheSolve function calculates the mean of the special "vector" 
# created with the above function. 

## It first checks to see if the mean has already been calculated.

## If so, it gets the mean from the cache and skips the computation.

## Otherwise, it calculates the mean of the data and sets the value 
# of the mean in the cache via the setmean function.



cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv<-x$getinv # getting cached inverse
        if(!is.null(inv)) { # checking cache inverse matrix empty or not
                message("getting cached inverse")
                return(inv) # returning cached inverse if nnot empty
        }
        data<-x$get() # getting matrix
        inv<-solve(data) # calculating inverse
        x$setinv(inv) # setting inverse martrix
        inv #returning the inverse
}
