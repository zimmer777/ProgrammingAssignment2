
## The pair of functions here use the scoping features of R to enable caching of a 
## matrix and its inverse by using the <<- operator.  

## The assignment instructions allow us to assume that the matrix passed in 
## invertible, so we'll skip any checking

## makeCacheMatrix outputs an object that stores the input matrix, x, and its inverse, x_inv
## it uses R's scoping ability to store these values, in particular to cache the inverse
##    Member functions: 
##        set(y) - sets the cached matrix to y
##        get() - returns the currently cached matrix
##        setinverse() - calculate and store the inverse
##        getinverse() - returns the cached inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # the cached matrix has two values in its scope x, the matrix, and x_inv, the inverse of x 
  
  x_inv <- NULL  # initialize x_inv to NULL when we call this
  
  
  set <- function(y) # sets the scoped x to the called value and clears the inverse
  {
    x <<- y
    x_inv <<- NULL
  }
  
  # because the cacheMatrix object only gets changed via the intial call of makeCacheMatrix()
  # or with set, no further checking of whether the matrix changes in needed
  
  get <- function() x  # returns stored matrix, x
  
  
  setinverse <- function(...) # calculate and store the inverse, takes options from solve(x,...)
  { 
    
    x_inv <<- solve(x,...)
    
    # putting the inverse solve here, so that spurious calls to setinverse 
    # dont put junk into x_inv
    # this way, set inverse always makes the inverse of whatever is stored in x
    
  
  }
  
  
  getinverse <- function() x_inv # returns the inverse
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)  # adds some help listing to the output object
  

}


## cachesolve(x,...) returns the cached matrix inverse of the cacheMatrix object x
##      which was created with the makeCacheMatrix() function above 
##      if the inverse ha been computed previously, return the cache and a notice
##      a cached value was used
##      if the cache is empty, calculate the inverse and store it
##      cachesolve also takes all the other options of solve(x,...) 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # Get the stored inverse -- will be NULL if inverse does not exist
  inv <- x$getinverse()

  
  # Check if inverse is NULL and also check if cached matrix is the same as 
  #  the matrix passed to this function
  
  
  if(!is.null(inv))   # if the returned inverse is not NULL, return the cached vlue  
  {
    message("getting cached matrix inverse") # notifiction of cache use
    return(inv)  # return the cached inverse and exit the function
  }

  # if the returned value is NULL, then set the inverse in the cache
  
  # NOTE: this is somewhat different from the example code
  #       in this implementation, the inverse is calculated in the setinverse()
  #       member function of the cached matrix object.
  #       This was done to prevent spurious use of object$setinverse(garbage)
  #       which could corrupt the cache without any other warning
  
  
  x$setinverse(...) # options for solve() passed
  
  # and then return the value that was just computed
  
  x$getinverse()
  
}
