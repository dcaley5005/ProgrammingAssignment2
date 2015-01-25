

## Establishes a function to define x as a matrix
makeCacheMatrix <- function(x = matrix()) {

## defines m as a NULL value in the global environment.  
  m <- NULL
  
## Defines set as a function of y.
  set <- function(y) {

## replaces x with y in the global environment.
    x <<- y

## m is defined as a special assignment as a NULL value
    m <<- NULL
 
  }

## Gets the value of x
  get <- function() x

## calculates for the inverse of x 
  setinverse <- function(x) 
    {
    
## solves for x by replacing m in the parent evironment with the inverse of a matrix
    m <<- solve(x)
  }

## gets the calculated inverse value from the parent vaule
  getinverse <- function() m

## establishes a list of the following elements.
      ## set, get, setinverse, getinverse.

  list(set = set, get = get,
       setinverse = setinverse,
       getinverse= getinverse)
}



## Write a short comment describing this function

## creates x as vector calling the functions and variables assigned in makeCacheMatrix 
cacheSolve <- function(x, ...) {

## Assigns m as a function of x$getinverse
        m <- x$getinverse()
        
## if null then m is returned with a message "getting cached data"
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }

## Assigns data as a function of x$get which get was established in the makeCacheMatrix.
## Then call m which was established in the makeCacheMatrix and calculates the inverse of the data.
        data <- x$get()
        m <- solve(data, ...)


## updates the value of m in the makeCacheMatrix 
        x$setinverse(m)

## Returns the value of m
        m
        ## Return a matrix that is the inverse of 'x'
}


