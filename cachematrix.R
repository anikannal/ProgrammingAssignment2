## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to 
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse
## 4.get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    
    ## Set the value for x. Either override existing value or set anew.
    set <- function(y) {
      x <<- y
      i <<- NULL
    }
    
    ## Return the value for x.
    get <- function() x
    
    ## Cache the inverse of inverse of x.
    setinverse <- function(inverse)  i <<- inverse
    
    ## Return the inverse of x.
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## The second function calculates the inverse of the special "matrix" created with the above function.
## It first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

  i <- x$getinverse()
    
    ## Return cached data if inverse already calculated.
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    
    ## Calculate the inverse if not already cached.
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
  
}
