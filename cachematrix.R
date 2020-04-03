rm(list=ls()) # Clears the working space
## MakeCachematrix is a  function which creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x=matrix()){ ## passing null matix as an argument.
  inv <- NULL                            ## Assigning null value to inverse      
  set <- function(y){                    ## A function set to set value of matrixis created with argument y. 
    x <<- y                              ## Assigning value of y in parent environment (x).
    inv <<- NULL                         ## Assigning null value to inverse 
  }
  get <- function() x                    ## function 'get' access value of x in parent environment.
  setinverse <- function(matinv) inv <<- matinv  ## This function receives value from Cachesolve function and assigns it to 'inv' variable in parent environment. 
  getinverse <- function() inv                   ## This function access value of inv in parent environment. 
  list(set = set,get = get,setinverse = setinverse,getinverse = getinverse)   ##this is created in order to access objects in makeCacheMatrix with $. 
}
cacheSolve <- function(x, ...){ ## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
                                ##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  matinv <- x$getinverse() ## access value in 'getinverse' and stores in matinv
  if (!is.null(matinv)){   ## checks whether matinv is null or not. 
    print("getting cached inverse") ## if matinv is not null, prints this message.
    return (matinv)                ## return matinv and prints it
  }
  data <- x$get()         ## if matinv is null, it access value of x from get and assigns to 'data' variable. 
  matinv <- solve(data, ...) ## Calculates inverse using solve function
  x$setinverse(matinv)       ## returns the matinv in setinverse function 
  matinv                     ## print matinv 
}

