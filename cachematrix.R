##Creating a list with an in memory stored inverse and original matrix
makeCacheMatrix <- function(mOriginal = matrix()) {
  
  ##Inverse is initially unknown, therefore it is NULL.
  mI            <- NULL
    
  ##After the original matrix is set, the Inverse should be reset to 0 to not calculate with a wrongly cached inverse.
  set <- function (y)
  {    
    mOriginal   <<- y
    mI          <<- NULL
  }
  
  ##get returns the original matrix.
  get         <- function()           mOriginal
  ##set sets the inverse matrix in memory, this should initially be calculated outside this function.
  setInverse  <- function(mInverse)   mI <<- mInverse
  
  ##after the inverse has ben calculated and set, GetInverse returns the in memory inverse.
  getInverse  <- function()           mI
  
  ##return value is a list which contains all functions.
  list(
      set         = set, 
      get         = get,
      setInverse  = setInverse,
      getInverse  = getInverse
  )  
}


##returns the inverse of a matrix and stores it in memory. 
##If the inverse has allready been calculated it returns the in memory inverse matrix.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mInverse <- x$getInverse()
  
  ##If Inverse is found in memory, return the in memory inverse.
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  
  ##if the inverse is not founc in memory, calculate the inverse and set it in memory.
  mOriginal     <-  x$get()
  mInverse      <-  solve(mOriginal)
  x$setInverse(mInverse)
  
  ##return the inverse.
  mInverse
}
