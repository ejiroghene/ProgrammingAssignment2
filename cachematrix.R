#The makeCacheMatrix function:
#makeCacheMatrix is a function which creates a special "matrix" object that can cache its inverse.
#Other functions are created in the working environment that can be used by catcheSolve to 
#get or set the inverted matrix in cache. 


#The cacheSolve function:
#cacheSolve is a function which computes/calculates the inverse of the special "matrix" 
#created by the makeCacheMatrix function. If the inverse of the matrix has already been 
#calculated, cachesolve would retrieve the inverse from the cache, but if the inverted 
#matrix does not exist in the cache, it will be created in the working environment 
#and it's inverted value is stored in cache.




#makeCacheMatrix creates a matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
              c <- NULL          #creates an empty placeholder for future value of cache,c
                                 #and sets cache to NULL.
              
              set <- function(y){
                x <<- y          

                c <<- NULL       
              }                  #the set function creates the matrix in the working environment and
                                 #resets the cache to NULL.
              
              get <- function()x  #this returns the value of the matrix, x
              
              setInvertMatrix <- function(inverse)      #this inverts the matrix
                c <<- inverse                           # and stores it in cache,c.
              
              getInvertMatrix <- function()c            #this returns the inverted matrix from cache,c.
              
              list(set = set, get = get, setInvertMatrix = setInvertMatrix, getInvertMatrix = getInvertMatrix)
              #this  returns  all of the functions defined within this working environment.
              
              
}






#cacheSolve is a function which computes the inverse of the "special matrix" 
#created by the makeCacheMatrix function.



cacheSolve <- function(x, ...) {
  
  
          c <- x$getInvertMatrix()  #first checks to see if the inverse
                                    # of the matrix, x, is in c(i.e cache).
                                      
          if(!is.null(c)) {
            
            
            message("getting cached data")
            return(c)           #the "if" conditional statement implies that
                                #if the cache, c, is not NULL(ie if it contains a matrix), then print
                                # a message, and then return the value of c, which is the inverse of x.
                  
          }
            
          else{
            
            
            matrix <- x$get()     # creates a matrix  if does not exist in the cache,c.
            
            c <- solve(matrix,...) # sets and returns the inverse of the matrix using the solve() function.
            
            
            x$setMatrix(c)        # sets the inverted matrix in cache
            
            
          return(c)              #returns the value of c, which is the  inverse of x.  
          
          }
            
                    
  
}
