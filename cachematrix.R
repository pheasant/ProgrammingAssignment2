## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

        # The function makeCacheMatrix gets a matrix as a parameter and returns a list with four elements 
        inv<- NULL
        #the first element "set". Here the function saves the values of the matrix
        set<- function(y) {
                x<<-y
                inv<<- NULL
        }
        
        #The second element returns the value of the cached matrix
        get <- function() x
        
        #The third element gives the ivnerse of the matrix by using the function "solve()"
        setinv <- function(solve) inv <<- solve
        #The fourth element returns the inverse of the matrix. 
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)


}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()        #get the matrix cache
        if(!is.null(inv)) {      #if there is a chache
                message("getting cached data") # notify the user about that 
                return(inv)     #and return the cached data 
        }
        data <- x$get()          #otherwise, if there are no cache data for the matrix in question
        inv <- solve(data, ...)  #compute the inverse
        x$setinv(inv)            # save your results 
        inv                      #and return them

        
}
