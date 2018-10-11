# Projet: R Programming Coursera, Week 3 Assignment: Lexical Scoping
# Author: Nic Preuss, preuss.nicolas@gmail.com
# creation of functions to create a square matrix and storing the inverse in the
# cache



###This function creates 4 sub functions to set and get a (c++ constructor style)
### a square matrix along with getSolve and setSolve as Solve(X) aka intverted matrix

makeCacheMatrix <- function(x = matrix()) {

        invertedMatrix <- NULL
        set <-function(y){
                x<<-y
                invertedMatrix<<-NULL
        }
        get <- function() x
        setInverted <- function(inverted) invertedMatrix<<-inverted
        getInverted <- function() invertedMatrix
        list(set=set, get=get,
             setInverted=setInverted,
             getInverted=getInverted)
}


## this function will either return the inveted matrix stored in the cache
##or compute the solve and store it in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverted <-x$getInverted()
        if(!is.null(inverted)){
                message("getting cached Inverted Matrix")
                return(inverted)
        }
        the_matrix <- x$get()
        inverted <- solve(the_matrix)
        x$setInverted(inverted)
        inverted
}
