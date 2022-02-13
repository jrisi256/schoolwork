squareRoot <- function(target)
{
    searchVector <- 0:target
    candidate <- -1
    oldDistance <- Inf
    newDistance <- -1
    
    left <- 1
    right <- length(searchVector)
    
    while(left <= right)
    {
        mid <- (left + right) %/% 2
        
        if(searchVector[mid] ^ 2 == target) {return(searchVector[mid])}
        
        # If the current item squared is less than the target
        else if(searchVector[mid] ^ 2 < target)
        {
            left <- mid + 1
            newDistance <- target - searchVector[mid] ^ 2
            
            # if it's closer to our target than the previous candidate, it becomes the new candidate
            if(newDistance < oldDistance)
            {
                oldDistance <- newDistance
                candidate <- searchVector[mid]
            }
        }
        
        # If the current item squared is more than the target, it cannot be the square root.
        else if(searchVector[mid] ^ 2 > target) {right <- mid - 1}
    }
    return(candidate)
}

test <- lapply(0:5000, function(x) {squareRoot(x) == floor(sqrt(x))})
all(unlist(test))
