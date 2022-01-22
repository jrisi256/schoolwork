isValid <- function(s){

  opens <- c('(','{','[')
  closes <- c(')','}',']')
  names(opens) <- closes # a named vector simulates a "hash table"
  stack <- c()
  svec <- unlist(strsplit(s,"")) # or str_split from tidyverse stringr
  for (bracket in svec){
      if (bracket %in% opens) { # push open bracket to stack
        stack <- c(stack,bracket)
      }
      else if (length(stack)==0) { # closed bracket w no open on stack => INVALID
        return(FALSE)
      }
      else if (stack[length(stack)] == opens[bracket]) { # pop open from stack
        stack <- stack[-length(stack)]
      }
      else { # closed bracket w nonmatch at top of stack => INVALID
        return(FALSE)
      }
}
  return(length(stack)==0) # Any remaining opens on the stack are unmatched
}

 

# There are a couple of things that make this messier in R than in Python

# First, R doesn't have true "hash tables", but you can get similar functionality by naming the elements of a vector. The names are the "keys" and the elements are the values. Here we create a vector where we can "look up" a closing bracket and its value will be the matching open bracket.

# Second, R doesn't have true "stacks" built-in. In particular, there's no "pop" method. You have to do it manually -- e.g., stack <- stack[-length(stack)]

# Third, it's more difficult in R to loop over characters in a string, so we use strsplit to make it into a vector of characters

# Fourth, R doesn't understand something like "if stack" as a check whether the stack is empty or not, so that's done here by the more inefficient checks of "length(stack)"

# So, the if-else-then tree is a little more complex

# First we check if it's an open bracket -- if so, put it on the stack.

# If it's not an open, it's a close.

# So, now (2nd) we check if the stack is empty -- if so, the close bracket comes before any matching open and we can stop -- the string is invalid

# If it's not empty, we (3rd) check whether the top/last element is the correct / matching open bracket. If so, we have a match and remove that open from the stack.

# Otherwise (4th) the open bracket on the stack doesn't match, so s is invalid

# When all of that is done, when we have iterated over the entire string, we check whether the stack is empty.

# If it's empty, we've matched every open with a close => valid string

# If it's not empty, that means we have open brackets that never got matched with a close => invalid string

# Bonus: why do we pop from and push to the *end* of a vector instead of the *beginning*?