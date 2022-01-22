def isValid(s):
    pairs = {')':'(', '}':'{', ']':'['}
    stack = []
    for bracket in s:
        if stack and (bracket in pairs and stack[-1] == pairs[bracket]):
            stack.pop()
        else:
            stack.append(bracket)
    return not stack

 

# define pairs as a dictionary / "hash table" with closing brackets as keys and matching open brackets as values. You "look up", e.g., ")" and it returns "("

# define stack as an empty list

# iterate over the characters in s

# if the stack isn't empty AND the current character is a closed bracket AND the last/top character of the stack is the matching open bracket ... you have matched the correct open bracket so "pop" the top/last character of the stack (remove it)

# otherwise, add the bracket to the stack. This will either be an open bracket, which can potentially be matched and removed by a subsequent closing bracket, or it will be an unmatched close bracket, which will stay on the stack permanently.

# at the end, return "true" if the stack is empty (everything you added to the stack found a match in the right order) and return "false" if the stack still has anything on it.