import numpy as np
import pandas as pd

my_string = "hello world"
print(my_string)
print(my_string[0:5])

my_list = [1, 2, 3]
my_list.append(1)
my_list = my_list + [5]
print(my_list)

print("The second element")
print(my_list[1])
print("\n")

print("The second element as a list")
print(my_list[1:2])
print("\n")

print("The second and third elements")
print(my_list[1:3])
print("\n")

for elem in my_list:
    print(elem)

    if elem == 1:
        print("You are number 1")
    else:
        print("You are not number 1")

my_dict = {1:"yes", 2:"yes", 3:"yes", 4:"no"}
print(my_dict[4])

a = np.array([1,2,3])
print("\n")
print("This is a numpy array")
print(a)