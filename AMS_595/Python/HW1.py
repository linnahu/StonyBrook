# -*- coding: utf-8 -*-
"""
Created on Thu Oct  5 20:12:12 2017

@author: 胡琳那
"""

import math
    
#1. Conditional Statements
x = int(input('Enter a number: '))
if x==-1:
    print('negative one')
elif x==0:
    print('zero')
elif x==1:
    print('positive one')
else:
    print('other value')



# 2. while loops
    n=[1,2]
    i=0;
    while n[i]+n[i+1]< 4000000:
        n.append(n[i]+n[i+1])
        i=i+1
 print(n)
    


#   3. List Comprehension
    n=[]
    for x in range(100,1000):
        for y in range(100,1000):
            if str(x*y) == str(y*x)[::-1]:
                n.append(x*y)
                palindrome = [n]
    print(palindrome)
    
    
    #  4. Functions
#    2a. (10 points) Write a function that determines if a number is prime.
    def isprime(n):
        if n ==1:
            return False
        p=2
        while p<n:
            if (n%p) == 0:
                return False
            p=p+1
        return True
# test codes
print(isprime(2))
print(isprime(10))
print(isprime(17))


#   2b. (10 points) Write a function that finds all the factors of a number.
def factorize(n):
    answer = []
    for i in range(1, n + 1):
       if n % i == 0:
           answer.append(i)
       i = i+1
    return answer
# test codes
print(factorize(2))
print(factorize(72))
print(factorize(196))

#    2c. (10 points) Write a function that finds the prime factorization of a number.
def prime_factorize(n):
    answer = []
    i=2
    while i<=n:
        if n % i == 0:
                answer.append(i)
                n= n//i
        else:
            i = i+1
    return answer
# test codes
print(prime_factorize(2))
print(prime_factorize(72))
print(prime_factorize(196))


#5. Recursive Functions
#(20 points) The following function uses recursion to generate the nth row of Pascal's triangle:
#              1
#           1     1
#        1     2     1
#      1    3     3     1
#   1    4     6     4    1
#1     5    10    10    5    1

def pascal(n):
    k=1
    line = [1]
    while k<n:
        line = [ line[i]+ line[i+1] for i in range(len(line)-1)]
        line.insert(0,1)
        line.append(1)
        k = k+1
    return line

print(pascal(6))
    
    
    
    
    