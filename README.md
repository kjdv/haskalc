# haskalc

Simple command-line calculator, written in Haskell. It's main purpose is to practice and teach myself Haskell.

Main features:
* Simple arithmetic, respecting operator precedence rules:
```
>>> 2 + 3 ^ 4
[1] 83.0
>>> (2 + 3) ^ 4
[2] 625.0
```
* Has basic math constants and functions:
```
>>> pi
[1] 3.141592653589793
>>> e
[2] 2.718281828459045
>>> cos(0)
[3] 1.0
```
* ability to create assign variables and functions:
```
>>> a = 5
[1] 5.0
>>> f(x,y) = x * y
[2] f(x,y)
>>> f(a, 2)
[3] 10.0
```
* easily retrieve previous answers:
```
>>> 1.0
[1] 1.0
>>> ans + 1
[2] 2.0
>>> ans_1 + ans_2
[3] 3.0
```
