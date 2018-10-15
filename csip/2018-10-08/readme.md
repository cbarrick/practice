# RPN Calculator

## The Problem
Evaluate arithmetic formulas in postfix notation,
also known as *reverse polish notation* or *RPN*.

You must support the following operations:
- Addition: +
- Subtraction: -
- Multiplication: *
- Division: /

## Examples:
Postfix           | Infix                   | Value
------------------|-------------------------|-------
1                 | 1                       | 1
1 2 +             | (1 + 2)                 | 3
1 2 + 3 *         | ((1 + 2) * 3)           | 9
1 2 + 3 4 + /     | ((1 + 2) / (3 + 4))     | 3/7 = 0.4286
1 2 * 3 - 4 5 + / | ((1 * 2) - 3) / (4 + 5) |


## Hints:
Unlike infix notation, postfix notation never requires
parenthesis or an order of operations.
