- handle empty expressions
- Allow expressions such as `a && !b` without having to add parentheses like `a && (!b)`
- Throw exception for invalid expressions such as `a b`
- Implement operator precedence to allow `a && b || c`
- Allow full if-else statements