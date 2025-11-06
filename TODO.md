# TODO List

## Completed Tasks

- [x] Implement ternary conditional operator (? :)
- [x] Implement type cast operator (type)expr with disambiguation
- [x] String literals output to AST
- [x] Array initialization with string literals (char[] = "string")

## Pending Tasks

### Expression Operators

- [ ] Implement comparison operators in cfold() (<, >, <=, >=, ==, !=)
- [ ] Implement increment/decrement operators (++, --) prefix and postfix
- [ ] Implement compound assignment operators (+=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)

### Type System

- [ ] Add type compatibility checking (sametype function)
- [ ] Implement type conversions and promotions in expressions
- [ ] Fix operator type propagation (currently copies left operand incorrectly)
- [ ] Add type checking validation for all operators
- [ ] Add lvalue validation for assignments and operators

### String Handling

- [ ] Simplify local char array initialization: `char ca[] = "s2"` should create ca as a constant char* pointer to the string literal (s2 is the array, ca is just a pointer)

## Future Enhancements

- [ ] Implement pass 2 (code generator)
- [ ] Add more comprehensive type checking
- [ ] Implement remaining C language features
- [ ] Optimize constant folding
- [ ] Add more semantic analysis
