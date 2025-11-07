# TODO List

## Completed Tasks

- [x] Implement ternary conditional operator (? :)
- [x] Implement type cast operator (type)expr with disambiguation
- [x] String literals output to AST
- [x] Array initialization with string literals (char[] = "string")
- [x] Memory copy operator (Y) for local array initialization
- [x] Struct assignment using COPY operator with block memory copy
- [x] Static local variables emitted in global data section
- [x] Enum implementation - constants in global namespace, variables are unsigned char
- [x] Implement comparison operators in cfold() (<, >, <=, >=, ==, !=)

## Pending Tasks

### Expression Operators

- [ ] Implement increment/decrement operators (++, --) prefix and postfix
- [ ] Implement compound assignment operators (+=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=)

### Type System

- [ ] Add type compatibility checking (sametype function)
- [ ] Implement type conversions and promotions in expressions
- [ ] Fix operator type propagation (currently copies left operand incorrectly)
- [ ] Add type checking validation for all operators
- [ ] Add lvalue validation for assignments and operators


## Future Enhancements

- [ ] Implement pass 2 (code generator)
- [ ] Add more comprehensive type checking
- [ ] Implement remaining C language features
- [ ] Optimize constant folding
- [ ] Add more semantic analysis
