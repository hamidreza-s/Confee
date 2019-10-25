Confee
===

Type safe, prototypal, modular alternative to config files. 
A Confee notation file is compiled to JSON, YAML or other config notation files.

AST Structure
===

- Grammar is the root, Statement, Expression and Node are non-terminal, and Node is terminal
- Grammar is a list of Statements
- Statement can contain another Statement, an Expression or a Node
- Statement has scope and can be defined in top-level (type, conf, import, export)
- Expression can contain another Expression or a Node
- Expression can not be defined in top-level (literal, condition, case)
- Literal Expression can be bool, string, number, array, object or other Expressions
- Node is an abstraction which contains a value or points to it
- Node can be a Token which generated in the lexing phase
- Node can also point to a Statement, an Expression, or another Node

``` 
                                                Grammar
                                                /     \
                                              Stmt  Stmt
                                              /  \     \
                                           Expr  Stmt  Node -> Stmt | Expr | Node
                                           /  \
  Literal | Lambda | Condition | Case <= Expr Node
                                               \\
                                              Token
```
