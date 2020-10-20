Confee
===

Type safe, prototypal, modular alternative to config files. 
A Confee notation file is compiled to JSON, YAML or other config notation files.

AST Structure
===

- Grammar is the root, Statement and Expression are non-terminal, Node is terminal
- Grammar is a list of Statements
- Statement can contain another Statement, an Expression or a Node
- Statement has scope and can be defined in top-level (type, conf, import)
- Expression can contain another Expression or a Node
- Expression can not be defined in top-level (literal, condition, case)
- Literal Expression can be bool, string, number, array, object, proto or other Expressions
- Node is an abstraction which contains a value or points to it
- Node can be a Token which generated in the lexing phase
- Node can also point to a Statement, an Expression, or another Node

``` 
                                                Grammar
                                                /     \
                                              Stmt  Stmt
                                              /  \      \
                                           Expr  Stmt   Node -> Stmt | Expr | Node
                                           /  \
  Literal | Lambda | Condition | Case <= Expr Node
                                                \\
                                                Token
```

Context-Free Grammar (BNF)
===

The context-free grammar of Confee contains data types we have in most common config file notations
such as **Bool**, **String**, **Number**, **Array** and **Object**. 
It also contains a special data type called *Proto* which is a clone of an object letting us 
override its fields.

The **Type** statement is used to define the types of a **Conf** statement, and they both can be
imported to other Confee file using **Import** statement.

```
<grammar> ::= <stmt> <grammar>
            | <stmt>
            
<stmt> ::= <typeStmt>
         | <confStmt>
         | <importStmt>
         
<typeStmt> ::= <typeKeyword> <name> <braceOpen> <typeStmtItems> <braceClose>

<typeStmtItems> ::= <typeStmtItem> <typeStmtItems>
                  | <typeStmtItem>
                  
<typeStmtItem> ::= <word> <colon> <name>
                 | <word> <colon> <bracketOpen> <name> <bracketClose>

<confStmt> ::= <confKeyword> <word> <colon> <confStmtType> <braceOpen> <confStmtItems> <braceClose>

<confStmtItems> ::= <confStmtItem> <confStmtItems>
                  | <confStmtItem>
             
<confStmtItem> ::= <word> <assignment> <expr>

<confStmtType> ::= <word>
                 | <name>
                 
<importStmt> ::= <importKeyword> <string>

<expr> ::= <exprLiteral>

<exprLiteral> ::= <exprLiteralBool>
                | <exprLiteralString>
                | <exprLiteralNumber>
                | <exprLiteralArray>
                | <exprLiteralObject>
                | <exprLiteralProto>
                
<exprLiteralBool> ::= <trueBool>
                    | <falseBool>
                    
<exprLiteralString> ::= <exprLiteralStringFactor> <exprLiteralStringOperator> <exprLiteral>
                      | <exprLiteralStringFactor> <exprLiteralStringOperator> <exprLiteralStringFactor>
                      | <exprLiteralStringFactor>
                      
<exprLiteralStringFactor> ::= <parenthesesOpen> <exprLiteralString> <parenthesesClose>
                            | <string>
                            | word
                            
<exprLiteralStringOperator> ::= <addition> | <subtraction>

<exprLiteralNumber> ::= <exprLiteralNumberFactor> <exprLiteralNumberOperator> <exprLiteralNumber>
                      | <exprLiteralNumberFactor> <exprLiteralNumberOperator> <exprLiteralNumberFactor>
                      | <exprLiteralNumberFactor>
                      
<exprLiteralNumberFactor> ::= <parenthesesOpen> <exprLiteralNumber> <parenthesesClose>
                            | <number>
                            | <word>
                            
<exprLiteralNumberOperator> ::= <addition>
                              | <subtraction>
                              | <division>
                              | <multiplication>
                              | <modulus>
                              
<exprLiteralArray> ::= <bracketOpen> <exprLiteralArrayItems> <bracketClose>

<exprLiteralArrayItems> ::= <exprLiteralArrayItem> <separator> <exprLiteralArrayItems>
                          | <exprLiteralArrayItem>
                          
<exprLiteralArrayItem> ::= <exprLiteral>

<exprLiteralObject> ::= <braceOpen> <exprLiteralObjectItems> <braceClose>

<exprLiteralObjectItems> ::= <exprLiteralObjectItem> <exprLiteralObjectItems>
                           | <exprLiteralObjectItem>
                           
<exprLiteralProto> ::= <word> <exprLiteralObject> 
```

Example
===

Here is an example to define data pipelines to be used as the `data-info.yaml` for Luigi.

The types can be used in a global space to be used by every data pipeline project:
```
type DataInfo {
     id: Text
     description: Text
     facts: DataFact
}

type DataFact {
     doc: Text
     workFlows: [DataWorkflow]
}

type DataWorkflow {
     id: Text
     order: Number
     account: Text
     schedule: Text
     dockerArgs: [String]
}
```

Then by importing the types and then defining the proto, we can define the configs:

```
import "/path/to/DataInfoTypes.confee"

conf workflow : DataWorkflow {
     order = 10
     account = "admin@dataflow.com"
     dockerArgs = ["--wrap-luigi", "--development"]
}

conf dataInfo : DataInfo {
     id = "e73d6402"
     description = "sample desc"
     facts = {
          doc = "sample doc"
          workFlows = [
               workflow { id = "a1dc6109" order = order + 1 schedule = "monthly" },
               workflow { id = "320a0de1" order = order + 2 schedule = "monthly" },
               workflow { id = "ac62a310" order = order + 3 schedule = "daily" },
               workflow { id = "68b703f8" order = order + 4 schedule = "daily" }
          ]
     }
}
```

Copyright
===

```
Copyright (c) 2019 Spotify AB
```
