Confee
===

Type safe, prototypal, modular alternative to config files. 
A Confee notation file is compiled to JSON, YAML or other config notation files.

Core features
==

In confee, every config item value is an expression which means it is evaluated and then returns 
something of its type. Each config item value can also be referenced in other item values in the 
same scope.

- **Bool**: It can be `true` or `false` and also can reference other booleans. It supports bitwise
  binary operations with other booleans using `and`, `or`, `xor` and negation unary operation using 
  `not` operator.
- **String**: It is used for storing string values and also can reference other string item values. 
  It supports *concat* and *remove* operations with other strings using `+` and `-` operator 
  respectively. The operator precedence can be set by parentheses, otherwise it defaults to
  be right-associative. 
- **Number**: It is used for storing number values and also can reference other number item values.
  It supports `+`, `-`, `*`, `/`, and `%` operators. The operator precedence can be set by 
  parentheses, otherwise it defaults to be right-associative.
- **Array**: It is used for storing a collection of values with the same type. It also supports
  multidimensional arrays whose values have the same type.
- **Object**: It is a dictionary data type composed of a collection of (key, value) pairs, such that 
  each possible key appears at most once in the collection. Each pair can have its own type and can
  have another nested object as its value.
- **Proto**: It is a clone of an object by which we can reuse an already defined object while being
  able to override its item values. 


The `type` statement is used to define the types of a `conf` statement, and they both can be
imported to other Confee file using `import` statement. 


AST and Context-Free Grammar (BNF)
===

- Grammar is the root, Statement and Expression are non-terminal, Node is terminal
- Grammar is a list of Statements
- Statement can contain another Statement, an Expression or a Node
- Statement has scope and can be defined in top-level (type, conf, import, export, format)
- Expression can contain another Expression or a Node
- Expression can not be defined in top-level (literal, lambda, condition, case)
- Literal Expression can be bool, string, number, array, object, proto or other Expressions
- Node is an abstraction which contains a value or points to it
- Node can be a Token which is generated in the lexing phase
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

The context-free grammar of Confee contains data types we have in most common config file notations
such as **Bool**, **String**, **Number**, **Array** and **Object**. 
It also contains a special data type called **Proto** which is a clone of an object letting us 
override its fields.

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

<exprLiteralBool> ::= <exprLiteralBoolUnaryOperator> <exprLiteralBool>
                    | <exprLiteralBoolUnaryOperator> <exprLiteralBoolFactor>
                    | <exprLiteralBoolFactor> <exprLiteralBoolBinaryOperator> <exprLiteralBool>
                    | <exprLiteralBoolFactor> <exprLiteralBoolBinaryOperator> <exprLiteralBoolFactor>
                    | <exprLiteralBoolFactor>

<exprLiteralBoolFactor> ::= <parenthesesOpen> <exprLiteralBool> <parenthesesClose>
                          | <trueBool>
                          | <falseBool>
                          | <word>

<exprLiteralBoolUnaryOperator> ::= <not>

<exprLiteralBoolBinaryOperator> ::= <and>
                                  | <or>
                                  | <xor>

<exprLiteralString> ::= <exprLiteralStringFactor> <exprLiteralStringOperator> <exprLiteral>
                      | <exprLiteralStringFactor> <exprLiteralStringOperator> <exprLiteralStringFactor>
                      | <exprLiteralStringFactor>
                      
<exprLiteralStringFactor> ::= <parenthesesOpen> <exprLiteralString> <parenthesesClose>
                            | <string>
                            | <word>

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
     projectName: Text
     description: Text
     storage: Text
     facts: DataFact
}

type DataFact {
     doc: Text
     workFlows: [DataWorkflow]
}

type DataWorkflow {
     name: Text
     account: Text
     schedule: Text
     dockerArgs: [String]
}
```

Then by importing the types and then defining the proto, we can define the configs:

```
import "/path/to/DataInfoTypes.confee"

conf workflow : DataWorkflow {
     name = "default-workflow-name"
     account = "admin@dataflow.com"
     dockerArgs = ["--wrap-luigi", "--development"]
}

conf dataInfo : DataInfo {
     projectName = "cool-project"
     description = "sample desc"
     storageType = "gcs"
     facts = {
          doc = "It is a very cool project."
          workFlows = [
               workflow { 
                    name = projectName + ".cool-job." schedule + "." + storageType
                    schedule = "hourly" 
               },
               workflow { 
                    name = projectName + ".cool-job." + schedule + "." + storageType 
                    schedule = "daily" 
               },
               workflow { 
                    name = projectName + ".cool-job." + schedule + "." + storageType
                    schedule = "monthly" 
               }
          ]
     }
}
```

Confee CLI
===

Confee config files can be compiled using `confee-cli` with the following flags:
```
confeec - Confee Compiler 0.0.1
Usage: confeec [options]

  -c, --config <name>     config name to be formatted
  -t, --target JSON|YAML  target format of confee file
  -i, --input <path>      path to confee input file
  -o, --output <path>     path to confee output file (optional)
  --help                  prints this usage text
```

It can be executed by `sbt` from the root path of the project and prints the compiled config file 
when then output path flag is not set:
```
# sbt 'confee-cli/run -c dataInfo -i ./confee-cli/src/main/resources/example/data-info.confee -t JSON'
```

Copyright
===

```
Copyright (c) 2021 Spotify AB
```
