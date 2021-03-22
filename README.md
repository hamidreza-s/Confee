Confee
===

Type safe, prototypal, modular alternative to config files. 
A Confee notation file is compiled to JSON, YAML or other config notation files.

Here is an example to define data pipelines to be used as the `data-info.yaml` for Luigi.
The types can be used in a global space to be used by every data pipeline project:
```
type DataInfo {
     project: String
     storage: String
     facts: Fact
}

type Fact {
     doc: String
     workFlows: [Workflow]
}

type Workflow {
     name: String
     account: String
     schedule: String
     dockerArgs: [String]
}
```

Then by importing the types and then defining the proto, we can define the configs:

```
import "/path/to/types.confee"

conf workflow : Workflow {
     name = "default-workflow-name"
     account = "admin@dataflow.com"
     schedule = "hourly"
     dockerArgs = ["--wrap-luigi", "--development"]
}

conf dataInfo : DataInfo {
     project = "cool-project"
     storage = "gcs"
     facts = {
          doc = "It is a very cool project."
          workFlows = [
               workflow { schedule = "hourly" name = "foo." + storage },
               workflow { schedule = "hourly" name = "bar." + storage },
               workflow { schedule = "weekly" name = "bat." + storage }
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

  -c, --config <name>      config name to be formatted
  -t, --target json|yaml   target format of confee file
  -i, --input <file>       path to confee input file
  -o, --output <file>      path to confee output file
  -I, --include <dir>,...  comma-separated directories to include for imports
  -R, --relax              disable object key name validator
  -T, --typeless           disable type checker
  --help                   prints this usage text
```

It can be executed by `sbt` from the root path of the project and prints the compiled config file
when then output path flag is not set:
```
# sbt 'confee-cli/run --config dataInfo
                      --target yaml
                      --input ./confee-cli/src/main/resources/example/data-info.confee
                      --include ./confee-cli/src/main/resources/example/'
```

Syntax
===

`conf`: It is a top-level key-value structure which has a type and will be formatted to and object.
- Conf name should always start with Lowercase
- Conf item keys can start with lowercase or uppercase

`type`: It is a top-level key-value structure similar to conf, but it just holds the types.
- Type names should just start with Uppercase
- Type item keys can start with lowercase or uppercase

`import`: It is a top-level statement which is used to import Conf and Type to a given file.

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
  have another nested object as its value. Object item keys can start with lowercase or uppercase.
- **Proto**: It is a clone of an object by which we can reuse an already defined object while being
  able to override its item values. Proto item keys can start with lowercase or uppercase.

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
                  
<typeStmtItem> ::= <typeItemKey> <colon> <name>
                 | <typeItemKey> <colon> <bracketOpen> <name> <bracketClose>

<typeItemKey> ::= <string>

<confStmt> ::= <confKeyword> <word> <colon> <confStmtType> <braceOpen> <confStmtItems> <braceClose>

<confStmtItems> ::= <confStmtItem> <confStmtItems>
                  | <confStmtItem>
             
<confStmtItem> ::= <confItemKey> <assignment> <expr>

<confStmtType> ::= <name>

<confItemKey> ::= <string>
                 
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

Copyright
===

```
Copyright (c) 2021 Spotify AB
```
