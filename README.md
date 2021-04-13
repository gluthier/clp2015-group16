# Tool Compiler Project

This project is a compiler for a small, Java-like, object-oriented programmign language called Tool (stands for Toy Object-Oriented Language). All phases of a modern compiler are developed, resulting in an implementation which compiles source files into Java Bytecode in the same way `javac` does.

It hase been done for the Computer Language Processing course at EPFL. More details about the project [here](https://lara.epfl.ch/w/cc10/tool_compiler_project).

## Syntax examples

Some examples of `.tool` programs can be found in the [programs](./programs/) folder. A formal description of the syntax can be found [here](https://lara.epfl.ch/w/cc10/tool).

## Liberal syntax extension

We imporved the base project by allowing a more liberal syntax with the four following main features:

1. **Semicolon inference**: optional semicolons to indicate the end of an instruction. To do this, our parser does the longest possible match on instructions. This means that it is possible to span an instruction over several lines and that it will be parsed correctly.
2. **Methods  as  infix  operators**:  ability  to  call  methods  in  an  infix  way.  This  applies  only  to  methods that have only one argument to keep the code readable. Methods with several arguments still have tobe called using the standard `Tool` notation.
3. **Expressions as statements**: let expressions take the place of statements. Even though it is not very interesting to be able to have literals hanging around in the code, it allows the `Tool` programmer to call methods of an object without necessarily having to assign the return value to a variable. This can help to remove a lot of code clutter.
4. **Operators as methods**: override or define operators for custom classes. This feature makes it possible to have methods defined with operators (+, -, *, etc.) for the  user’s  classes.  The  aim  of  this  extension  is  to allow the programmer to have clearer code by being able to define these methods for any class.

More details in our [paper](./report-template/report.pdf). Some examples of programs using the liberal syntax extension can be found in the [liberalprograms](./liberalprograms/) folder.
