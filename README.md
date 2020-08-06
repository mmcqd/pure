# Pure
This is an interpreter for arbitrary pure type systems. Check [here](https://en.wikipedia.org/wiki/Pure_type_system) for a solid description.

Pure uses bidirectional type checking, so it's typing rules are slightly different from the ones listed on wikipedia. Here are the typing rules:

![Rules](https://i.imgur.com/ntdjEI9.png)
  

## Compiling

Pure uses [Dune](https://github.com/ocaml/dune), which can be install with `opam install dune`. 

To build the interpreter, run `dune build repl.exe`, and run the resulting file at `_build/default/repl.exe` 

## Using Pure

The top of every `.pure` file must contain 3 interpreter pragmas: `%SORTS`, `%AXIOMS`, and `%RULES`. To use System F as an example:

```
%SORTS Type | Kind
%AXIOMS Type : Kind
%RULES Type,Type,Type | Kind,Type,Type
```
These define the system that the rest of the file will be type checked against. The rest of the file can be zero or more declarations.

Declarations are in OCaml style, using `let` with an optional type annotation.
```
let id = \(A : Type)(x : A) x
let if : \/(A : Type) A -> A = \(A)\(x) x
let f : \/(A B : Type) (A -> A -> B) -> A -> A -> B = \(A B : Type)(f : A -> A -> B)(x y : A) f x y
let g : \/(A B : Type) (A -> A -> B) -> A -> A -> B = \(_ _ f x y) f x y
```
Lambda functions have optional type annotations on their arguments. If none are provided, Pure will try to infer the type of the function. 
A function declared at the top level with no annotations on it's arguments cannot have it's type infered, so an annoation on the declaration becomes necessary.
Notice how the type annotation on the declaration allows us to avoid giving names to the the type parameters of the lambda.
In annoted lambdas and in pi types, arguments with the same type can be conviniently grouped together. In unannotated lambdas, all arguments can be grouped together.
Once a file has been read, you'll be presented with a REPL. Here you can evaluate expressions and make new top level bindings.

## Some Well Known Pure Type Systems
### Simply Typed Lambda Calculus, with the unit type
```
%SORTS <> | Unit | Type
%AXIOMS <> : Unit | Unit : Type
%RULES Type,Type,Type
```

### System F
```
%SORTS Type | Kind
%AXIOMS Type : Kind
%RULES Type,Type,Type | Kind,Type,Type
```

### System Fω
```
%SORTS Type | Kind
%AXIOMS Type : Kind
%RULES Type,Type,Type | Kind,Type,Type | Kind,Kind,Kind
```

### Calculus of Constructions
```
%SORTS Prop | Type
%AXIOMS Prop : Type
%RULES Prop,Prop,Prop | Prop,Type,Type | Type,Prop,Prop | Type,Type,Type
```

### System U
```
%SORTS * | BOX | TRI
%AXIOMS * : BOX | BOX : TRI
%RULES *,*,* | BOX,*,* | BOX,BOX,BOX | TRI,*,* | TRI,BOX,BOX
```

I'll note that in all of these systems, the last two elements of each RULE triple are the same. The only type system off the top of my head where this is not the case is one with an infinite heirachy of universes, but this is not expressible using Pure 

(we'd need something like `forall s1,s2. s1,s2,max(s1,s2)`).




 
