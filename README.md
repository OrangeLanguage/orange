# Orange

The Orange programming language compiler.

[![Run on Repl.it](https://repl.it/badge/github/OrangeLanguage/orange)](https://repl.it/github/OrangeLanguage/orange)

## What is Orange?

Functional languages, especially pure functional languages like Haskell, have 
many powerful features that traditional imperative languages lack. Unfortunately
they suffer from complexity and unfamiliarity, and often have to resort to an
imperative style when dealing with side effects. To bridge this gap we created 
Orange, an easy to learn object-functional language that supports the best
features of both object-oriented and purely-functional languages.

- Familiar syntax and semantics
- Simple and extensible classes
- Generic pattern matching
- Partial lazy evaluation with lazy parameters
- Control flow (`if` `else` `while` `for`) as functions
- The effect monad with resumable exceptions
- Continuations with higher order resume (`resume(resume)`)

### That's cool and all, but show me some code

In the spirit of the Repl.it language jam, we'll use Orange to implement a
simple interpreter. You can try the examples by pasting them into the Orange 
repl, or you can test out the end result using `import "examples/Language.oj"`.
The repl can be started with `npm install` and `npm start`.

To begin, we'll define a simple arithmetic language.

```
import "std/Prelude.oj"

class IntExpr(int)
class AddExpr(lhs, rhs)
class MulExpr(lhs, rhs)
```

Classes in Orange are similar to data classes in other languages. They can be
constructed using their name (`IntExpr(1)`) and their fields can be 
accessed with dot notation (`expr.int`). Methods can be defined on classes 
outside of their definition (`def IntExpr.square()`) so that users can extend 
existing classes with new methods.

Now that we've declared our data structure, we can implement an interpreter with
pattern matching and recursion.

```
def Any.eval() match (this) {
    IntExpr(int) int
    AddExpr(lhs, rhs) lhs.eval() + rhs.eval()
    MulExpr(lhs, rhs) lhs.eval() * rhs.eval()
}

// 2
AddExpr(IntExpr(1), IntExpr(1)).eval()
```

The `match` keyword is similar to pattern matching in functional languages or
`switch` statements in imperative languages. It allows us to branch on the type
of an expression and deconstruct it, which we can then use to evaluate the 
expression.

We declare `eval` as a method on `Any`. The Any type is the top type (`Object`)
for Orange, which allows eval to work for any object that has `matchIntExpr`,
`matchAddExpr`, or `matchMulExpr` defined.

This arithmetic language is neat, but it only interprets simple programs. To add 
functions and application, we'll need an environment.

```
import "std/Control.oj"

class Env(get)

def emptyEnv
    Env((name) do "undefined " + name)

def Env.insert(name, value)
    Env((n) if (n == name) { value }.else { this.get(n) })
```

Here we define an environment as a function from a name to a value. The empty
environment is a function which always throws an exception (`do` in Orange is
similar to `throw`, but more powerful). Inserting a value into an environment 
creates a function that returns that value if the name is equal. 

The line `import "std/Control.oj"` imports the `if` function, which uses 
trailing block expressions to provide a convenient if expression. Since if is
just a function, we could have equivalently written 
`if(n == name, value).else(this.get(n))`. Behind the scenes, if uses lazy 
parameters to prevent evaluation of unused branches

With our environment implemented, we can now interpret functions and application.

```
class IdentExpr(name)
class LambdaExpr(name, expr)
class ApplyExpr(fn, arg)

def Any.eval(env) match (this) {
    IntExpr(int) int
    AddExpr(lhs, rhs) lhs.eval(env) + rhs.eval(env)
    MulExpr(lhs, rhs) lhs.eval(env) * rhs.eval(env)
    IdentExpr(name) env.get(name)
    LambdaExpr(name, expr) (arg) expr.eval(env.insert(name, arg))
    ApplyExpr(fn, arg) fn.eval(env)(arg.eval(env))
}

def Any.evalRoot() this.eval(emptyEnv)

// 2
LambdaExpr("a", IdentExpr("a")).evalRoot()(2)

// 4
LambdaExpr("a", MulExpr(IntExpr(2), IdentExpr("a"))).evalRoot()(2)

// 4
ApplyExpr(LambdaExpr("a", MulExpr(IntExpr(2), IdentExpr("a"))), IntExpr(2)).evalRoot()
```

This evaluator leverages Orange's functions and closures to avoid writing 
variable substitution or closure capturing code. As a bonus we get full
compatability with Orange, allowing us to use functions in our mini language as 
though they were Orange functions.

With our evaluator complete, we can move on to the parser. We'll need to define 
a reader effect to read from a string, but first we'll define a simpler effect 
to introduce the concept.

```
class Get()
class Set(state)

def get() do Get()
def set(state) do Set(state)
def modify(f) set(f(get()))
```

The state effect represents a changing state, where `get` returns the current 
state and `set` changes the state. We also define `modify`, which applies a 
function to the current state. Right now these functions will simply terminate 
the program. To make them work properly, we'll need to define a handler 
(`handle` in Orange is similar to `catch`, but more powerful). Since this is a 
rather complicated function, we'll build it step by step. 

```
def mutable(state, lazy f) 
    handle (f()) (effect) match (effect) {
        Get() resume(state)
        Set(newState) resume({})
    }

// 3
mutable (2) { get() + 1 }
```

`mutable` is a function that takes in a state and a `lazy` expression to evaluate
with that state. Lazy expressions are simply expressions that are passed as zero
argument functions. `handle` evaluates the expression and catches any effects 
thrown. If the effect is `Get`, it resumes evaluation with the state. If the 
effect is `Set`, it resumes evaluation with the empty block.

At the moment, this function isn't doing much mutating. It also cannot handle
more than one effect, as effects in the resume function are not being handled. 
We can solve both of these using recursion.

```
def mutable(state, lazy f) 
    handle (f()) (effect) match (effect) {
        Get() mutable(state) { resume(state) }
        Set(newState) mutable(newState) { resume({}) }
    }

// 3
mutable (0) {
    set(1)
    modify((x) x + 2)
    get()
}
```

By resuming in our handler, we can handle any number of effects. Passing the 
new state to our handler allows us to continue evaluation with the new state. 
Combining these two allows us to easily use mutable variables.

Before we can continue, there is one last change we need to make to our mutable
handler. It currently handles every effect, which prevents us from composing it 
with other effect handlers.

```
def mutable(state, lazy f) 
    handle (f()) (effect) match (effect) {
        Get() mutable(state) { resume(state) }
        Set(newState) mutable(newState) { resume({}) }
        Any() {
            let cont do effect
            mutable(state) { resume(cont) }
        }
    }
```

The `Any` case matches any object and immediately re-throws it. If it is 
handled, it binds the result to `cont` and resumes evaluation. `let` is used 
instead of `def` to declare local variables, but cannot define recursive
functions.

With our mutable effect finished, we can now implement the reader effect.

```
class Next()
class HasNext()
class Peek()

def next() do Next()
def hasNext() do HasNext()
def peek() do Peek()

def readString(string, i, lazy f)
    handle (f()) (effect) match (effect) {
        Next() readString(string, i + 1) { resume({}) }
        HasNext() readString(string, i) { resume(i < string.length()) }
        Peek() readString(string, i) { resume(string.charAt(i)) }
        Any() {
            let cont do effect
            readString(string, i) { resume(cont) }
        }
    }
```

The reader effect supports three operations: `next`, which will advance the
reader one element; `hasNext`, which will be true if there is another element to
read; and `peek`, which will return the current element. The state of the reader
is stored as a string and and index, though one could write a handler which 
reads from a file or a list of characters.

We can use this effect to define an integer parser.

```
def IntExpr.toString() this.int.toString()

def charIsInt(c)
    c >= '0' && c <= '9'

def charToInt(c)
    c.toInt() - '0'.toInt()

def while(lazy condition, lazy then) 
    if (condition()) { 
        then()
        while (condition()) { then() }
    }.else {}

def parseIntExpr()
    mutable (0) {
        while (hasNext() && charIsInt(peek())) {
            let char peek()
            next()
            modify((sum) sum * 10 + charToInt(char))
        }
        IntExpr(get())
    }

// 123
readString("123", 0, parseIntExpr())
```

`parseIntExpr` uses both mutable and read effects, creating a mutable integer
and modifying it while the next character is an integer. `while` is implemented 
as a lazy function which recursively tests its condition. When the parser 
reaches a non-integer character, it wraps the result in an integer expression
and returns it.

With the root parser done, we can use precedence climbing to parse addition and
multiplication.

```
def AddExpr.toString() "(" + this.lhs.toString() + " + " + this.rhs.toString() + ")"
def MulExpr.toString() "(" + this.lhs.toString() + " * " + this.rhs.toString() + ")"

def ifEnd(lazy then, lazy f)
    if (hasNext().not()) { then() }.else { f() }

def skipIgnored()
    while (hasNext() && peek() == ' ') {
        next()
    }

// We'll implement this later
def parseApplyExpr parseIntExpr

def parseMulExpr() {
    let expr parseApplyExpr()
    skipIgnored()
    ifEnd (expr) {
        if (peek() == "*") {
            next()
            MulExpr(expr, parseMulExpr())
        }.else { expr }
    }
}

def parseAddExpr() {
    let expr parseMulExpr()
    skipIgnored()
    ifEnd (expr) {
        if (peek() == "+") {
            next()
            AddExpr(expr, parseAddExpr())
        }.else { expr }
    }
}

// ((2 * 2) + 1)
readString("2 * 2 + 1", 0, parseAddExpr())

// (1 + (2 * 2))
readString("1 + 2 * 2", 0, parseAddExpr())
```

`skipIgnored` skips whitespace and is called at the beginning of each parser.
`ifEnd` is a simple convenience function that keeps the parser from running past
the end of the string. `parseAddExpr` and `parseMulExpr` are both recursive, 
parsing an expression and then optionally parsing an operator. Since 
multiplication has higher precedence than addition, it is defined first.

The rest of the parser is simply an extension to the existing parser.

```
def IdentExpr.toString() this.name
def LambdaExpr.toString() "(λ" + this.name + " " + this.expr.toString() + ")"
def ApplyExpr.toString() "(" + this.fn.toString() + " " + this.arg.toString() + ")"

def parseParens() {
    let expr parseAddExpr()
    if (peek == ')') {
        do "Expected ')'"
    }.else {
        next()
        expr
    }
}

def charIsIdent(c)
    c >= 'a' && c <= 'z'

def parseIdentExpr()
    mutable ("") {
        while (hasNext() && charIsIdent(peek())) {
            let char peek()
            next()
            modify((ident) ident + char.toString())
        }
        IdentExpr(get())
    }

def parseLambdaExpr() {
    let ident parseIdentExpr()
    skipIgnored()
    LambdaExpr(ident.name, parseAddExpr())
}

def parseAtomicExpr() {
    skipIgnored()
    ifEnd (do "Unexpected end of reader") {
        if (peek() == '(') {
            next()
            parseParens()
        }.elif (peek() == '\\') {
            next()
            parseLambdaExpr()
        }.elif (charIsInt(peek())) {
            parseIntExpr()
        }.elif (charIsIdent(peek())) {
            parseIdentExpr()
        }.else {
            do "Unexpected character '" + peek().toString() + "'"
        }
    }
}

def parseApplyTrail(expr) {
    skipIgnored()
    ifEnd (expr) {
        if (peek() == ')') {
            next()
            expr
        }.else {
            parseApplyTrail(ApplyExpr(expr, parseAtomicExpr()))
        }
    }
}

def parseApplyExpr() {
    let expr parseAtomicExpr()
    skipIgnored()
    ifEnd (expr) {
        if (peek() == '(') {
            next()
            parseApplyTrail()
        }.else { expr }
    }
}

// (2 * (2 + 1))
readString("2 * (2 + 1)", 0, parseAddExpr())

// x
readString("x", 0, parseAddExpr())

// (λx x)
readString("\\x x", 0, parseAddExpr())

// ((x 1) 2))
readString("x(1 2)", 0, parseAddExpr())
```

With the parser complete, we can connect the parser and evaluator to define an 
eval function.

```
def eval(string)
    readString(string, 0, parseAddExpr()).evalRoot()

// 4
eval("2*2")

def inc eval("\\x x + 1")

// 2
inc(1)
```

While this language is not particularly complex, it shows how Orange's small 
feature set (definitions, functions, classes and effects) can be used to implement 
non-trivial programs with familiar yet extensible semantics. Further extensions 
would require the backtracking and ambiguity effects which, while really cool, 
would take too long to properly implement, test, and explain. If you're 
interested though, here's a prototype.

```
def readString(string, i, lazy f)
    handle (f()) (effect) match (effect) {
        Next() readString(string, i + 1) { resume({}) }
        HasNext() readString(string, i) { resume(i < string.length()) }
        Peek() readString(string, i) { resume(string.charAt(i)) }
        // Attempts to resume with try, handling any effects by resuming with else
        Backtrack(try, else) 
            handle (readString(string, i) { resume(try()) }) { resume(else) }
        Any() {
            let cont do effect
            readString(string, i) { resume(cont) }
        }
    }

// Handles the ambiguity effect by collecting all possible results in a list.
def ambiguousList(lazy f)
    handle (singleton(f())) (effect) match (effect) {
        // Concatenates the results of resuming with true and resuming with false
        // Note: list concatenation isn't currently implemented
        Ambiguous() ambiguousList { resume(true) } + ambiguousList { resume(false) }
        Any() {
            let cont do effect
            writeList(list) { resume(cont) }
        }
    }
```

## Potential Features

Since Orange was implemented in 21 days during the Repl.it language jam, there 
are lots of cool features that we were unable to implement. This is an
incomprehensive list of features that could potentially be added to Orange in 
the future.

### Improved Syntax and Syntax Sugar

Orange's current syntax is sufficent but incomplete. Some examples of additional
syntax include infix application, unary operators, improved `def` and `let`,
`handle` default matching, newline statement terminators, and additional 
patterns for pattern matching.

### Named Effect Handlers

Multiple effects can be approximated with unique objects, but there is currently
no way to generalize named effects. Named effects work especially well with
infix operators, allowing traditional `name = value` syntax

```
class Write(unique, elem)

def Any.write(elem) do Write(this, elem)
infix right 1 += write

// Approximates named effects with unique objects
def Unique.writeList(list, lazy f) {
    handle ({ f() list }) (effect) match (effect) {
        Write(unique, elem) 
            if (unique == this) { 
                elem : writeList(list) { resume({}) }
            }.else { 
                let cont do effect
                writeList (list) { resume(cont) }
            }
        Any() {
            let cont do effect
            writeList (list) { resume(cont) }
        }
    }
}

Any().writeList(nil, (x) {
    Any().writeList(nil, (y) {
        x += 1
        y += 2
    })
})
```

### Let Generalized for any Continuation

Let syntax currently desugars to the following declaration.

```
def let(x, f) f(x)
let(2, (x) ...)
```

This could be extended to work with any function that takes a continuation.

```
def mutable(x, f) Any().mutableState(x, (var) f(var))

let mutable x 0
let mutable x 0
x += 2
++y
```

### Lenses

Lenses are necessary when dealing with immutable data to update nested fields.
This can be done by generating lenses for all fields by default while allowing
classes like List to define their own custom lenses.

```
class Address(street, city)
class Person(name, address)

def person ...

import "std/Lens.oj"

// Colon is being used for lens composition
person:address:street.get() 
person:address:street.set("123rd St")
```

### The Big One, Types

Adding a type system to Orange has its pros and cons. On the upside, it greatly 
improves static analysis for both objects and effects. On the downside, it
reduces flexibilty and steepens the learning curve for new functional 
programmers. While we are generally in favor of type systems, we are not sure if 
it is the correct choice for Orange--and even if it were, it would take lots of 
time that could be going towards implementing other features.

If we were to add a type system, it would likely be an extension of Scala's Dot 
Calculus with effect types and bidirectional type checking. Orange's syntax for 
such a type system would attempt to match the familiarity the rest of the 
language.

## Getting Started

Orange is written in PureScript, runs on JavaScript, and builds with npm and 
spago. 

### Install Dependencies

```bash
spago install
npm install
```

### Build & Run the REPL

```bash
spago build
npm run repl
```

### VSCode

Orange includes a simple VSCode plugin located in [vscode](vscode).
