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

### That's cool and all, but show me some code

In the spirit of the Repl.it language jam, we'll use Orange to implement a
simple interpreter. You can try the examples by pasting them into the Orange 
repl, or you can test out the end result using `import "examples/Language.oj"`.
The repl can be started with `npm install` and `npm start`.

To begin, we'll define a simple arithmetic language.

```
class IntExpr(int)
class AddExpr(lhs, rhs)
class MulExpr(lhs, rhs)
```

Now that we've declared our data structure, we can implement an interpreter with
pattern matching and recursion.

```
def Any.eval() match (this) {
    IntExpr(int) int
    AddExpr(lhs, rhs) lhs.eval().add(rhs.eval())
    MulExpr(lhs, rhs) lhs.eval().mul(rhs.eval())
}

// 2
AddExpr(IntExpr(1), IntExpr(1)).eval()
```

The `add` and `mul` methods are looking a bit ugly, but we can fix that by
defining some operators.

```
infix left 6 +(x, y) x.add(y)
infix left 7 *(x, y) x.mul(y)

def Any.eval() match (this) {
    IntExpr(int) int
    AddExpr(lhs, rhs) lhs.eval() + rhs.eval()
    MulExpr(lhs, rhs) lhs.eval() * rhs.eval()
}
```

This arithmetic language is cool, but it only works for simple programs. To add 
functions and applications, we'll need an environment.

```
class Env(get)

infix left 4 ==(x, y) x.eq(y)

def Env.insert(name, value)
    Env((n) {n == name}.if(value, this.get(n)))
```

Now that we have an environment, we can interpret functions and applications.

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

def Any.evalRoot() this.eval(Env((name) do {"undefined " + name}))

// 2
LambdaExpr("a", IdentExpr("a")).evalRoot()(2)

// 4
LambdaExpr("a", MulExpr(IntExpr(2), IdentExpr("a"))).evalRoot()(2)

// 4
ApplyExpr(LambdaExpr("a", MulExpr(IntExpr(2), IdentExpr("a"))), IntExpr(2)).evalRoot()
```

With our evaluator complete, we can move on to the parser. First we'll need to
define a reader effect.

```
infix left 4 <(x, y) x.lt(y)
infix left 4 >(x, y) x.gt(y)

class Next()
class HasNext()
class Peek()
def next() do Next()
def hasNext() do HasNext()
def peek() do Peek()

def readString(string, i, lazy f)
    handle (f()) (eff) match (eff) {
        Next() readString(string, i + 1) { resume({}) }
        HasNext() readString(string, i) { resume(i < string.length()) }
        Peek() readString(string, i) { resume(string.charAt(i)) }
        Any() resume(do eff)
    }
```

We can use this effect to define an integer parser.

```
def IntExpr.toString() this.int.toString()

infix left 6 -(x, y) x.sub(y)
infix left 4 <=(x, y) x.leq(y)
infix left 4 >=(x, y) x.geq(y)
infix left 3 &&(x, lazy y) x.and(y())
infix left 2 ||(x, lazy y) x.or(y())

def skipIgnored()
    (hasNext() && peek() == ' ').if({
        next()
        skipIgnored()
    }, {})

def charToInt(c)
    c.toInt() - '0'.toInt()

def charIsInt(c)
    c >= '0' && c <= '9'

def parseIntExpr() {
    def go(sum) 
        {hasNext() && charIsInt(peek())}.if({
            let char peek()
            next()
            go(sum * 10 + charToInt(char))
        }, sum)
    skipIgnored()
    IntExpr(go(0))
}

// 123
readString("123", 0, parseIntExpr())
```

Now we can use precedence climbing to parse addition and multiplication.

```
def AddExpr.toString() "(" + this.lhs.toString() + " + " + this.rhs.toString() + ")"
def MulExpr.toString() "(" + this.lhs.toString() + " * " + this.rhs.toString() + ")"

def ifHasNext(lazy then, lazy f)
    {hasNext().not()}.if(then(), f())

// We'll implement this next
def parseApplyExpr parseIntExpr

def parseMulExpr() {
    let expr parseApplyExpr()
    skipIgnored()
    ifHasNext(expr) {
        {peek() == '*'}.if({
            next()
            MulExpr(expr, parseMulExpr())
        }, expr)
    }
}

def parseAddExpr() {
    let expr parseMulExpr()
    skipIgnored()
    ifHasNext(expr) {
        {peek() == '+'}.if({
            next()
            AddExpr(expr, parseAddExpr())
        }, expr)
    }
}

// ((2 * 2) + 1)
readString("2*2+1", 0, parseAddExpr())

// (1 + (2 * 2))
readString("1+2*2", 0, parseAddExpr())
```

The rest of the parser is simply an extension to the existing parser.

```
def IdentExpr.toString() this.name
def LambdaExpr.toString() "(λ" + this.name + " " + this.expr.toString() + ")"
def ApplyExpr.toString() "(" + this.fn.toString() + " " + this.arg.toString() + ")"

def parseParens() {
    skipIgnored()
    let expr parseAddExpr();
    {peek() != ')'}.if(do "Expected ')'") {
        next()
        expr
    }
}

def charIsIdent(c)
    c >= 'a' && c <= 'z'

def parseIdentExpr() {
    def go(ident) 
        {hasNext() && charIsIdent(peek())}.if({
            let char peek()
            next()
            go(ident + char.toString())
        }, ident)
    skipIgnored()
    IdentExpr(go(""))
}

def parseLambdaExpr() {
    skipIgnored()
    let ident parseIdentExpr()
    skipIgnored()
    LambdaExpr(ident.name, parseAddExpr())
}

def parseAtomicExpr() {
    skipIgnored()
    ifHasNext(do "Unexpected end of reader") {
        {peek() == '('}.if({ next() parseParens() }) {
            {peek() == '\\'}.if({ next() parseLambdaExpr() }) {
                {charIsInt(peek())}.if(parseIntExpr()) {
                    {charIsIdent(peek())}.if(parseIdentExpr()) {
                        do "Unexpected character"
                    }
                }
            }
        }
    }
}

def parseApplyExpr() {
    let expr parseAtomicExpr()
    skipIgnored()
    ifHasNext(expr) {
        {peek() == '$'}.if({
            next()
            ApplyExpr(expr, parseAtomicExpr())
        }, expr)
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

## Potential Features

Since Orange was implemented in 21 days during the Repl.it language jam, there 
are lots of cool features that we were unable to implement. This is an
incomprehensive list of features that could potentially be added to Orange in 
the future.

### Improved Syntax and Syntax Sugar

Orange's current syntax is sufficent but incomplete. Some examples of additional
syntax include infix application, unary operators, better `def` and `let`,
newline statement terminators, and additional patterns for pattern matching.

### Named Effect Handlers

Multiple effects can be approximated with unique objects, but there is currently
no way to generalize named effects. Named effects work especially well with
infix operators, allowing traditional `name = value` syntax

```
class Write(unique, elem)

def Any.write(elem) do Write(this, elem)
infix right 1 += write

def Unique.writeList(list, lazy f) {
    handle ({ f() list }) (effect) match (effect) {
        Write(unique, elem) 
            if (unique == this) { 
                elem : writeList(list) { resume({}) }
            }.else { resume(do effect) }
        Any() resume(do effect)
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
programmers. We are generally in favor of type systems, but we are not sure
if it is the correct choice for Orange--and even if it were, it would take lots
of time that could be going towards implementing other features.

If we were to add a type system, it would likely be an extension of Scala's Dot 
Calculus with effect types and bidirectional type checking. Describing Orange's 
syntax for such a type system is too speculative to be done here, but it would
take the same familiarity over simplicity approach as the rest of the language.

## Getting Started

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

### Run tests

```bash
spago test
```
