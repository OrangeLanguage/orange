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
simple interpreter. To start, we'll define a simple arithmetic language.

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

This arithmetic language is cool, but it only works for simple programs. We'll 
add functions and application soon, but first we'll need an environment.

```
class Env(get)

infix left 4 ==(x, y) x.eq(y)

def Env.insert(name, value)
    Env((n) {n == name}.if(value, this.get(name)))
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

We can clean up eval a bit using effects.

```
def local(name, value, lazy f)
    handle (f()) (n)
        {n == name}.if(local(name, value) { resume(value) }, do name)

def Any.eval() match (this) {
    IntExpr(int) int
    AddExpr(lhs, rhs) lhs.eval() + rhs.eval()
    MulExpr(lhs, rhs) lhs.eval() * rhs.eval()
    IdentExpr(name) do name
    LambdaExpr(name, expr) (arg) local(name, arg) { expr.eval() }
    ApplyExpr(fn, arg) fn.eval()(arg.eval())
}

def Any.evalRoot() 
    handle (this.eval()) (n)
        do {"undefined " + n}
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
    }
```

We can use this effect to define an integer parser.

```
def IntExpr.toString() "Expr(" + this.int.toString() + ")"

infix left 6 -(x, y) x.sub(y)
infix left 4 <=(x, y) x.leq(y)
infix left 4 >=(x, y) x.geq(y)
infix left 3 &&(x, lazy y) x.and(y())
infix left 2 ||(x, lazy y) x.or(y())

def charToInt(c)
    c.toInt() - '0'.toInt()

def charIsInt(c)
    c >= '0' && c <= '9'

def let(x, f) f(x)

def parseIntExpr() {
    def go(sum) 
        {hasNext() && charIsInt(peek())}.if(
            let(peek(), (char) {
                next()
                go(sum * 10 + charToInt(char))
            }), sum)
    IntExpr(go(0))
}

// Expr(123)
readString("123", 0, parseIntExpr())
```

Now we can use precedence climbing to parse addition and multiplication.

```
def AddExpr.toString() "Expr(" + this.lhs.toString() + " + " + this.rhs.toString() + ")"
def MulExpr.toString() "Expr(" + this.lhs.toString() + " * " + this.rhs.toString() + ")"

def ifHasNext(lazy then, lazy f)
    {hasNext().not()}.if(then(), f())

def parseMulExpr() 
    let(parseIntExpr(), (expr) 
        ifHasNext(expr) {
            {peek() == '*'}.if({
                next()
                MulExpr(expr, parseMulExpr())
            }, expr)
        })

def parseAddExpr() 
    let(parseMulExpr(), (expr) 
        ifHasNext(expr) {
            {peek() == '+'}.if({
                next()
                AddExpr(expr, parseAddExpr())
            }, expr)
        })

// Expr(Expr(Expr(2) * Expr(2)) + Expr(1))
readString("2*2+1", 0, parseAddExpr())

// Expr(Expr(1) + Expr(Expr(2) * Expr(2)))
readString("1+2*2", 0, parseAddExpr())
```

Skipping ignored characters allows us to process whitespace.

```
def skipIgnored()
    (hasNext() && peek() == ' ').if({
        next()
        skipIgnored()
    }, {})

def parseIntExpr() {
    def go(sum) 
        {hasNext() && charIsInt(peek())}.if(
            let(peek(), (char) {
                next()
                go(sum * 10 + charToInt(char))
            }), sum)
    skipIgnored()
    IntExpr(go(0))
}

def parseMulExpr() 
    let(parseIntExpr(), (expr) {
        skipIgnored()
        ifHasNext(expr) {
            {peek() == '*'}.if({
                next()
                MulExpr(expr, parseMulExpr())
            }, expr)
        }
    })

def parseAddExpr() 
    let(parseMulExpr(), (expr) { 
        skipIgnored()
        ifHasNext(expr) {
            {peek() == '+'}.if({
                next()
                AddExpr(expr, parseAddExpr())
            }, expr)
        }
    })

// Expr(Expr(Expr(2) * Expr(2)) + Expr(1))
readString("  2  *  2  +  1  ", 0, parseAddExpr())
```

The rest of the parser is simply an extension to the existing parser.

```
def IdentExpr.toString() "Expr(" + this.name + ")"
def LambdaExpr.toString() "Expr(λ" + this.name + "." + this.expr.toString() + ")"
def ApplyExpr.toString() "Expr(" + this.fn.toString() + " $ " + this.arg.toString() + ")"

def parseParens() {
    skipIgnored()
    let(parseAddExpr(), (expr) {
        {peek() != ')'}.if(do "Expected ')'") {
            next()
            expr
        }
    })
}

def charIsIdent(c)
    c >= 'a' && c <= 'z'

def parseIdentExpr() {
    def go(ident) 
        {hasNext() && charIsIdent(peek())}.if(
            let(peek(), (char) {
                next()
                go(ident + char.toString())
            }), ident)
    skipIgnored()
    IdentExpr(go(""))
}

def parseLambdaExpr() {
    skipIgnored()
    let(parseIdentExpr(), (ident) {
        skipIgnored()
        let(parseAddExpr(), (expr) LambdaExpr(ident.name, expr))
    })
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

def parseApplyExpr() 
    let(parseAtomicExpr(), (expr) {
        skipIgnored()
        ifHasNext(expr) {
            {peek() == '$'}.if({
                next()
                ApplyExpr(expr, parseAtomicExpr())
            }, expr)
        }
    })

def parseMulExpr() 
    let(parseApplyExpr(), (expr) {
        skipIgnored()
        ifHasNext(expr) {
            {peek() == '*'}.if({
                next()
                MulExpr(expr, parseMulExpr())
            }, expr)
        }
    })

def parseAddExpr() 
    let(parseMulExpr(), (expr) { 
        skipIgnored()
        ifHasNext(expr) {
            {peek() == '+'}.if({
                next()
                AddExpr(expr, parseAddExpr())
            }, expr)
        }
    })

// Expr(Expr(2) * Expr(Expr(2) + Expr(1)))
readString("2 * (2 + 1)", 0, parseAddExpr())

// Expr(x)
readString("x", 0, parseAddExpr())

// Expr(λx.Expr(x))
readString("\\x x", 0, parseAddExpr())

// Expr(Expr(Expr(x) $ Expr(1)) $ Expr(2))
readString("(x $ 1) $ 2", 0, parseAddExpr())
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
