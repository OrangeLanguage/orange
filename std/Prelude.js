class _Any {
    matchAny(f, cont, _cont) {
        return f(((f) =>
          f(((_2) =>
            _cont(_2)))));
    }
}

function Any(_cont) {
  return _cont(new _Any());
}

const BigInt = require('big-integer');

const _unit = new _Any();
_unit.toString = () => "unit"

class _Bool extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    _if(t, f, _cont) { 
        if (this.value) {
            return t(t => _cont(t));
        } else {
            return f(f => _cont(f));
        }
    }

    and(x, _cont) { 
        if (this.value) {  
            return x(x => _cont(x));
        } else {
            return _cont(_false);
        }
    }

    or(x, _cont) {
        if (this.value) {
            return _cont(_true);
        } else {
            return x(x => _cont(x));
        }
    }

    not(_cont) {
        if (this.value) {
            return _cont(_false);
        } else {
            return _cont(_true);
        }
    }

    toString(_cont) { return _cont(new _String(this.value.toString())); }
    eq(x, _cont) { return x(x => _cont(new _Bool(this.value == x.value))); }
    neq(x, _cont) { return x(x => _cont(new _Bool(this.value != x.value))); }
}

const _true = new _Bool(true);
const _false = new _Bool(false);

class _Int extends _Any {
    constructor(value) {
        super();
        this.value = BigInt(value);
    }

    toString(_cont) { return _cont(new _String(this.value.toString())); }

    add(x, _cont) { return x(x => _cont(new _Int(this.value.add(x.value)))); }
    sub(x, _cont) { return x(x => _cont(new _Int(this.value.subtract(x.value)))); }
    mul(x, _cont) { return x(x => _cont(new _Int(this.value.multiply(x.value)))); }
    div(x, _cont) { return x(x => _cont(new _Int(this.value.divide(x.value)))); }
    rem(x, _cont) { return x(x => _cont(new _Int(this.value.mod(x.value)))); }
    pow(x, _cont) { return x(x => _cont(new _Int(this.value.pow(x.value)))); }

    lt(x, _cont) { return x(x => _cont(new _Bool(this.value.lesser(x.value)))); }
    gt(x, _cont) { return x(x => _cont(new _Bool(this.value.greater(x.value)))); }
    eq(x, _cont) { return x(x => _cont(new _Bool(this.value.equals(x.value)))); }
    leq(x, _cont) { return x(x => _cont(new _Bool(this.value.lesserOrEquals(x.value)))); }
    geq(x, _cont) { return x(x => _cont(new _Bool(this.value.greaterOrEquals(x.value)))); }
    neq(x, _cont) { return x(x => _cont(new _Bool(this.value.notEquals(x.value)))); }
}

class _Char extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    toString(_cont) { return _cont(new _String(this.value.toString())); }
    toInt(_cont) { return _cont(new _Int(this.value.charCodeAt())); }
    lt(x, _cont) { return x(x => _cont(new _Bool(this.value < x.value))); }
    gt(x, _cont) { return x(x => _cont(new _Bool(this.value > x.value))); }
    eq(x, _cont) { return x(x => _cont(new _Bool(this.value == x.value))); }
    leq(x, _cont) { return x(x => _cont(new _Bool(this.value <= x.value))); }
    geq(x, _cont) { return x(x => _cont(new _Bool(this.value >= x.value))); }
    neq(x, _cont) { return x(x => _cont(new _Bool(this.value != x.value))); }
}

class _String extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    toString(_cont) { return _cont(this); }
    length(_cont) { return _cont(new _Int(this.value.length)); }
    charAt(x, _cont) { return x(x => _cont(new _Char(this.value.charAt(x.value)))); }
    add(x, _cont) { return x(x => _cont(new _String(this.value + x.value))); }
    eq(x, _cont) { return x(x => _cont(new _Bool(this.value == x.value))); }
    neq(x, _cont) { return x(x => _cont(new _Bool(this.value != x.value))); }
}

const log = (line, _cont) => {
    return line(line => {
        console.log(line.toString(x => x.value));
        return _cont(_unit);
    })
}
