class _Any {}

const _unit = new _Any();
_unit.toString = () => "unit"

class _Bool extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    _if(t, f, _cont) { 
        if (this.value) {
            return _cont()(() => t());
        } else {
            return _cont()(() => f());
        }
    }

    toString(_cont) { return _cont()(() => new _String(this.value.toString())); }

    and(x, _cont) { return _cont()(() => new _Bool(this.value && x().value)); }
    or(x, _cont) { return _cont()(() => new _Bool(this.value || x().value)); }
}

const _true = new _Bool(true);
const _false = new _Bool(false);

class _Int extends _Any {
    constructor(value) {
        super();
        this.value = (value);
    }

    toString(_cont) { return _cont()(() => new _String(this.value.toString())); }

    add(x, _cont) { return _cont()(() => new _Int(this.value + x().value)); }
    sub(x, _cont) { return _cont()(() => new _Int(this.value - x().value)); }
    mul(x, _cont) { return _cont()(() => new _Int(this.value * x().value)); }
    div(x, _cont) { return _cont()(() => new _Int(this.value / x().value)); }
    rem(x, _cont) { return _cont()(() => new _Int(this.value % x().value)); }
    pow(x, _cont) { return _cont()(() => new _Int(this.value ** x().value)); }

    lt(x, _cont) { return _cont()(() => new _Bool(this.value < x().value)); }
    gt(x, _cont) { return _cont()(() => new _Bool(this.value > x().value)); }
    eq(x, _cont) { return _cont()(() => new _Bool(this.value == x().value)); }
    leq(x, _cont) { return _cont()(() => new _Bool(this.value <= x().value)); }
    geq(x, _cont) { return _cont()(() => new _Bool(this.value >= x().value)); }
    neq(x, _cont) { return _cont()(() => new _Bool(this.value != x().value)); }
}

class _Char extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    toString(_cont) { return _cont()(() => new _String(this.value.toString())); }
}

class _String extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    toString(_cont) { return _cont()(() => this); }
    add(x, _cont) { return _cont()(() => new _String(this.value + x().value)); }
}

const log = (line, _cont) => {
    console.log(line().toString(() => x => x().value));
    return _cont()(() => _unit);
}
