class _Any {}

const _unit = new _Any();
_unit.toString = () => "unit"

class _Bool extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    _if(t, f) { 
        if (this.value) {
            return t();
        } else {
            return f();
        }
    }

    toString() { return new _String(this.value.toString()); }

    and(x) { return new _Bool(this.value && x().value) }
    or(x) { return new _Bool(this.value || x().value) }
}

const _true = new _Bool(true);
const _false = new _Bool(false);

class _Int extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    toString() { return new _String(this.value.toString()); }

    add(x) { return new _Int(this.value + x().value); }
    sub(x) { return new _Int(this.value - x().value); }
    mul(x) { return new _Int(this.value * x().value); }
    div(x) { return new _Int(this.value / x().value); }
    rem(x) { return new _Int(this.value % x().value); }
    pow(x) { return new _Int(this.value ** x().value); }

    lt(x) { return new _Bool(this.value < x().value); }
    gt(x) { return new _Bool(this.value > x().value); }
    eq(x) { return new _Bool(this.value == x().value); }
    leq(x) { return new _Bool(this.value <= x().value); }
    geq(x) { return new _Bool(this.value >= x().value); }
    neq(x) { return new _Bool(this.value != x().value); }
}

class _Char extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    toString() { return new _String(this.value.toString()); }
}

class _String extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    toString() { return this; }
    add(x) { return new _String(this.value + x().value); }
}

const log = (line) => {
    console.log(line().toString().value);
    return _unit;
}
