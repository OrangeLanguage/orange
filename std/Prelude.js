const _handle = (x) => { console.log(`Unhandled effect ${x}`); }

class _Any {

}

const _unit = new _Any();
_unit.toString = () => "unit"

class _Bool extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    if(_handle, t, f) { 
        if (this.value) {
            return t();
        } else {
            return f();
        }
     }

    and(_handle, x) { return new _Bool(this.value && x().value) }
    or(_handle, x) { return new _Bool(this.value || x().value) }
}

const _true = new _Bool(true);
const _false = new _Bool(false);

class _Int extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    add(_handle, x) { return new _Int(this.value + x().value); }
    sub(_handle, x) { return new _Int(this.value - x().value); }
    mul(_handle, x) { return new _Int(this.value * x().value); }
    div(_handle, x) { return new _Int(this.value / x().value); }
    rem(_handle, x) { return new _Int(this.value % x().value); }
    pow(_handle, x) { return new _Int(this.value ** x().value); }

    lt(_handle, x) { return new _Bool(this.value < x().value); }
    gt(_handle, x) { return new _Bool(this.value > x().value); }
    eq(_handle, x) { return new _Bool(this.value == x().value); }
    leq(_handle, x) { return new _Bool(this.value <= x().value); }
    geq(_handle, x) { return new _Bool(this.value >= x().value); }
    neq(_handle, x) { return new _Bool(this.value != x().value); }
}

function intToString(_handle, i) {
    return new _String(i().value.toString());
}

class _Char extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }
}

class _String extends _Any {
    constructor(value) {
        super();
        this.value = value;
    }

    add(_handle, x) { return new _String(this.value + x().value); }
}
