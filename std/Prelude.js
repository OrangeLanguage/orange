const _handle = (x) => { console.log(`Unhandled effect ${x}`); }

class Bool {
    constructor(value) {
        this.value = value;
    }

    toString() {
        return this.value.toString();
    }

    if(_handle, t, f) { 
        if (this.value) {
            return t();
        } else {
            return f();
        }
     }

    and(_handle, x) { return new Bool(this.value && x().value) }
    or(_handle, x) { return new Bool(this.value || x().value) }
}

const _true = new Bool(true);
const _false = new Bool(false);

class Int {
    constructor(value) {
        this.value = value;
    }

    toString() {
        return this.value.toString();
    }

    add(_handle, x) { return new Int(this.value + x().value); }
    sub(_handle, x) { return new Int(this.value - x().value); }
    mul(_handle, x) { return new Int(this.value * x().value); }
    div(_handle, x) { return new Int(this.value / x().value); }
    rem(_handle, x) { return new Int(this.value % x().value); }
    pow(_handle, x) { return new Int(this.value ** x().value); }

    lt(_handle, x) { return new Bool(this.value < x().value); }
    gt(_handle, x) { return new Bool(this.value > x().value); }
    eq(_handle, x) { return new Bool(this.value == x().value); }
    leq(_handle, x) { return new Bool(this.value <= x().value); }
    geq(_handle, x) { return new Bool(this.value >= x().value); }
    neq(_handle, x) { return new Bool(this.value != x().value); }
}

class Char {
    constructor(value) {
        this.value = value;
    }

    toString() {
        return this.value.toString();
    }
}

class String {
    constructor(value) {
        this.value = value;
    }

    toString() {
        return this.value.toString();
    }

    add(_handle, x) { return new String(this.value + x().value); }
}
