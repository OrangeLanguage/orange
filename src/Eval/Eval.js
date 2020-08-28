exports.evalString = function evalString(s) {
    return function () {
        try {
            const e = eval(s);
            if (e !== undefined && e !== null) {
                return e.toString(() => x => x().value);
            } else {
                return String(e);
            }
        } catch (e) {
            if (e.effect) {
                throw new Error("Unhandled effect " + e.effect.toString(() => x => x().value));
            } else {
                throw new Error(e.toString());
            }
        }
    }
};