exports.evalString = function (s) {
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
                return "Unhandled effect " + e.effect.toString(() => x => x().value);
            } else {
                return e.toString();
            }
        }
    }
};