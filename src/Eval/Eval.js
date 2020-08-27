exports.evalString = function (s) {
    return function () {
        try {
            return eval(s).toString().value;
        } catch (e) {
            if (e.effect) {
                console.log("Unhandled effect", e.effect.toString().value);
            } else {
                console.log(e);
            }
        }
    }
};