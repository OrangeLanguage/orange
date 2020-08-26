exports.evalString = function (s) { 
    return function () { 
        return eval(s).toString((x) => { console.log(`Unhandled effect ${x}`); }).value; 
    } 
};