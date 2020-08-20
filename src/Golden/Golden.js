const diff = require("diff");

function diffImpl(first) {
  return second => diff.diffLines(first, second).filter(x => x.added || x.removed);
}

exports.diffImpl = diffImpl;