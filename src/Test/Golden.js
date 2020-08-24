const diff = require("diff");

function clean(text) {
  return text.split(/\r?\n/g).join("\n");
}

function diffImpl(first) {
  return second => diff.diffLines(clean(first), clean(second)).filter(x => x.added || x.removed);
}

exports.diffImpl = diffImpl;