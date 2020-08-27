const chalk = require("chalk");
const highlight = require("cli-highlight").highlight;

exports.highlightJS = (source) => highlight(source, { language: "js" });

exports.colorize = (c) => (str) => {
  switch (c) {
    case "cyan":
      return chalk.cyanBright(str);
    case "green":
      return chalk.greenBright(str);
    case "blue":
      return chalk.blueBright(str);
    case "yellow":
      return chalk.yellow(str);
    case "orange":
      return chalk.keyword("orange")(str);
    case "red":
      return chalk.redBright(str);
    default:
      return str;
  }
};
