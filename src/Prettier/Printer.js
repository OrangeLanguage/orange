const chalk = require("chalk");

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
    default:
      return str;
  }
};
