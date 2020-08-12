const chalk = require('chalk');

exports.orange = chalk.keyword('orange');
exports.cyan = c => chalk.cyanBright(c);
exports.green = c => chalk.greenBright(c);
exports.blue = c => chalk.blueBright(c);