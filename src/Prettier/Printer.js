const chalk = require('chalk');

exports.colorize = c => str => {
    if (c == 'cyan') {
        return chalk.cyanBright(str);
    } else if (c == 'green') {
        return chalk.greenBright(str);
    } else if (c == 'blue') {
        return chalk.blueBright(str);
    } else if (c == 'yellow') {
        return chalk.yellow(str);
    } else if (c == 'orange') {
        return chalk.keyword('orange')(str);
    } else {
        return str;
    }
}