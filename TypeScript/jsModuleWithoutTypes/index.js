'use strict';

function main(msg) {
  console.log(`
JS module without types
__dirname: ${__dirname}
msg: ${msg}
    `);
}

module.exports = main;
