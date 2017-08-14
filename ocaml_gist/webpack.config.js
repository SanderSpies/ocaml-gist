const path = require('path');

module.exports = {
  entry: {
    ocaml_gist: './lib/js/src/gist_tool.js',
  },
  output: {
    path: path.join(__dirname, "./"),
    filename: '[name].js',
  },
};
