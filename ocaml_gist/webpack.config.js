const webpack = require('webpack');
const path = require('path');

module.exports = {
  plugins: [
    new webpack.DefinePlugin({
    'process.env': {
      NODE_ENV: JSON.stringify('production')
    }
  })],
  entry: {
    ocaml_gist: './lib/js/src/gist_tool.js',
    "src/output/ocaml_gist": './lib/js/src/gist_tool.js'
  },
  output: {
    path: path.join(__dirname, "./"),
    filename: '[name].js',
  },
};
