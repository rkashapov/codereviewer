const webpack = require("webpack");
const path = require("path");
const CompressionPlugin = require('compression-webpack-plugin');

const MODE =
  process.env.npm_lifecycle_event === "build" ? "production" : "development";

module.exports = function(env) {
  return {
    mode: MODE,
    entry: path.resolve(__dirname, "./src/frontend/main.js"),
    output: {
      path: path.resolve(__dirname, "static"),
      filename: "bundle.js"
    },
    module: {
      rules: [
        {
          test: [/\.elm$/],
          exclude: [/elm-stuff/, /node_modules/],
          use: [
            {
              loader: "elm-webpack-loader",
              options:
                MODE === "production" ? {
                  optimize: true,
                  runtimeOptions: ['-A128M', '-H128M', '-n8m'],
                } : { debug: false }
            }
          ]
        }
      ]
    },
    resolve: {
      extensions: [".js", ".elm"]
    },
    plugins: [new CompressionPlugin()],
  };
};