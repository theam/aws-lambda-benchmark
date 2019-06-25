const path = require("path");
const slsw = require("serverless-webpack");

module.exports = {
	mode: slsw.lib.webpack.isLocal ? "development" : "production",
	entry: slsw.lib.entries,
	resolve: {
		extensions: [".js", ".json", ".ts", ".tsx"]
	},
	output: {
		libraryTarget: "commonjs",
		path: path.join(__dirname, ".webpack"),
		filename: "[name].js"
	},
	target: "node",
	externals: [{ "aws-sdk": "commonjs aws-sdk" }],
	module: {
		rules: [
			{
				test: /\.ts(x?)$/,
				use: [
					{
						loader: "ts-loader"
					}
				]
			}
		]
	},
	devtool: process.env.DEBUGGER ? "source-map" : ""
};
