const path = require('path');

module.exports = {
    entry: {
        app: "./src/Main.purs",
        test: "./test/Main.purs"
    },
    mode: "development",
    devtool: "source-map",
    module: {
        rules: [{
            test: /\.purs$/,
            exclude: /node_modules/,
            use: [
                {
                    loader: 'purs-loader',
                    options: {
                        src: [
                            'bower_components/purescript-*/src/**/*.purs',
                            'src/**/*.purs',
                            'test/**/*.purs'
                        ],
                        bundle: false
                    }
                }
            ]            
        }]
    },
    target: 'node',
    resolve: {
        extensions: [".purs", ".js"]
    },
    output: {
        filename: "[name].js",
        path: path.resolve(__dirname, "dist")
    }
}