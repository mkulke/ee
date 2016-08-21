//
//  watch and build
//
var exec = require('child_process').exec;
var chokidar = require('chokidar');
var historyApiFallback = require('connect-history-api-fallback');

var source_paths = ["src/*.elm"];

var make = function () {
    exec("elm-make src/Main.elm --warn --output=public/main.js --yes", function(err, stdout, stderr){
        if (err) console.log(stderr);
        else console.log(stdout);
    });
}

// initial make
make();

chokidar.watch(source_paths, {ignored: /[\/\\]\./, ignoreInitial: true}).on('all', function(event, path) {

    // clear the terminal
    process.stdout.write('\u001B[2J\u001B[0;0f');

    // run the Elm compiler
    make();
});

//
//  browser sync
//
var browserSync = require('browser-sync');

browserSync({
    server: "public",
    files: ["public/*"],
    port: 8001,
    open: false,
    notify: false,
    middleware: [ historyApiFallback() ]
});
