/* jshint node: true */

"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var less = require("gulp-less");
var uglify = require("gulp-uglify");
var concat = require("gulp-concat");
var rimraf = require("rimraf");

var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

var sourcesCli = [
    "bower_components/purescript-*/src/**/*.purs",
    "src/ListHelper.purs",
    "src/Types.purs",
    "src/Transformer.purs",
    "src/Levels.purs",
    "src/Helper.purs",
    "src/Unsafe.purs",
    "src/Levels/*.purs",
    "cli/*.purs"
];

gulp.task("clean-docs", function(cb) {
    rimraf("docs", cb);
});

gulp.task("clean-dist", function(cb) {
    rimraf("dist", cb);
});

gulp.task("clean", gulp.series("clean-docs", "clean-dist"));

gulp.task("psc", function() {
    return purescript.compile({
            src: sources,
            ffi: foreigns,
            output: "output/main"
        });
});

gulp.task("bundle", gulp.series("psc", function() {
    return purescript.bundle({
            src: "output/main/**/*.js",
            output: "dist/main.js",
            module: "Main",
            main: "Main"
        });
}));

gulp.task("psc:cli", function() {
    return purescript.compile({
            src: sourcesCli,
            ffi: foreigns,
            output: "output/cli"
        });
});

gulp.task("bundle:cli", gulp.series("psc:cli", function() {
    return purescript.bundle({
            src: "output/cli/**/*.js",
            output: "dist/cli.js",
            module: "Main",
            main: "Main"
        });
}));

gulp.task("psci", function () {
    return purescript.psci({
            src: sourcesCli,
            ffi: foreigns
        })
        .pipe(gulp.dest("."));
});

gulp.task("less", function() {
    return gulp.src("css/*.less")
        .pipe(less({}))
        .pipe(gulp.dest("dist"));
});

gulp.task("concat", gulp.series("bundle", function() {
    return gulp.src([
            "bower_components/Sortable/Sortable.min.js",
            "dist/main.js"
        ])
        .pipe(concat("main.js"))
        .pipe(gulp.dest("dist"));
}));

gulp.task("compress", gulp.series("concat", function() {
    return gulp.src("dist/main.js")
        .pipe(uglify())
        .pipe(gulp.dest("dist"));
}));

gulp.task("docs", gulp.series("clean-docs", function () {
    return purescript.docs({
            src: sources,
            docgen: {
                "DOMHelper": "docs/DOMHelper.md",
                "Helper": "docs/Helper.md",
                "Levels": "docs/Levels.md",
                "Sortable": "docs/Sortable.md",
                "Storage": "docs/Storage.md",
                "Transformer": "docs/Transformer.md",
                "Types": "docs/Types.md",
                "Unsafe": "docs/Unsafe.md"
            }
        });
}));

gulp.task("prod", gulp.series("clean", "less", "psci", "bundle:cli", "bundle", "concat", "compress", "docs"));
gulp.task("dev", gulp.series("less", "psci", "bundle", "concat"));
gulp.task("default", gulp.series("less", "psci", "bundle", "concat", "docs"));
