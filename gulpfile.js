/* jshint node: true */

"use strict";

var gulp = require("gulp");
var purescript = require("gulp-purescript");
var less = require("gulp-less");
var uglify = require("gulp-uglify");

var sources = [
    "src/**/*.purs",
    "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
    "src/**/*.js",
    "bower_components/purescript-*/src/**/*.js"
];

var sourcesCli = [
    "bower_components/**/src/**/*.purs",
    "src/Types.purs",
    "src/Transformer.purs",
    "src/Level.purs",
    "src/Helper.purs",
    "src/Unsafe.purs",
    "src/Levels/*.purs",
    "leveleditor/*.purs"
];

gulp.task("make", function() {
    return gulp.src(sources)
        .pipe(purescript.psc({
            main: "Main",
            module: ["Main"],
            ffi: foreigns,
            output: "main.js"
        }))
        .pipe(gulp.dest("dist"));
});

gulp.task("cli", function() {
    return gulp.src(sourcesCli)
        .pipe(purescript.psc({
            main: "Main",
            module: ["Main"],
            ffi: foreigns,
            output: "cli.js"
        }))
        .pipe(gulp.dest("dist"));
});

gulp.task("dotpsci", function () {
    return gulp.src(sourcesCli)
        .pipe(purescript.dotPsci({
            ffi: foreigns
        }));
});

gulp.task("less", function() {
    return gulp.src("css/*.less")
        .pipe(less({}))
        .pipe(gulp.dest("dist"));
});

gulp.task("compress", ["make"], function() {
    return gulp.src("dist/main.js")
        .pipe(uglify())
        .pipe(gulp.dest("dist"));
});

gulp.task("prod", ["less", "dotpsci", "make", "compress"]);
gulp.task("default", ["less", "dotpsci", "make"]);
