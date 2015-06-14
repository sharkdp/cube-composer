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
    "bower_components/**/src/**/*.purs",
    "src/Types.purs",
    "src/Transformer.purs",
    "src/Level.purs",
    "src/Helper.purs",
    "src/Unsafe.purs",
    "src/Levels/*.purs",
    "leveleditor/*.purs"
];

gulp.task("clean-docs", function(cb) {
    rimraf("docs", cb);
});

gulp.task("clean-dist", function(cb) {
    rimraf("dist", cb);
});

gulp.task("clean", ["clean-docs", "clean-dist"]);

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

gulp.task("concat", ["make"], function() {
    return gulp.src([
            "bower_components/Sortable/Sortable.min.js",
            "bower_components/isomer/dist/isomer.min.js",
            "dist/main.js"
        ])
        .pipe(concat("main.js"))
        .pipe(gulp.dest("dist"));
});

gulp.task("compress", ["concat"], function() {
    return gulp.src("dist/main.js")
        .pipe(uglify())
        .pipe(gulp.dest("dist"));
});

gulp.task("docs", ["clean-docs"], function () {
    return gulp.src(sources)
        .pipe(purescript.pscDocs({
            docgen: {
                "DOMHelper": "docs/DOMHelper.md",
                "Helper": "docs/Helper.md",
                "Isomer": "docs/Isomer.md",
                "Level": "docs/Level.md",
                "Sortable": "docs/Sortable.md",
                "Storage": "docs/Storage.md",
                "Transformer": "docs/Transformer.md",
                "Types": "docs/Types.md",
                "Unsafe": "docs/Unsafe.md"
            }
        }));
});

gulp.task("prod", ["clean", "less", "dotpsci", "make", "concat", "compress", "docs"]);
gulp.task("dev", ["less", "dotpsci", "make", "concat"]);
gulp.task("default", ["less", "dotpsci", "make", "concat", "docs"]);
