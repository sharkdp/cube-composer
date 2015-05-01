module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
        pkg: grunt.file.readJSON("package.json"),

        src: [
            "bower_components/**/src/**/*.purs",
            "src/**/*.purs"
        ],

        srcPsci: [
            "bower_components/**/src/**/*.purs",
            "src/Types.purs",
            "src/Transformer.purs",
            "src/Solver.purs"
        ],

        srcCli: [
            "bower_components/**/src/**/*.purs",
            "src/Types.purs",
            "src/Transformer.purs",
            "src/Solver.purs",
            "src/Level.purs",
            "src/Levels/*.purs",
            "leveleditor/cli.purs"
        ],

        uglify: {
            dist: {
                files: {
                    "dist/main.js": "dist/main.js"
                }
            }
        },

        psc: {
            options: {
                "main": "Main",
                "modules": ["Main"]
            },

            all: {
                src: ["<%=src%>"],
                dest: "dist/main.js"
            },

            cli: {
                src: ["<%=srcCli%>"],
                dest: "dist/cli.js"
            }
        },

        dotPsci: ["<%=srcPsci%>"],

        watch: {
            files: ["Gruntfile.js", "<%=src%>"],
            tasks: ["dev"]
        }
    });

    grunt.loadNpmTasks("grunt-contrib-uglify");
    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-contrib-watch");

    grunt.registerTask("dev", ["psc:all", "dotPsci"]);
    grunt.registerTask("prod", ["psc", "uglify"]);
    grunt.registerTask("default", "dev");
};
