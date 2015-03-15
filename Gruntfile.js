module.exports = function(grunt) {
    grunt.initConfig({
        pkg: grunt.file.readJSON('package.json'),

        jshint: {
            files: ['js/*.js', 'test/*.js'],
            options: {
                jshintrc: '.jshintrc'
            }
        },

        browserify: {
            dev: {
                src: ['js/io.js'],
                dest: 'dist/cube-composer-bundle.js',
                options: {
                    browserifyOptions: {
                        debug: true
                    }
                }
            },
            prod: {
                src: ['js/io.js'],
                dest: 'dist/cube-composer-bundle.js'
            }
        },

        uglify: {
            dist: {
                files: {
                    'dist/cube-composer-bundle.js': 'dist/cube-composer-bundle.js'
                }
            }
        },

        mochaTest: {
            test: {
                src: ['test/*.js']
            }
        },

        watch: {
            files: ['Gruntfile.js', 'js/*.js', 'test/*.js'],
            tasks: ['dev']
        }
    });

    grunt.loadNpmTasks('grunt-contrib-jshint');
    grunt.loadNpmTasks('grunt-browserify');
    grunt.loadNpmTasks('grunt-contrib-uglify');
    grunt.loadNpmTasks('grunt-mocha-test');
    grunt.loadNpmTasks('grunt-contrib-watch');

    grunt.registerTask('dev', ['jshint', 'mochaTest', 'browserify:dev']);
    grunt.registerTask('prod', ['jshint', 'mochaTest', 'browserify:prod', 'uglify']);
    grunt.registerTask('default', 'prod');
};
