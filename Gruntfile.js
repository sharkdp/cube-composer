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
            io: {
                src: ['js/io.js'],
                dest: 'dist/cube-composer-bundle.js',
                options: {
                    debug: true
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
            tasks: ['jshint', 'mochaTest', 'browserify']
        }
    });

    grunt.loadNpmTasks('grunt-contrib-jshint');
    grunt.loadNpmTasks('grunt-browserify');
    grunt.loadNpmTasks('grunt-mocha-test');
    grunt.loadNpmTasks('grunt-contrib-watch');

    grunt.registerTask('test', ['jshint', 'mochaTest']);
    grunt.registerTask('default', ['browserify']);
};
