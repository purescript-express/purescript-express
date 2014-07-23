module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
        libFiles: [
            "src/**/*.purs",
            "bower_components/purescript-*/src/**/*.purs",
            "bower_components/purescript-*/src/**/*.purs.hs",
        ],

        testsFiles: [
            "tests/*.purs",
            "<%=libFiles%>",
        ],

        clean: {
            lib: ["js", "externs"],
            tests: ["tmp"],
        },

        pscMake: ["<%=libFiles%>"],
        dotPsci: ["<%=libFiles%>"],

        psc: {
            tests: {
                options: {
                    module: ["Main"],
                    main: true,
                },
                src: ["<%=testsFiles%>"],
                dest: "tmp/tests.js",
            },
        },

        express: {
            tests: {
                options: {
                    script: 'tmp/tests.js',
                    background: false,
                },
            },
        },

        watch: {
            tests: {
                files: ["<%=testsFiles%>"],
                tasks: ["express:tests:stop", "default"],
                options: {
                    interrupt: true,
                    atBegin: true,
                },
            },
        },
    });

    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-express-server");

    grunt.registerTask("make", ["pscMake", "dotPsci"]);
    grunt.registerTask("test", ["clean:tests", "psc:tests", "express:tests"]);
    grunt.registerTask("default", ["make", "test"]);
};
