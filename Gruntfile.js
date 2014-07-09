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
        dotPsci: ["<%=appFiles%>"],

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

        execute: {
            tests: {
                src: ['tmp/tests.js'],
            },
        },
    });

    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-execute");

    grunt.registerTask("make", ["pscMake", "dotPsci"]);
    grunt.registerTask("test", ["clean:tests", "psc:tests", "execute:tests"]);
    grunt.registerTask("default", ["test", "make"]);
};
