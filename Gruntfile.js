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

        exampleFiles: [
            "example/*.purs",
            "<%=libFiles%>",
        ],

        clean: {
            lib: ["output"],
            tests: ["output/tests.js"],
            example: ["output/example.js"],
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
                dest: "output/tests.js",
            },
            example: {
                options: {
                    module: ["Main"],
                    main: true,
                },
                src: ["<%=exampleFiles%>"],
                dest: "output/example.js",
            },
        },

        express: {
            example: {
                options: {
                    script: 'output/example.js',
                    background: false,
                },
            },
        },

        execute: {
            tests: {
                src: ["output/tests.js"],
            },
        },

        watch: {
            tests: {
                files: ["<%=testsFiles%>"],
                tasks: ["clean:tests", "make", "psc:tests", "execute:tests"],
                options: {
                    interrupt: true,
                    atBegin: true,
                },
            },
            example: {
                files: ["<%=exampleFiles%>"],
                tasks: ["clean:example", "make", "psc:example", "express:example:stop", "express:example"],
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
    grunt.loadNpmTasks("grunt-execute");

    grunt.registerTask("make", ["clean:lib", "pscMake", "dotPsci"]);
    grunt.registerTask("test", ["clean:tests", "make", "psc:tests", "execute:tests"]);
    grunt.registerTask("example", ["clean:example", "make", "psc:example", "express:example"]);
    grunt.registerTask("default", ["test"]);
};
