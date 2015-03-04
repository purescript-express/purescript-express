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

        documentedFiles: [
            "src/Node/Express/*.purs",
            "src/Control/Monad/Eff/Class.purs",
            "src/Data/Default.purs",
        ],

        clean: {
            lib: ["output"],
            tests: ["output/tests.js"],
            examples: [
                "output/JSMiddleware.js",
                "output/ToDoServer.js",
                "output/EmbedApp.js",
                "output/Capture.js",
            ],
        },

        pscMake: ["<%=libFiles%>"],
        dotPsci: ["<%=libFiles%>"],

        psc: {
            options: {
                modules: ["Main"],
                main: true,
            },
            tests: {
                src: ["<%=testsFiles%>"],
                dest: "output/tests.js",
            },
            examples: {
                files: {
                    "output/JSMiddleware.js":
                        ["examples/JSMiddleware.purs", "<%=libFiles%>"],
                    "output/ToDoServer.js":
                        ["examples/ToDoServer.purs", "<%=libFiles%>"],
                    "output/EmbedApp.js":
                        ["examples/EmbedApp.purs", "<%=libFiles%>"],
                    "output/Capture.js":
                        ["examples/Capture.purs", "<%=libFiles%>"],
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
        },

        docgen: {
            docs: {
                src: "<%=documentedFiles%>",
                dest: "DOCS.md",
            },
        },
    });

    grunt.loadNpmTasks("grunt-contrib-watch");
    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-express-server");
    grunt.loadNpmTasks("grunt-execute");

    grunt.registerTask("make", ["clean:lib", "pscMake", "dotPsci"]);
    grunt.registerTask("test", ["clean:tests", "psc:tests", "execute:tests"]);
    grunt.registerTask("examples", ["clean:examples", "psc:examples"]);
    grunt.registerTask("default", ["make", "test"]);
};
