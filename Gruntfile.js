module.exports = function(grunt) {
    "use strict";

    grunt.initConfig({
        appFiles: [
            "src/*.purs",
            "src/**/*.purs",
            "bower_components/purescript-*/src/**/*.purs",
            "bower_components/purescript-*/src/**/*.purs.hs",
        ],

        clean: {
            app: ["js", "externs"],
        },

        pscMake: {
            app: {
                src: ["<%=appFiles%>"],
                dest: "js/node_modules",
            },
        },

        dotPsci: {
            src: ["<%=appFiles%>"],
        },

        execute: {
            app: {
                src: ['js/run.js'],
            },
        },
    });

    grunt.loadNpmTasks("grunt-contrib-clean");
    grunt.loadNpmTasks("grunt-purescript");
    grunt.loadNpmTasks("grunt-execute");

    grunt.registerTask("make", ["pscMake:app", "dotPsci"]);
    grunt.registerTask("run", ["make", "execute:app"]);
    grunt.registerTask("default", ["make"]);
};
