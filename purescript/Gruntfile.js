module.exports = function(grunt) {
  grunt.registerTask("rectangle", ["psc:rectangle"]);

  "use strict";

  grunt.initConfig({

    srcFiles: [
      "bower_components/**/src/**/*.purs",
      "../dependencies/Control/Monad/Eff/DOM.purs"
    ],

    psc: {
      options: {
        modules: ["Main"]
      },
      chat: {
      src: ["src/Chat.purs", "<%=srcFiles%>"],
        dest: "../templates/chat.julius"
      },
       problem: {
      src: ["src/prob.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
       rectangle: {
      src: ["src/Rectangle.purs", "<%=srcFiles%>"],
        dest: "../templates/playback.julius"
      },
      shapes: {
      src: ["src/Shapes.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      random: {
      src: ["src/Random.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      refs: {
      src: ["src/Refs.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      },
      lsystem: {
      src: ["src/LSystem.purs", "<%=srcFiles%>"],
        dest: "dist/Main.js"
      }
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("chat", ["psc:chat"]);
  grunt.registerTask("rectangle", ["psc:rectangle"]);
  grunt.registerTask("problem", ["psc:problem", "dotPsci"]);
  grunt.registerTask("shapes",    ["psc:shapes"]);
  grunt.registerTask("random",    ["psc:random"]);
  grunt.registerTask("refs",      ["psc:refs"]);
  grunt.registerTask("lsystem",   ["psc:lsystem"]);
  grunt.registerTask("default",   ["psc:rectangle", "dotPsci"]);
};
