module.exports = function(grunt) {
  
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
      src: ["src/Chat.purs", "src/WebSocket.purs", "<%=srcFiles%>"],
        dest: "../templates/chat.julius"
      },
       playback: {
      src: ["src/Playback.purs", "src/WebSocket.purs", "<%=srcFiles%>"],
        dest: "../templates/playback.julius"
      },
    },

    dotPsci: ["<%=srcFiles%>"]
  });

  grunt.loadNpmTasks("grunt-purescript");
  grunt.registerTask("chat", ["psc:chat"]);
  grunt.registerTask("playback", ["psc:playback"]);
  grunt.registerTask("default",   ["psc:playback", "dotPsci"]);
};
