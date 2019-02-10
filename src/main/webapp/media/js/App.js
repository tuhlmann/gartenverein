(function(window, $) {
  "use strict";

  window.App = (function() {
    // the instance to return
    var inst = {};

    inst.init = function(settings) {
      inst.settings = settings;
    };

    /**
      * A convenience function for parsing string namespaces and
      * automatically generating nested namespace objects.
      *
      * Example:
      * App.namespace('modules.module2');
      *
      */
    inst.namespace = function(ns_string) {
      var parts = ns_string.split('.'),
          parent = inst,
          pl;

      pl = parts.length;
      for (var i = 0; i < pl; i++) {
        // create a property if it doesnt exist
        if (typeof parent[parts[i]] === 'undefined') {
          parent[parts[i]] = {};
        }
        parent = parent[parts[i]];
      }
      return parent;
    };

    return inst;
  }());
})(this, jQuery);


/*
(function(window, $) {

  window.app = {
    notices: {
      / **
        * This must be called to clear out notices div during ajax calls. For idMessages.
        * /
      clear: function() {
        $(".notices-container").each(function(index) {
          $(this).html("");
        });
        $("form div.control-group").each(function(index) {
          $(this).removeClass("error");
          $(this).removeClass("warning");
          $(this).removeClass("info");
        });
      },
      / **
        * Add css class to outer control-group div
        * /
      onError: function(id, type) {
        var it = $("#"+id);
        if (it.html()) {
          it.closest("div.control-group").addClass(type);
        }
      }
    },
    login: {
      timeoutRtn: 0,
      // monitor the password input field and select the yes_password radio if something has been entered
      monitorPassword: function() {
        if (!$("#yes_password").attr("checked")) {
          var pwd = $("#id_password").val();
          if (pwd.length > 0) {
            $("#yes_password").attr("checked", "checked");
          }
          else {
            app.login.startMonitor();
          }
        }
      },
      startMonitor: function() {
        self.timeoutRtn = setTimeout("app.login.monitorPassword()", 1000);
      }
    }
  };

})(this, jQuery);
*/