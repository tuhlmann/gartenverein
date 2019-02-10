App.namespace("views.user");
App.views.user.Dashboard = (function($) {
  "use strict";

  var inst = {};

  inst.init = function() {
    log('Initialize Dashboard Page');
  };

  inst.closeMultiSearch = function() {
    $('#db-multi-search-results').fadeOut('fast');
  };

  return inst;
}(jQuery));
