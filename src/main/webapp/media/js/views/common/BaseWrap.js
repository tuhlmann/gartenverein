App.namespace("views.common");
App.views.common.BaseWrap = (function($) {
  "use strict";

  var inst = {};

  inst.initSidebarToggle = function() {
    $('#sidebar-menu-toggle').click(function() {
      $('#sidebar-menu-toggle i').toggleClass('icon-chevron-left icon-chevron-right');
      $('#application-sidebar').animate({
        width: 'toggle'
      }, 0, function() {
        $(document).trigger("sidebar-toggle");
      });
      $('#application-content').toggleClass('col-md-12 col-md-9');
      $('#application-content').toggleClass('no-sidebar');
    });
  };

  inst.init = function() {
    inst.initSidebarToggle();
  };

  return inst;
}(jQuery));
