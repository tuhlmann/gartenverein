App.namespace("views.management");
App.views.management.Team = (function($) {
  "use strict";

  var inst = {};

  inst.init = function() {
    App.views.common.Common.init();
  };

  inst.resetInvitationForm = function() {
    $('[name=invitation-widget]').find('form')[0].reset();
  }

  return inst;
}(jQuery));
