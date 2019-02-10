App.namespace("views.common");
App.views.common.Upload = (function($) {
  "use strict";

  var inst = {};

  inst.init = function() {
  };

  inst.renderUploadedDocument = function(blockId, file) {
    // console.log("Append: "+file.name);
    // &nbsp;<a href='Javascript://' onclick='"+file.delete_func+"' ><i class='icon-remove'></i></a>
    $("#files_"+blockId).append("<p id='"+file.elemId+"'>Gespeichert: "+file.name+"</p>");
  };

  inst.initFileupload = function(selector, uniqueId, doneFunc) {
    $(selector).fileupload({
      url: '/api/upload/',
      dataType: 'json',
      done: function (e, data) {
        //console.log(data);
        doneFunc();
        //$.each(data.result.files, function (index, file) {
        //  inst.renderUploadedDocument(uniqueId, file)
        //});
      },
      progressall: function (e, data) {
        var progress = parseInt(data.loaded / data.total * 100, 10);
        $('#prg_'+uniqueId+' .bar').css('width', progress + '%');
      }
    });
  };

  return inst;
}(jQuery));
