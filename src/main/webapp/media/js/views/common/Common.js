App.namespace("views.common");
App.views.common.Common = (function($) {
  "use strict";

  var inst = {};

  inst.init = function() {
    inst.initCollapsible();
    inst.initTooltips();
    inst.initModals();
    //inst.initPopovers();
  };

  inst.initCollapsible = function() {
    $('.collapse-master').on('hidden.bs.collapse', function () {
      if ( !$('.collapse-master.in').length ) { $('.collapse-detail').collapse('show'); }
    });

    $('.collapse-master').on('shown.bs.collapse', function () {
      if ( $('.collapse-detail.in').length ) { $('.collapse-detail').collapse('hide'); }
    });

    $('.collapse-detail').on('hidden.bs.collapse', function () {
      if ( !$('.collapse-detail.in').length ) { $('.collapse-master').collapse('show'); }
    });

    $('#master-detail-accordion').on('show.bs.collapse hide.bs.collapse', function(e){
      $(e.target).siblings('.panel-heading').find('.accordion-toggle i').toggleClass('icon-double-angle-down icon-double-angle-up', 200);
    });

  };

  inst.initTooltips = function() {
    $("[data-toggle='tooltip']").tooltip();
  };

  inst.initPopovers = function() {
    $(document).on('hidden.bs.popover', function (e) {
      $(e.target).remove();
    });
  };

  inst.initModals = function() {
    $(document).on('hidden.bs.modal', function (e) {
      $(e.target).remove();
    });
  };

  // inst.setupEditorField = function(fieldId) {
    // CKEDITOR.replace(fieldId, {
      // fullPage : true,
      // toolbar : 'MyToolbar',
      // autoUpdateElement: true
    // });
  // };

  inst.buildOptions = function(isSimpleEditor, docUploadCallback) {
    var options = {
      plugins: ['fullscreen'],
      minHeight: 200,
      autoresize: false,
      buttons: ['formatting', '|', 'bold', 'italic', 'underline', 'deleted', '|',
                'unorderedlist', 'orderedlist', 'outdent', 'indent', '|',
                'image', 'table', '|',
                'fontcolor', 'backcolor', '|', 'alignment', '|', 'horizontalrule']
    };

    if (!isSimpleEditor) {
      options.imageUpload = '/api/imgup';
      options.fileUpload  = '/api/fileup';
      options.imageUploadCallback = function(image, json) {
        $(image).attr('alt', json.name);
        docUploadCallback(json.guid);
      };
      options.fileUploadCallback = function(link, json) {
        log(link);
        $(link).html(json.name);
        docUploadCallback(json.guid);
      };
      options.buttons = ['html', '|', 'formatting', '|', 'bold', 'italic', 'underline', 'deleted', '|',
                         'unorderedlist', 'orderedlist', 'outdent', 'indent', '|',
                         'image', 'file', 'table', 'link', '|',
                         'fontcolor', 'backcolor', '|', 'alignment', '|', 'horizontalrule'];
    }

    return options;
  };

  inst.setupEditorField = function(fieldId, isSimpleEditor, docUploadCallback) {
    $('#'+fieldId).redactor( inst.buildOptions(isSimpleEditor, docUploadCallback) );
  };

  inst.updateEditorOnSubmit = function(fieldId, isSimpleEditor, callAfterUpdate) {
    inst.updateEditorField(fieldId);
    callAfterUpdate();
  };

  inst.updateEditorField = function(fieldId, isSimpleEditor) {
    var data = $('#'+fieldId).redactor('get');
    //console.log("REDACTOR: "+data);
    // if (isSimpleEditor) {
      // var markdown = new reMarked().render(data);
      // console.log("MD: "+markdown);
      // $('#'+fieldId).val(markdown);
    // } else {
      data = data.replace(/<table>/gi, '<table class="table">');
      $('#'+fieldId).val(data);
    // }

  };

  // inst.updateEditorField = function(fieldId) {
    // //CKEDITOR.instances[fieldId].updateElement();
    // var data = CKEDITOR.instances[fieldId].getData();
    // //console.log("CKEDITOR: "+data);
    // var markdown = new reMarked().render(data);
    // //console.log("MD: "+markdown);
    // $('#'+fieldId).val(markdown);
//
    // //var converter = new Showdown.converter();
    // //var html = converter.makeHtml(markdown);
    // //console.log("Back to HTML: "+html);
  // };

  return inst;
}(jQuery));
