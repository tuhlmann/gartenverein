App.namespace("views.user");
App.views.user.Events = (function($) {
  "use strict";

  var inst = {};

  var EventsBridge = function() {};

  var CalendarInstance = {};

  inst.visibleEvents = [];

  inst.init = function() {
    App.views.common.Common.init();
    inst.initFullCalendar();
    inst.listenSidebarToggle();
  };

  inst.setupEndDateToggle = function(formGuid) {
    $(document).on('event-allday-toggle', function(event, elem){
      if ($(elem).is(':checked')) {
        $('#'+formGuid+' .end-date-input').hide();
        $('#'+formGuid+' .start-date-input .time-input').hide();
      } else {
        $('#'+formGuid+' .end-date-input').show();
        $('#'+formGuid+' .start-date-input .time-input').show();
        $('#'+formGuid+' .time-input').css('display', 'inline-table');
      }
    });
  };

  inst.setEventsBridge = function(bridge) {
    App.views.user.Events.EventsBridge = bridge;
  };

  inst.initFullCalendar = function() {
    var date = new Date();
    var d = date.getDate();
    var m = date.getMonth();
    var y = date.getFullYear();

    App.views.user.Events.CalendarInstance = $('#appointments');
    App.views.user.Events.CalendarInstance.fullCalendar({
      header: {
        left: 'prev,next today',
        center: 'title',
        right: 'month,agendaWeek,agendaDay'
      },
      firstDay: 1,
      timeFormat: 'H(:mm)',
      monthNames: ['Januar', 'Februar', 'März', 'April', 'Mai', 'Juni', 'Juli',
                   'August', 'September', 'Oktober', 'November', 'Dezember'],
      monthNamesShort: ['Jan', 'Feb', 'Mär', 'Apr', 'Mai', 'Jun', 'Jul', 'Aug', 'Sep', 'Okt', 'Nov', 'Dez'],
      dayNames: ['Sonntag', 'Montag', 'Dienstag', 'Mittwoch', 'Donnerstag', 'Freitag', 'Samstag'],
      dayNamesShort: ['So', 'Mo', 'Di', 'Mi', 'Do', 'Fr', 'Sa'],
      columnFormat: {
        month: 'ddd',    // Mon
        week: 'ddd, d.M', // Mon 9/7
        day: 'dddd, d.M'  // Monday 9/7
      },
      buttonText: {
        today:    'Heute',
        month:    'Monat',
        week:     'Woche',
        day:      'Tag'
      },
      allDayText: 'Tag',
      axisFormat: 'H:mm',
      selectable: false,
      dayClick: function(date, allDay, jsEvent, view) {
        if (allDay) {
          //log('Clicked on the entire day: ' + date);
          var now = moment();
          var dt = moment(date);
          dt.hour(now.hour()+1);
          appointments_createEvent(dt.valueOf(), true);
        }else{
          //log('Clicked on the slot: ' + date);
          appointments_createEvent(date.getTime(), false);
        }
      },
      events: function(start, end, callback) {
        inst.visibleEvents = [];
        /**
         * Call a REST API function that takes the dates and returns the same list of events
         * as the event bridge
         */
        App.views.user.Events.EventsBridge({start: start.getTime(), end: end.getTime()}, callback);
      },
      eventRender: function(event, element) {
        //log(element);
        inst.visibleEvents.push(event.id);
        $(element).addClass('popover-holder');
        element.popover({
          html: 'true',
          title : '<span class="text-info"><strong>'+event.title+'</strong>&nbsp;</span>'+
                  '<span><button type="button" class="close" style="line-height:15px;" onclick="App.views.user.Events.closePopover();">&nbsp;&times;</button></span>',
          content: inst.popoverContent(event),
          container: 'body',
          placement: 'bottom'
        });
      }
    });
  };

  inst.addEvent = function(event) {
    if (_.contains(inst.visibleEvents, event.id)) {
      App.views.user.Events.CalendarInstance.fullCalendar( 'removeEvents', event.id );
      App.views.user.Events.CalendarInstance.fullCalendar( 'renderEvent', event );
    } else {
      App.views.user.Events.CalendarInstance.fullCalendar( 'renderEvent', event );
    }
  };

  inst.removeEvent = function(id) {
    App.views.user.Events.CalendarInstance.fullCalendar( 'removeEvents', id );
  };

  inst.rerenderCalendar = function() {
    App.views.user.Events.CalendarInstance.fullCalendar( 'render' );
  };

  inst.listenSidebarToggle = function() {
    $(document).on("sidebar-toggle", function(){
      window.setTimeout(function(){
        inst.rerenderCalendar();
      },0);
    });
  };

  inst.closePopover = function() {
    $(".popover-holder").popover("hide");
    $(".popover").remove();
  };

  inst.popoverContent = function(event) {
    var txt = '<div><div>'+event.desc+'</div><hr/>'+
              '<div>Autor: '+event.author+'</div>'+
              '<div>Beginn: '+inst.formatDate(event.start, event.allDay)+'</div>'+
              '<div>'+inst.endDateOrAllDay(event.allDay, event.end)+'</div>'+
              '<br/><div class="pull-right">';

    if (event.can_edit) {
      txt += '<button type="button" class="btn btn-mini btn-primary" onclick="App.views.user.Events.closePopover();'+event.editFunc+'">Bearbeiten</button>'+
             '<span>&nbsp;</span>';
    }
    if (event.can_delete) {
      txt += '<button type="button" class="btn btn-mini btn-danger" onclick="App.views.user.Events.closePopover();'+event.deleteFunc+'">Löschen</button>';
    }
    txt += '</div></div><div class="clearfix"></div>';
    return txt;
  };

  inst.formatDate = function(date, allDay) {
    if (allDay) {
      return moment(date).format("DD.MM.YYYY");
    } else {
      return moment(date).format("DD.MM.YYYY HH:mm");
    }
  };

  inst.endDateOrAllDay = function(allDay, end) {
    var d = moment(end);
    if ((!allDay) && (d !== null) && (d.isValid())) {
      return "Ende: " + inst.formatDate(end, false);
    } else {
      return "Ganztägiges Ereignis";
    }
  };

  return inst;
}(jQuery));
