/* ============================================================
 * bootstrap-rowlink.js j1
 * http://jasny.github.com/bootstrap/javascript.html#rowlink
 * ============================================================
 * Copyright 2012 Jasny BV, Netherlands.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * ============================================================
 *
 * Adapted by tuhlmann to use onclick functionality instead of a href
 * */

!function ($) {

  "use strict"; // jshint ;_;

  var Rowhandler = function (element, options) {
    options = $.extend({}, $.fn.rowhandler.defaults, options)
    var tr = element.nodeName == 'tr' ? $(element) : $(element).find('tr:has(td)')

    tr.each(function() {
      var link = $(this).find(options.target).first();
      if (!link.length) return;

      //var href = link.attr('href')
      var onclick = link.attr('onclick');

      $(this).find('td').not('.nolink').click(function() {
        //window.location = href;
        //console.log(link);
        var handler = new Function("event", onclick);
        if (typeof handler == "function") {
          handler.apply();
        }
      })

      $(this).addClass('rowlink')
      link.replaceWith(link.html())
    })
  }


 /* ROWLINK PLUGIN DEFINITION
  * =========================== */

  $.fn.rowhandler = function (options) {
    return this.each(function () {
      var $this = $(this)
      , data = null //$this.data('rowlink')
      if (!data) $this.data('rowlink', (data = new Rowhandler(this, options)))
    })
  }

  $.fn.rowhandler.defaults = {
    target: "a"
  }

  $.fn.rowhandler.Constructor = Rowhandler


 /* ROWHANDLER DATA-API
  * ================== */

  $(function () {
    $('[data-rowhandler="rowlink"]').each(function () {
      $(this).rowhandler($(this).data())
    })
  })

}(window.jQuery)
