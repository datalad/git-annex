// Uses long-polling to update a div with a specified id,
// by polling an url, which should return a new div, with the same id.

connfails=0;

connfailed=
  '<div id="modal" class="modal fade">' +
  '  <div class="modal-header">' +
  '    <h3>git-annex has shut down</h3>' +
  '  </div>' +
  '  <div class="modal-body">' +
  '    You can now close this browser window.' +
  '  </div>' +
  '</div>' ;

function longpoll(url, divid) {
	(function( $ ) {
		$.ajax({
			'url': url,
			'dataType': 'html',
			'success': function(data, status, jqxhr) {
				$('#' + divid).replaceWith(data);
				connfails=0;
				return 1;
			},
			'error': function(jqxhr, msg, e) {
				connfails=connfails+1;
				if (connfails > 3) {
					// blocked by many browsers
					window.close();
					$('#modal').replaceWith(connfailed);
					$('#modal').modal('show');
					return 0;
				}
				else {
					return 1;
				}
			}
		});
	})( jQuery );
}
