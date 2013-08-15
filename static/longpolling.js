// Updates a div with a specified id, by polling an url,
// which should return a new div, with the same id.

connfails=0;

longpollcallbacks = $.Callbacks();

function longpoll(url, divid, cont, fail) {
	$.ajax({
		'url': url,
		'dataType': 'html',
		'success': function(data, status, jqxhr) {
			$('#' + divid).replaceWith(data);
			longpollcallbacks.fire();
			connfails=0;
			cont();
		},
		'error': function(jqxhr, msg, e) {
			connfails=connfails+1;
			if (connfails > 3) {
				fail();
			}
			else {
				cont();
			}
		}
	});
}
