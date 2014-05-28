connfails=0;

longpollcallbacks = $.Callbacks();

// Updates a div with a specified id, by polling an url,
// which should return a new div, with the same id.
function longpoll_div(url, divid, cont, fail) {
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
			// It's normal to get 1 failure per longpolling
			// element when navigating away from a page.
			// So 12 allows up to 4 longpolling elements per
			// page.
			if (connfails > 12) {
				fail();
			}
			else {
				cont();
			}
		}
	});
}

function longpoll_data(url, cont) {
	$.ajax({
		'url': url,
		'dataType': 'text',
		'success': function(data, status, jqxhr) {
			connfails=0;
			cont(1, data);
		},
		'error': function(jqxhr, msg, e) {
			connfails=connfails+1;
			cont(0);
		}
	});
}

//EOF