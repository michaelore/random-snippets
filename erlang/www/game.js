var server = "/game/controller.yaws";
function click(id) {
    try {
	var op;
	if ($("#" + id).attr("src")=="/www/images/x.png") {
	    op = "clickx.png";
	} else {
	    op = "clicko.png";
	}
        $.getJSON(server,
                {'op': op, 'id': id},
		function update);
    } catch(e) {
        alert(e);
    }
}

function update(data) {
    $("#" + data.id).attr({'src': data.src});
}

document.onload(function() {
	$.getJSON(server,
	    {'op': "hello"},
	    function(list) {
	    $.each(list, function update);
	    });
	});
