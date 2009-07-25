var server = "/game/controller.yaws";
var client_id;
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

function request_update() {
    $.getJSON(server,
            {'op': "update", 'id': client_id},
            function(mat) {
            $.each(mat, function update);
            });
}

document.onload(function() {
	$.getJSON(server,
	    {'op': "hello"},
	    function(obj) {
	    $.each(obj.mat, function update);
            client_id = obj.id;
	    });
        setInterval("request_update", 50);
	});
