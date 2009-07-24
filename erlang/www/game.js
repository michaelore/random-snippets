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
                function(data) {
                try {
                $("#" + data.id).attr({'src': data.src});
                } catch (e) {
                alert(e);
                }})
    } catch(e) {
        alert(e);
    }
}
