var server = "/game/controller.yaws";
function click(id) {
    try {
        $.getJSON(server,
                {'op': "click", 'id': id},
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
