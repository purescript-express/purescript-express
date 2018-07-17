exports.thirdPartMain = function(app) {
    return function() {
        app.get('/', function (req, res) {
            res.send("Hello world!")
        });
        return {};
    };
}
