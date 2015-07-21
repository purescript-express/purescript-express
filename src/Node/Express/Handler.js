// module Node.Express.Handler

exports.reqRoute = function (req) {
    return function () {
        return req.route.path;
    };
};
