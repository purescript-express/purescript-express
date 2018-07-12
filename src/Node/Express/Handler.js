// module Node.Express.Handler

exports._nextWithError = function (nxt, err) {
    return function () {
        nxt(err);
    };
};
