// module Node.Express.Internal.Utils

exports.nextWithError = function (nxt, err) {
    return function () {
        nxt(err);
    };
};
