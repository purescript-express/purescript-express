// module Node.Express.Internal.Utils

exports.nextWithError = function (nxt, err) {
    return function () {
        nxt(err);
    };
};

exports.decodeURI = function(str) {
    try {
        return decodeURIComponent(str.replace(/\+/g, ' '));
    } catch(err) {
        return str;
    }
}
