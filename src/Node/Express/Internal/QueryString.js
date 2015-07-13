// module Node.Express.Internal.QueryString

exports.decode = function(str) {
    try {
        return decodeURIComponent(str.replace(/\+/g, ' '));
    } catch(err) {
        return str;
    }
}