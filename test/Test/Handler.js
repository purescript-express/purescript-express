// module Test.Handler

exports.cwd = process.cwd();

exports.unsafeUpdateMapInPlace = function(map) {
    return function(key) {
        return function(newValue) {
            return function() {
                map[key] = newValue;
            };
        };
    };
}
