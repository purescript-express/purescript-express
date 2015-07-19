// module Test.Handler

exports.cwdJson = JSON.stringify(process.cwd());

exports.unsafeUpdateMapInPlace = function(map) {
    return function(key) {
        return function(newValue) {
            return function() {
                map[key] = newValue;
            };
        };
    };
}
