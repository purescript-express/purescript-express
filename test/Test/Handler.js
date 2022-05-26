export const cwdJson = JSON.stringify(process.cwd());

export function unsafeUpdateMapInPlace(map) {
    return function (key) {
        return function (newValue) {
            return function () {
                map[key] = newValue;
            };
        };
    };
}

export function unsafeStringify(x) {
    return JSON.stringify(x);
}
