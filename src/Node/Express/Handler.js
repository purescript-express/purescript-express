export function _nextWithError(nxt, err) {
    return () => { nxt(err); };
};
