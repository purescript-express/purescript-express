import cp from "cookie-parser";

// we could write this middleware as
// `export function _cookieParser("cookie-parser")()`
// but this variant is better for tree-shaking
export function _cookieParser(req, res, nxt) {
    return cp()(req, res, nxt);
}
