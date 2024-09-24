import express from "express";

export function _static(root) {
    return express.static(root)
}
