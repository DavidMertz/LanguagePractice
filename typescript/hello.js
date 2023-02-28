#!/usr/bin/env node
var __makeTemplateObject = (this && this.__makeTemplateObject) || function (cooked, raw) {
    if (Object.defineProperty) { Object.defineProperty(cooked, "raw", { value: raw }); } else { cooked.raw = raw; }
    return cooked;
};
function greeter(person) {
    return "Hello, " + person;
}
var user = "Jane User";
console.log(greeter(user));
console.log(__makeTemplateObject(["Foo   | Bar\n             ", "  | ", "\n             ", "  | ", "\n             "], ["Foo   | Bar\n             ", "  | ", "\n             ", "  | ", "\n             "]), 1, 2, 3, 4);
