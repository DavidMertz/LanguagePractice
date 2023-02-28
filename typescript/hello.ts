#!/usr/bin/env node

function greeter(person: string) {
  return "Hello, " + person;
}
 
let user = "Jane User";
 
console.log(greeter(user));

console.log`Foo   | Bar
             ${1}  | ${2}
             ${3}  | ${4}
             `;

