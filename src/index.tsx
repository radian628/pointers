import { allocateStringLiterals, concatArrayBuffers } from "./allocate";
import { parseStatementList } from "./parser";
import { ParseInput } from "./parser-utils";
import { run } from "./runtime/run";
import {
  DefaultPrimitives,
  DefaultTypes,
  ExecutionContext,
} from "./runtime/runtime";
import { TypecheckContext, formatDiagnostic } from "./typecheck";

/*
High priority:
TODO: Implement arrays
TODO: Prevent parsing bug with variables starting with "if", "while", etc.
TODO: 

Med priority:
TODO: Implement print with format codes (actual printf)
TODO: Implement an "isAssignableTo" function which can be used in several typechecking contexts
- return types
- assignments
- function argument passing
TODO: Implement an "isCondition" function for implicit casting to booleans
- used for logical ops
- loop conditions
TODO: ++ and -- operators, both prefix and postfix.

Low priority:
TODO: Test structs and fix the ridiculous amount of inevitable bugs
TODO: struct members
*/

/*
Syntax highlighting and backtracking:
Currently there's a potential issue with syntax highlighting causing problems
with backtracking. I think the solution to this is to just discard ranges earlier in the list
which overlap with ones later in the list.

*/

// const TEST1 = `

// while (x > 3) {
//     x += 1;
// }

// for (int i = 0; i < 100; i += 1) {
//     b += i;
//     c += 2 * i;
// }

// struct person {
//     char* name;
//     int age;
// }

// int main(int argc, char** argv) {
//     int x = 0;
//     x = 3;
//     x += 1;

//     if (x > 3) {
//         x *= 1;
//     } else if (x < 0) {
//         x += cos(69.123);
//     } else {
//         printf(fart.poopy);
//     }

//     printf(x);
// }

// `;

// const TEST2 = `

// char* strtest = "fart";

// int quadruple(int n) {
//     return n * 4;
// }

// int x = 5;

// x = x + 69;

// x *= 3;

// x = quadruple(x);

// if (x == 888) {
//     x += 1;
// }

// if (x < 0) {
//     x += 1;
// } else {
//     int y = 32;
// }

// int y = 32;

// while (x > 0) {
//     x -= 1;
//     y += 1;
// }

// x += 69;

// int* z = &x;

// int fart = *z;

// int incX() {
//     x += 1;
// }

// incX();
// incX();
// incX();

// for (int i = 0; i < 10; i += 1) {
//     incX();
// }

// char chartest1 = 'a';
// char chartest2 = 'b';

// int printstr(char * str) {
//     while (*str != '\\0') {
//         putc(*str);

//         str += 1;
//     }
// }

// int printnum(int num) {
//     int placevalue = 1;

//     while (placevalue <= num) {
//         placevalue *= 10;
//     }

//     placevalue /= 10;

//     while (placevalue > 0) {
//         putc(48 + ((num / placevalue) % 10));

//         placevalue /= 10;
//     }
// }

// char * test1 = "aaaabbbbccccdddd\\n";

// printstr("test\\n");
// printstr("another test\\n");
// printstr("yet another test\\n");
// printstr("test custom print fn ");
// printnum(42069);
// printstr("\\n");
// printnum(1000);
// printstr("\\n");
// printnum(999);
// printstr("\\n");
// printnum(1001);
// printstr("\\n");

// printstr(test1);
// printstr(test1 + 1);
// printstr((int *)test1 + 1);
// `;

// const TEST3 = `
// int x;
// for (x = 65; x < 91; x += 1) {
//     putc(x);
// }
// `;

// const output = run(TEST3);

// if (output.type === "success") {
//   console.log(output);
// } else {
//   console.log(output.errors.map((err) => formatDiagnostic(err)).join("\n"));
// }

import { render } from "solid-js/web";
import { createSignal } from "solid-js";
import { CodeEditor } from "./ui/CodeEditor";
import { Page } from "./ui/Page";

render(() => {
  return <Page></Page>;
}, document.getElementById("main")!);
