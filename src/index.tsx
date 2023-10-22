import { allocateStringLiterals, concatArrayBuffers } from "./allocate";
import { parseStatementList } from "./parser";
import { ParseInput } from "./parser-utils";
import {
  DefaultPrimitives,
  DefaultTypes,
  ExecutionContext,
} from "./runtime/runtime";

/*
The crux of the typechecking problem:
- I don't want to implement a secondary typechecking system.
- But I also can't really integrate it within the current system because 
  doing that wouldn't be exhaustive and would mimic the way a dynamic language
  handles type/error checking more than a static language. I don't want the
  errors to depend on which branch of an if-else chain I end up entering.
- I might want to implement a "type" function for each thing which statically
  checks its type. I think that to some degree this problem is unsolvable. This
  is because there's no equivalent for assembly-based languages: All I can do is
  ensure that my types are correct. The assembly can still be broken.

High priority:
TODO: Implement arrays
TODO: Prevent parsing bug with variables starting with "if", "while", etc.

Med priority:
TODO: Implement print with format codes (actual printf)
TODO: Implement a typechecking layer and define proper behavior for bitwise ops and whatnot

Low priority:
TODO: Test structs and fix the ridiculous amount of inevitable bugs
TODO: struct members
*/

const TEST1 = `

while (x > 3) {
    x += 1;
}

for (int i = 0; i < 100; i += 1) {
    b += i;
    c += 2 * i;
}

struct person {
    char* name;
    int age;
}

int main(int argc, char** argv) {
    int x = 0;
    x = 3;
    x += 1;

    if (x > 3) {
        x *= 1;
    } else if (x < 0) {
        x += cos(69.123);
    } else {
        printf(fart.poopy);
    }

    printf(x);
}

`;

const TEST2 = `

char* strtest = "fart";

int quadruple(int n) {
    return n * 4;
}

int x = 5;

x = x + 69;

x *= 3;

x = quadruple(x);

if (x == 888) {
    x += 1;
}

if (x < 0) {
    x += 1;
} else {
    int y = 32;
}

int y = 32;

while (x > 0) {
    x -= 1;
    y += 1;
}

x += 69;

int* z = &x;

int fart = *z; 

int incX() {
    x += 1;
}

incX();
incX();
incX();

for (int i = 0; i < 10; i += 1) {
    incX();
}

char chartest1 = 'a';
char chartest2 = 'b';

int printstr(char * str) {
    while (*str != '\\0') {
        putc(*str);

        str += 1;
    }
}

int printnum(int num) {
    int placevalue = 1;
    
    while (placevalue <= num) {
        placevalue *= 10;
    }

    placevalue /= 10;

    while (placevalue > 0) {
        putc(48 + ((num / placevalue) % 10));

        placevalue /= 10;
    }
}

char * test1 = "aaaabbbbccccdddd\\n";

printstr("test\\n");
printstr("another test\\n");
printstr("yet another test\\n");
printstr("test custom print fn ");
printnum(42069);
printstr("\\n");
printnum(1000);
printstr("\\n");
printnum(999);
printstr("\\n");
printnum(1001);
printstr("\\n");

printstr(test1);
printstr(test1 + 1);
printstr((int *)test1 + 1);`;

function retrieveNullTerminatedString(mem: ArrayBuffer, i: number) {
  const uint8array = new Uint8Array(mem);

  const dst: number[] = [];

  while (uint8array[i] != 0) {
    dst.push(uint8array[i++]);
  }

  return new Uint8Array(dst).buffer;
}

const parseInput = new ParseInput(TEST2, 0, 0);

const tree = parseStatementList(parseInput);

console.log(tree);
console.log(tree.debug());

const globalMem = {
  mem: new ArrayBuffer(0),
};

allocateStringLiterals(tree, globalMem);

let stdout = "";

const finalState = tree.exec(
  new ExecutionContext({
    littleEndian: true,
    memory: concatArrayBuffers(globalMem.mem, new ArrayBuffer(256)),
    stack: [
      {
        blocks: [
          {
            bindings: new Map(),
          },
        ],
        base: globalMem.mem.byteLength,
        bindings: new Map(),
        temporaries: [],
        functionDefinitions: new Map([
          [
            "printf",
            {
              type: "external",
              def(ctx, args) {
                ctx = ctx.clone();
                ctx = args[0].exec(ctx);
                const value = ctx.popTempValueAndGetData();

                const text = new TextDecoder().decode(
                  retrieveNullTerminatedString(ctx.memory, value as number)
                );

                stdout += text;

                return ctx;
              },
            },
          ],
          [
            "putc",
            {
              type: "external",
              def(ctx, args) {
                ctx = ctx.clone();
                ctx = args[0].exec(ctx);
                const value = ctx.popTempValueAndGetData();
                console.log(value);
                stdout += String.fromCharCode(Number(value));
                return ctx;
              },
            },
          ],
        ]),
        freed: false,
        returnType: {
          definition: DefaultPrimitives.int,
          pointers: 0,
        },
        argc: 0,
      },
    ],
    esp: globalMem.mem.byteLength,
    types: DefaultTypes,
  })
);

console.log(finalState, new Uint8Array(finalState.memory));

console.log("STDOUT\n", stdout);
