import {
  ExecutionError,
  FunctionDefNode,
  ParseExpr,
  TypeAnnotationNode,
} from "../ast";

// primitive type definition
type PrimitiveTypeDefinition =
  | {
      size: 1 | 2 | 4 | 8;
      category: "int" | "uint";
      name: string;
    }
  | {
      size: 4 | 8;
      category: "float";
      name: string;
    };

// struct type definition
type StructTypeDefinition = {
  category: "struct";
  fields: [string, Type][];
  name: string;
};

/// any type definition
export type TypeDefinition = PrimitiveTypeDefinition | StructTypeDefinition;

// directory of all defined types
type DefinedTypes = Map<string, TypeDefinition>;

// all-encompassing thing for representing types
export type Type = {
  pointers: number;
  definition: TypeDefinition;
};

// points to an actual instance of a variable in memory
export type AnonymousVariableInstance = {
  type: Type;
  offset: number;
};
export type VariableInstance = AnonymousVariableInstance & {
  name: string;
};

export type StackFrame = {
  base: number;
  bindings: Map<string, VariableInstance>;
  temporaries: AnonymousVariableInstance[];
  functionDefinitions: Map<
    string,
    | {
        type: "internal";
        def: FunctionDefNode;
      }
    | {
        type: "external";
        def: (ctx: ExecutionContext, args: ParseExpr[]) => ExecutionContext;
      }
  >;
  freed: boolean;
  returnType: Type;
  argc: number;
  blocks: {
    bindings: Map<string, VariableInstance>;
  }[];
};

function assert(cond: any, msg: string) {
  if (!cond) throw new Error("ASSERTION FAILED: " + msg);
}

export function notNull<T>(
  v: T | undefined | null,
  name: string,
  ctx: ExecutionContext
): T {
  if (v === undefined || v === null) {
    console.log(ctx);
    throw new ExecutionError(`EXPECTED ${name} TO NOT BE UNDEFINED.`, ctx);
  }
  return v;
}

export function assertPrimitiveTypeDef(
  def: TypeDefinition
): PrimitiveTypeDefinition {
  if (def.category === "struct")
    throw new Error("GOT UNEXPECTED STRUCT TYPEDEF");

  return def;
}

function bigintify(n: number, type: Type): number | bigint {
  if (
    (type.definition.category === "int" ||
      type.definition.category === "uint") &&
    type.definition.size === 8
  )
    return BigInt(n);

  return n;
}

// TODO: make sure that editing one context doesn't affect previous ones
// ideally all of these execution contexts should be able to function independently
// of one another
export class ExecutionContext {
  types: DefinedTypes;
  memory: ArrayBuffer;
  view: DataView;
  esp: number;
  littleEndian: boolean;
  prev?: ExecutionContext;
  stack: StackFrame[];
  stdout: string;

  constructor(
    opts: {
      littleEndian: boolean;
      memory: ArrayBuffer;
      stack: StackFrame[];
      esp: number;
      types: DefinedTypes;
      stdout: string;
    },
    prev?: ExecutionContext
  ) {
    this.stack = opts.stack;
    this.memory = opts.memory;
    this.littleEndian = opts.littleEndian;
    this.view = new DataView(this.memory);
    this.prev = prev;
    this.types = opts.types;
    this.esp = opts.esp;
    this.stdout = opts.stdout;
  }

  getvar(name: string) {
    return (
      this.stacktop().bindings.get(name) ?? this.stack[0].bindings.get(name)
    );
  }

  clone() {
    return new ExecutionContext(
      {
        memory: this.memory,
        littleEndian: this.littleEndian,
        stack: this.stack,
        esp: this.esp,
        types: this.types,
        stdout: this.stdout,
      },
      this
    );
  }

  sizeof(type: Type) {
    if (type.pointers > 0) return 4;

    if (type.definition.category === "struct") {
      return type.definition.fields.reduce(
        (prev, field) => prev + this.sizeof(field[1]),
        0
      );
    }

    return type.definition.size;
  }

  _setStruct(instance: AnonymousVariableInstance, value: ArrayBuffer) {
    const mem = new Uint8Array(this.memory.slice(instance.offset));
    const valAsUint8array = new Uint8Array(value);
    const size = this.sizeof(instance.type);
    for (let i = 0; i < size; i++) {
      mem[i] = valAsUint8array[i];
    }
  }

  _getStruct(instance: AnonymousVariableInstance) {
    return this.memory.slice(
      instance.offset,
      instance.offset + this.sizeof(instance.type)
    );
  }

  setVar(
    instance: AnonymousVariableInstance,
    value: bigint | number | ArrayBuffer
  ) {
    if (instance.type.definition.category === "struct")
      return this._setStruct(instance, value as ArrayBuffer);

    const typecode =
      instance.type.definition.category + instance.type.definition.size;

    const useBigInt = typecode === "int8" || typecode === "uint8";

    const setterFn =
      instance.type.pointers > 0
        ? this.view.setUint32
        : {
            int1: this.view.setInt8,
            int2: this.view.setInt16,
            int4: this.view.setInt32,
            int8: this.view.setBigInt64,
            uint1: this.view.setUint8,
            uint2: this.view.setUint16,
            uint4: this.view.setUint32,
            uint8: this.view.setBigUint64,
            float4: this.view.setFloat32,
            float8: this.view.setFloat64,
          }[typecode];

    assert(setterFn, `setterFn exists (typecode ${typecode})`);

    return setterFn?.apply(this.view, [
      instance.offset,
      // @ts-expect-error no thank you
      useBigInt ? BigInt(value) : Number(value),
      this.littleEndian,
    ]);
  }

  getVar(instance: AnonymousVariableInstance): number | bigint | ArrayBuffer {
    if (instance.type.definition.category === "struct")
      return this._getStruct(instance);

    const getterFn =
      instance.type.pointers > 0
        ? this.view.getUint32
        : {
            int1: this.view.getInt8,
            int2: this.view.getInt16,
            int4: this.view.getInt32,
            int8: this.view.getBigInt64,
            uint1: this.view.getUint8,
            uint2: this.view.getUint16,
            uint4: this.view.getUint32,
            uint8: this.view.getBigUint64,
            float4: this.view.getFloat32,
            float8: this.view.getFloat64,
          }[instance.type.definition.category + instance.type.definition.size];

    assert(getterFn, "getterFn exists");

    return getterFn.apply(this.view, [instance.offset, this.littleEndian]);
  }

  _push(
    type: Type,
    value: number | bigint | ArrayBuffer,
    doNotSetVar?: boolean
  ): AnonymousVariableInstance {
    const esp = this.esp;

    // initialize a zeroed-out struct
    // TODO: figure out how ot deal with the fact that both struct + fields
    // are loaded to teh stack. I think I only have to load the struct
    // because you add/remove it from it all-in-one
    if (type.definition.category === "struct" && type.pointers === 0) {
      for (const [fieldname, fieldvalue] of type.definition.fields) {
        this._push(fieldvalue, bigintify(0, fieldvalue));
      }
      return {
        type,
        offset: esp,
      };
    }

    const varInstance = {
      type,
      offset: esp,
    };

    this.esp += this.sizeof(type);

    if (!doNotSetVar) this.setVar(varInstance, value);

    return varInstance;
  }

  stacktop() {
    return this.stack[this.stack.length - 1];
  }

  blocktop() {
    return this.stacktop().blocks[this.stacktop().blocks.length - 1];
  }

  pushAnonymous(type: Type, value: number | bigint | ArrayBuffer) {
    const instance = this._push(type, value);

    this.stacktop().temporaries.push(instance);
  }

  pushNamed(type: Type, value: number | bigint | ArrayBuffer, name: string) {
    const instance = this._push(type, value);

    const binding = {
      ...instance,
      name,
    };

    this.stacktop().bindings.set(name, binding);

    this.blocktop().bindings.set(name, binding);
  }

  pushBlock() {
    this.stacktop().blocks.push({
      bindings: new Map(),
    });
  }

  popBlock() {
    // remove the block
    const block = notNull(this.stacktop().blocks.pop(), "block", this);

    // get rid of all the block's bindings
    for (const [name, binding] of [...block.bindings].reverse()) {
      this.esp -= this.sizeof(binding.type);

      this.stacktop().bindings.delete(name);
    }
  }

  popTempValue() {
    const val = notNull(
      this.stacktop().temporaries.pop(),
      "tempvalue in stack",
      this
    );

    this.esp -= this.sizeof(val.type);

    return val;
  }

  popTempValueAndGetData() {
    const val = this.popTempValue();
    return this.getVar(val);
  }

  popTempValueAndGetBoth() {
    const val = this.popTempValue();
    return {
      value: this.getVar(val),
      typeinfo: val,
    };
  }

  addFunctionDefinition(name: string, node: FunctionDefNode) {
    this.stacktop().functionDefinitions.set(name, {
      def: node,
      type: "internal",
    });
  }

  getFunctionDefinition(name: string) {
    for (const frame of this.stack.slice().reverse()) {
      const def = frame.functionDefinitions.get(name);
      if (def) {
        return def;
      }
    }
  }

  popStackFrame() {
    const top = this.stack.pop();

    if (!top) throw new ExecutionError("Popping off of an empty stack!", this);

    top.freed = true;
    this.esp = top.base;

    // pop function arguments
    for (let i = 0; i < top.argc; i++) {
      this.popTempValue();
    }

    return top;
  }
}

export const DefaultPrimitives = {
  // integer types
  long: {
    size: 8,
    category: "int",
    name: "long",
  },
  int: {
    size: 4,
    category: "int",
    name: "int",
  },
  short: {
    size: 2,
    category: "int",
    name: "short",
  },
  char: {
    size: 1,
    category: "int",
    name: "char",
  },

  // unsigned integer types
  "unsigned long": {
    size: 8,
    category: "int",
    name: "unsigned long",
  },
  "unsigned int": {
    size: 4,
    category: "int",
    name: "unsigned int",
  },
  "unsigned short": {
    size: 2,
    category: "int",
    name: "unsigned short",
  },
  "unsigned char": {
    size: 1,
    category: "int",
    name: "unsigned char",
  },

  // floats
  float: {
    size: 4,
    category: "float",
    name: "float",
  },
  double: {
    size: 8,
    category: "float",
    name: "double",
  },

  // bool
  bool: {
    size: 1,
    category: "uint",
    name: "bool",
  },
} as const;

export const IntsBySize = {
  1: DefaultPrimitives.char,
  2: DefaultPrimitives.short,
  4: DefaultPrimitives.int,
  8: DefaultPrimitives.long,
};
export const UintsBySize = {
  1: DefaultPrimitives["unsigned char"],
  2: DefaultPrimitives["unsigned short"],
  4: DefaultPrimitives["unsigned int"],
  8: DefaultPrimitives["unsigned long"],
};
export const FloatsBySize = {
  4: DefaultPrimitives.float,
  8: DefaultPrimitives.double,
};

export const DefaultTypes: DefinedTypes = new Map(
  Object.entries(DefaultPrimitives).map(
    ([k, v]) => [k, { ...v, name: k }] as const
  )
);

export function execAndRetrieveData(ctx: ExecutionContext, expr: ParseExpr) {
  ctx = expr.exec(ctx);
  return {
    ctx,
    data: ctx.popTempValueAndGetBoth(),
  };
}

export function constructTypeFromNode(
  ctx: ExecutionContext,
  node: TypeAnnotationNode
): Type {
  return {
    definition: notNull(
      ctx.types.get(node.d.name),
      `Type '${node.d.name}' does not exist.`,
      ctx
    ),
    pointers: node.d.pointers,
  };
}
