(() => {
  // src/lexing.tsx
  var hexDigit = "[0-9a-fA-F]";
  var stringLiteralWithEscapeCodesAnd = (strs) => ["\\\\(n|t|r|0|\\\\)", `\\\\x${hexDigit}{2}`, `\\\\u${hexDigit}{4}`, ...strs].join("|");
  var charLiteralRegex = new RegExp(`'(${stringLiteralWithEscapeCodesAnd(["[^']"])})'`);
  var stringLiteralRegex = new RegExp(`"(${stringLiteralWithEscapeCodesAnd(['[^"]'])})*"`);
  var numberRegex = /[0-9]+(\.[0-9]*)?/;
  var identRegex = /[a-zA-Z_][a-zA-Z0-9_]*/;
  var skipRegex = /[ \r\t\n]+/;
  var opRegex = ["+", "-", "*", "/", "||", "&&", "^^", "==", "!=", ">=", "<=", ">", "<", "&", "|", "^", "<<", ">>", ".", "->", "%"];
  var numberTypeRegex = /[fui]/g;
  var unaryOpRegex = ["!", "*", "&", "~"];

  // src/runtime/runtime.tsx
  function assert(cond, msg) {
    if (!cond)
      throw new Error("ASSERTION FAILED: " + msg);
  }
  function notNull(v, name, ctx) {
    if (v === void 0 || v === null) {
      console.log(ctx);
      throw new ExecutionError(`EXPECTED ${name} TO NOT BE UNDEFINED.`, ctx);
    }
    return v;
  }
  function bigintify(n, type) {
    if ((type.definition.category === "int" || type.definition.category === "uint") && type.definition.size === 8)
      return BigInt(n);
    return n;
  }
  var ExecutionContext = class _ExecutionContext {
    constructor(opts, prev) {
      this.stack = opts.stack;
      this.memory = opts.memory;
      this.littleEndian = opts.littleEndian;
      this.view = new DataView(this.memory);
      this.prev = prev;
      this.types = opts.types;
      this.esp = opts.esp;
    }
    getvar(name) {
      return this.stacktop().bindings.get(name) ?? this.stack[0].bindings.get(name);
    }
    clone() {
      return new _ExecutionContext({
        memory: this.memory,
        littleEndian: this.littleEndian,
        stack: this.stack,
        esp: this.esp,
        types: this.types
      }, this);
    }
    sizeof(type) {
      if (type.pointers > 0)
        return 4;
      if (type.definition.category === "struct") {
        return type.definition.fields.reduce((prev, field) => prev + this.sizeof(field[1]), 0);
      }
      return type.definition.size;
    }
    _setStruct(instance, value) {
      const mem = new Uint8Array(this.memory.slice(instance.offset));
      const valAsUint8array = new Uint8Array(value);
      const size = this.sizeof(instance.type);
      for (let i = 0; i < size; i++) {
        mem[i] = valAsUint8array[i];
      }
    }
    _getStruct(instance) {
      return this.memory.slice(instance.offset, instance.offset + this.sizeof(instance.type));
    }
    setVar(instance, value) {
      if (instance.type.definition.category === "struct")
        return this._setStruct(instance, value);
      const typecode = instance.type.definition.category + instance.type.definition.size;
      const useBigInt = typecode === "int8" || typecode === "uint8";
      const setterFn = instance.type.pointers > 0 ? this.view.setUint32 : {
        int1: this.view.setInt8,
        int2: this.view.setInt16,
        int4: this.view.setInt32,
        int8: this.view.setBigInt64,
        uint1: this.view.setUint8,
        uint2: this.view.setUint16,
        uint4: this.view.setUint32,
        uint8: this.view.setBigUint64,
        float4: this.view.setFloat32,
        float8: this.view.setFloat64
      }[typecode];
      assert(setterFn, `setterFn exists (typecode ${typecode})`);
      return setterFn?.apply(this.view, [
        instance.offset,
        // @ts-expect-error no thank you
        useBigInt ? BigInt(value) : Number(value),
        this.littleEndian
      ]);
    }
    getVar(instance) {
      if (instance.type.definition.category === "struct")
        return this._getStruct(instance);
      const getterFn = instance.type.pointers > 0 ? this.view.getUint32 : {
        int1: this.view.getInt8,
        int2: this.view.getInt16,
        int4: this.view.getInt32,
        int8: this.view.getBigInt64,
        uint1: this.view.getUint8,
        uint2: this.view.getUint16,
        uint4: this.view.getUint32,
        uint8: this.view.getBigUint64,
        float4: this.view.getFloat32,
        float8: this.view.getFloat64
      }[instance.type.definition.category + instance.type.definition.size];
      assert(getterFn, "getterFn exists");
      return getterFn.apply(this.view, [instance.offset, this.littleEndian]);
    }
    _push(type, value, doNotSetVar) {
      const esp = this.esp;
      if (type.definition.category === "struct" && type.pointers === 0) {
        for (const [fieldname, fieldvalue] of type.definition.fields) {
          this._push(fieldvalue, bigintify(0, fieldvalue));
        }
        return {
          type,
          offset: esp
        };
      }
      const varInstance = {
        type,
        offset: esp
      };
      this.esp += this.sizeof(type);
      if (!doNotSetVar)
        this.setVar(varInstance, value);
      return varInstance;
    }
    stacktop() {
      return this.stack[this.stack.length - 1];
    }
    blocktop() {
      return this.stacktop().blocks[this.stacktop().blocks.length - 1];
    }
    pushAnonymous(type, value) {
      const instance = this._push(type, value);
      this.stacktop().temporaries.push(instance);
    }
    pushNamed(type, value, name) {
      const instance = this._push(type, value);
      const binding = {
        ...instance,
        name
      };
      this.stacktop().bindings.set(name, binding);
      this.blocktop().bindings.set(name, binding);
    }
    pushBlock() {
      this.stacktop().blocks.push({
        bindings: /* @__PURE__ */ new Map()
      });
    }
    popBlock() {
      const block = notNull(this.stacktop().blocks.pop(), "block", this);
      for (const [name, binding] of [...block.bindings].reverse()) {
        this.esp -= this.sizeof(binding.type);
        this.stacktop().bindings.delete(name);
      }
    }
    popTempValue() {
      const val = notNull(this.stacktop().temporaries.pop(), "tempvalue in stack", this);
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
        typeinfo: val
      };
    }
    addFunctionDefinition(name, node) {
      this.stacktop().functionDefinitions.set(name, {
        def: node,
        type: "internal"
      });
    }
    getFunctionDefinition(name) {
      for (const frame of this.stack.slice().reverse()) {
        const def = frame.functionDefinitions.get(name);
        if (def) {
          return def;
        }
      }
    }
    popStackFrame() {
      const top = this.stack.pop();
      if (!top)
        throw new ExecutionError("Popping off of an empty stack!", this);
      top.freed = true;
      this.esp = top.base;
      for (let i = 0; i < top.argc; i++) {
        this.popTempValue();
      }
      return top;
    }
  };
  var DefaultPrimitives = {
    // integer types
    long: {
      size: 8,
      category: "int",
      name: "long"
    },
    int: {
      size: 4,
      category: "int",
      name: "int"
    },
    short: {
      size: 2,
      category: "int",
      name: "short"
    },
    char: {
      size: 1,
      category: "int",
      name: "char"
    },
    // unsigned integer types
    "unsigned long": {
      size: 8,
      category: "int",
      name: "unsigned long"
    },
    "unsigned int": {
      size: 4,
      category: "int",
      name: "unsigned int"
    },
    "unsigned short": {
      size: 2,
      category: "int",
      name: "unsigned short"
    },
    "unsigned char": {
      size: 1,
      category: "int",
      name: "unsigned char"
    },
    // floats
    float: {
      size: 4,
      category: "float",
      name: "float"
    },
    double: {
      size: 8,
      category: "float",
      name: "double"
    },
    // bool
    bool: {
      size: 1,
      category: "uint",
      name: "bool"
    }
  };
  var IntsBySize = {
    1: DefaultPrimitives.char,
    2: DefaultPrimitives.short,
    4: DefaultPrimitives.int,
    8: DefaultPrimitives.long
  };
  var UintsBySize = {
    1: DefaultPrimitives["unsigned char"],
    2: DefaultPrimitives["unsigned short"],
    4: DefaultPrimitives["unsigned int"],
    8: DefaultPrimitives["unsigned long"]
  };
  var FloatsBySize = {
    4: DefaultPrimitives.float,
    8: DefaultPrimitives.double
  };
  var DefaultTypes = new Map(Object.entries(DefaultPrimitives).map(([k, v]) => [k, {
    ...v,
    name: k
  }]));
  function execAndRetrieveData(ctx, expr) {
    ctx = expr.exec(ctx);
    return {
      ctx,
      data: ctx.popTempValueAndGetBoth()
    };
  }
  function constructTypeFromNode(ctx, node) {
    return {
      definition: notNull(ctx.types.get(node.d.name), `Type '${node.d.name}' does not exist.`, ctx),
      pointers: node.d.pointers
    };
  }

  // src/typecheck.tsx
  function typeErr(node, ...msgs) {
    return {
      success: false,
      why: msgs.map((msg) => ({
        node,
        msg
      }))
    };
  }
  function typeSuccess(type) {
    return {
      success: true,
      type
    };
  }
  function organizeTypeErrors(typeerrors) {
    const errs = typeerrors.filter((t) => !t.success);
    const successes = typeerrors.filter((t) => t.success).map((t) => t.type);
    if (errs.length === 0)
      return [void 0, successes];
    return [{
      success: false,
      why: errs.map((err) => {
        if (err.success)
          return [];
        return err.why;
      }).flat(1)
    }, successes];
  }
  function isStruct(type) {
    return type.definition.category === "struct" && type.pointers === 0;
  }
  function isPointer(type) {
    return type.pointers > 0;
  }
  function isFloat(type) {
    return type.definition.category === "float" && type.pointers === 0;
  }
  function typeToString(type) {
    let basename = type.definition.name;
    if (type.definition.category === "struct")
      basename = "struct " + basename;
    return basename + " " + "".padStart(type.pointers, "*");
  }
  function combineTypesForArithmetic(node, a, b, op) {
    const aPointer = isPointer(a);
    const bPointer = isPointer(b);
    if (aPointer && bPointer)
      return typeErr(node, "Cannot do arithmetic between a pointer and a pointer.");
    if (a.definition.category === "struct" || b.definition.category === "struct")
      return typeErr(node, "Cannot do arithmetic on structs.");
    if (aPointer || bPointer) {
      if (op !== "+" && op !== "-")
        return typeErr(node, `Cannot do the '${op}' operation with a pointer.`);
    }
    const biggestSize = Math.max(a.definition.size, b.definition.size);
    const switchToFloat = a.definition.category === "float" || b.definition.category === "float";
    const switchToSigned = a.definition.category === "int" || b.definition.category === "int";
    if (switchToFloat) {
      return typeSuccess({
        definition: FloatsBySize[biggestSize],
        pointers: 0
      });
    }
    if (switchToSigned) {
      return typeSuccess({
        definition: IntsBySize[biggestSize],
        pointers: 0
      });
    }
    return typeSuccess(a.definition.size === biggestSize ? a : b);
  }
  function combineTypesForComparisonAndLogical(node, a, b, op) {
    if (op !== "!=" && op !== "==" && isStruct(a) || isStruct(b))
      return typeErr(node, `Cannot use the '${op}' operator on structs.`);
    return typeSuccess({
      definition: DefaultPrimitives.char,
      pointers: 0
    });
  }
  function combineTypesForBitwise(node, a, b, op) {
    if (isFloat(a) || isFloat(b))
      return typeErr(node, `Cannot use the '${op}' operator on a floating-point value.`);
    if (isPointer(a) || isPointer(b))
      return typeErr(node, `Cannot use the '${op}' operator on pointers.`);
    return combineTypesForArithmetic(node, a, b, "+");
  }
  function typecheckBinaryOperation(ctx, op, left, right, node) {
    const mltype = left.type(ctx);
    const mrtype = right.type(ctx);
    const [errs, [ltype, rtype]] = organizeTypeErrors([mltype, mrtype]);
    if (errs)
      return errs;
    switch (op) {
      case "->":
      case ".":
        return typeErr(node, "This operation is currently not supported.");
    }
    switch (op) {
      case "+":
      case "-":
      case "*":
      case "/":
      case "%":
        return combineTypesForArithmetic(node, ltype, rtype, op);
      case "!=":
      case "==":
      case "<=":
      case ">":
      case ">=":
      case "<":
      case "&&":
      case "^^":
      case "||":
        return combineTypesForComparisonAndLogical(node, ltype, rtype, op);
      case "&":
      case "^":
      case "|":
      case "<<":
      case ">>":
        return combineTypesForBitwise(node, ltype, rtype, op);
    }
  }
  function pointerTo(type) {
    return {
      definition: type.definition,
      pointers: type.pointers + 1
    };
  }
  function dereference(type) {
    return {
      definition: type.definition,
      pointers: type.pointers - 1
    };
  }
  function typecheckUnaryOperation(ctx, op, value, node) {
    const mtype = value.type(ctx);
    const [errs, [type]] = organizeTypeErrors([mtype]);
    if (errs)
      return errs;
    if (op === "*") {
      if (!isPointer(type))
        return typeErr(node, `The '*' operator can only be applied to a pointer type, as it represents the dereferencing of a pointer.`);
      return typeSuccess(dereference(type));
    } else if (op === "!") {
      if (isStruct(type))
        return typeErr(node, `The '!' operator cannot be applied to a struct type.`);
      return typeSuccess({
        definition: DefaultPrimitives.char,
        pointers: 0
      });
    } else if (op === "~") {
      if (isFloat(type))
        return typeErr(node, `The '~' operator cannot be applied to a floating point type.`);
      if (isStruct(type))
        return typeErr(node, `The '~' operator cannot be applied to a struct type.`);
      return typeSuccess(type);
    }
    const lv = value.typeLValue(ctx);
    const [errs2, [lvalueType]] = organizeTypeErrors([lv]);
    if (errs2)
      return errs2;
    return typeSuccess(pointerTo(lvalueType));
  }

  // src/parser-utils.tsx
  function matchOnString(matcher, str) {
    if (typeof matcher === "string") {
      return str.startsWith(matcher) ? matcher : void 0;
    } else if (Array.isArray(matcher)) {
      for (const matchStr of matcher) {
        if (str.startsWith(matchStr))
          return matchStr;
      }
      return void 0;
    } else if (matcher instanceof RegExp) {
      const match = matcher.exec(str);
      if (!match)
        return void 0;
      if (match.index === 0)
        return match[0];
    }
  }
  var ParseInput = class _ParseInput {
    position() {
      return this.pos;
    }
    slice() {
      return this.src.slice(this.pos);
    }
    constructor(src, position, bindingPower) {
      this.src = src;
      this.bp = bindingPower;
      this.pos = position;
    }
    isNext(matcher) {
      const skipmatch = matchOnString(skipRegex, this.slice());
      const strmatch = matchOnString(matcher, this.slice().slice(skipmatch?.length ?? 0));
      if (!strmatch)
        return void 0;
      return strmatch;
    }
    expect(matcher, highlight) {
      const skipmatch = matchOnString(skipRegex, this.slice());
      const strmatch = matchOnString(matcher, this.slice().slice(skipmatch?.length ?? 0));
      if (!strmatch)
        return [void 0, this];
      const len = strmatch.length + (skipmatch?.length ?? 0);
      return [strmatch, new _ParseInput(this.src, this.pos + len, this.bindingPower())];
    }
    match(branches, fallback) {
      for (const b of branches) {
        const result = this.expect(b[0], b[1]);
        if (!result[0])
          continue;
        return b[2](result[0], result[1]);
      }
      return fallback(this);
    }
    err(start, msg) {
      return [new ErrorNode(start, this, {
        msg
      }), this];
    }
    bindingPower() {
      return this.bp;
    }
    setBindingPower(bp) {
      return new _ParseInput(this.src, this.pos, bp);
    }
    mut() {
      return new MutableParseInput(this);
    }
  };
  var MutableParseInput = class {
    constructor(src) {
      this.src = src;
    }
    position() {
      return this.src.position();
    }
    isNext(matcher) {
      return this.src.isNext(matcher);
    }
    expect(matcher, highlight) {
      const [result, src] = this.src.expect(matcher, highlight);
      this.src = src;
      return result;
    }
    match(branches, fallback) {
      const [result, src] = this.src.match(branches.map((b) => [b[0], b[1], (str, src2) => {
        this.src = src2;
        return [b[2](str), this.src];
      }]), (src2) => {
        this.src = src2;
        return [fallback(), this.src];
      });
      this.src = src;
      return result;
    }
    err(start, msg) {
      const [result, src] = this.src.err(start, msg);
      this.src = src;
      return result;
    }
    bindingPower() {
      return this.src.bindingPower();
    }
    setBindingPower(bp) {
      this.src = this.src.setBindingPower(bp);
    }
    current() {
      return this.src;
    }
    parse(nodetype, bindingPower) {
      const node = nodetype(this.src.setBindingPower(bindingPower));
      this.src = node.end;
      return node;
    }
  };
  var ParseNode = class {
    constructor(start, end, d) {
      this.d = d;
      this.start = start;
      this.end = end;
    }
    check(ctx) {
      const errors = [...this.checkInner(ctx)].flat();
      return errors.filter((e) => e);
    }
    map(callback) {
      callback(this);
      this.mapInner(callback);
    }
    // lvalues are treated as pointers to whatever they're being assigned to
    execLValue(ctx) {
      throw new ExecutionError(`This expression cannot be used as an lvalue.`, ctx);
    }
    setBindingPower(bp) {
      return new this.constructor(this.start, this.end.setBindingPower(bp), this.d);
    }
    setParserPointer(pp) {
      this.end = pp;
      return this;
    }
    typeLValue(ctx) {
      return typeErr(this, "This cannot be used as an lvalue (thing that can be assigned to).");
    }
    checkLValue(ctx) {
      const result = this.typeLValue(ctx);
      if (result.success)
        return [];
      return result.why;
    }
  };
  function requiresSemicolon(stmt) {
    return !(stmt instanceof IfNode || stmt instanceof FunctionDefNode || stmt instanceof StructDefinitionNode || stmt instanceof LoopNode);
  }
  var BindingPowers = {
    // logical
    "||": 40,
    "^^": 50,
    "&&": 60,
    // bitwise
    "|": 70,
    "^": 80,
    "&": 90,
    // comparison
    "==": 100,
    "!=": 100,
    ">=": 110,
    "<=": 110,
    ">": 110,
    "<": 110,
    // bitshift
    ">>": 120,
    "<<": 120,
    // arithmetic
    "+": 130,
    "-": 130,
    "*": 140,
    "/": 140,
    "%": 140,
    // member access
    ".": 160,
    "->": 160
  };
  var UnaryBindingPowers = {
    // unaries
    "*": 150,
    "&": 150,
    "~": 150,
    "!": 150
  };

  // src/nodes/BinaryOpNode.tsx
  function handleBinaryOperation(ctx, op) {
    const right = ctx.popTempValueAndGetBoth();
    const left = ctx.popTempValueAndGetBoth();
    let lv = left.value;
    let rv = right.value;
    switch (op) {
      case "->":
      case ".":
        return ctx;
    }
    if (lv instanceof ArrayBuffer || rv instanceof ArrayBuffer) {
      throw new ExecutionError("Cannot do this operation between structs", ctx);
    }
    if (left.typeinfo.type.definition.category === "float" || right.typeinfo.type.definition.category === "float") {
      lv = Number(lv);
      rv = Number(rv);
    } else {
      lv = BigInt(lv);
      rv = BigInt(rv);
    }
    let output;
    let outputType = left.typeinfo.type;
    if ((op === "+" || op === "-") && (left.typeinfo.type.pointers > 0 || right.typeinfo.type.pointers > 0)) {
      rv = Number(rv);
      lv = Number(lv);
      const leftIsPtr = left.typeinfo.type.pointers > 0;
      if (op === "-") {
        rv *= -1;
      }
      const ptrVal = leftIsPtr ? lv : rv;
      const nonPtrVal = leftIsPtr ? rv : lv;
      const ptrType = leftIsPtr ? left.typeinfo : right.typeinfo;
      const nonPtrType = leftIsPtr ? right.typeinfo : left.typeinfo;
      output = ptrVal + nonPtrVal * ctx.sizeof({
        definition: ptrType.type.definition,
        pointers: ptrType.type.pointers - 1
      });
      outputType = ptrType.type;
      ctx.pushAnonymous(outputType, output);
      return ctx;
    }
    switch (op) {
      case "+":
      case "-":
      case "*":
      case "/":
      case "%":
        output = {
          "+": (a, b) => a + b,
          "-": (a, b) => a - b,
          "*": (a, b) => a * b,
          "/": (a, b) => a / b,
          "%": (a, b) => a % b
        }[op](lv, rv);
        break;
      case "!=":
      case "==":
      case "<=":
      case ">":
      case ">=":
      case "<":
      case "&&":
      case "^^":
      case "||":
        output = Number({
          "==": (a, b) => a == b,
          "!=": (a, b) => a != b,
          ">=": (a, b) => a >= b,
          ">": (a, b) => a > b,
          "<=": (a, b) => a <= b,
          "<": (a, b) => a < b
        }[op](lv, rv));
        break;
      case "&":
      case "^":
      case "|":
      case "<<":
      case ">>":
        output = {
          "&": (a, b) => a & b,
          "^": (a, b) => a ^ b,
          "|": (a, b) => a | b,
          ">>": (a, b) => a >> b,
          "<<": (a, b) => a << b
        }[op](lv, rv);
        break;
    }
    if (output !== void 0) {
      ctx.pushAnonymous(outputType, output);
    }
    return ctx;
  }
  var BinaryOpNode = class extends ParseNode {
    debug() {
      return `(${this.d.op} ${this.d.left.debug()} ${this.d.right.debug()})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx = this.d.left.exec(ctx);
      ctx = this.d.right.exec(ctx);
      ctx = handleBinaryOperation(ctx, this.d.op);
      return ctx;
    }
    // TODO: Implement lvalues here later
    mapInner(cb) {
      automap(this.d.left, cb);
      automap(this.d.right, cb);
    }
    type(ctx) {
      return typecheckBinaryOperation(ctx, this.d.op, this.d.left, this.d.right, this);
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
  };

  // src/nodes/AssignmentNode.tsx
  var AssignmentNode = class extends ParseNode {
    debug() {
      return `(${this.d.op ?? ""}= ${this.d.left.debug()} ${this.d.right.debug()})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx = this.d.left.execLValue(ctx);
      if (this.d.op) {
        ctx = this.d.left.exec(ctx);
      }
      ctx = this.d.right.exec(ctx);
      if (this.d.op) {
        handleBinaryOperation(ctx, this.d.op);
      }
      const right = ctx.popTempValueAndGetBoth();
      const left = ctx.popTempValueAndGetBoth();
      ctx.setVar({
        type: right.typeinfo.type,
        offset: left.value
      }, right.value);
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.left, cb);
      automap(this.d.right, cb);
    }
    *checkInner(ctx) {
      const mltype = this.d.left.typeLValue(ctx);
      const mrtype = this.d.right.type(ctx);
      const [errs, [ltype, rtype]] = organizeTypeErrors([mltype, mrtype]);
      yield errs?.why;
    }
  };

  // src/nodes/ErrorNode.tsx
  var ErrorNode = class extends ParseNode {
    debug() {
      return `(#ERROR# '${this.d.msg}')`;
    }
    exec(ctx) {
      throw new ExecutionError(`Parse Error: ${this.d.msg}`, ctx);
      return ctx;
    }
    mapInner(cb) {
    }
    type(ctx) {
      return {
        success: false,
        why: [{
          node: this,
          msg: this.d.msg
        }]
      };
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
  };

  // src/nodes/FunctionCallNode.tsx
  var FunctionCallNode = class extends ParseNode {
    debug() {
      return `(${this.d.name} ${this.d.args.map((arg) => arg.debug()).join(" ")})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      for (const arg of this.d.args) {
        ctx = arg.exec(ctx);
      }
      const fndef = ctx.getFunctionDefinition(this.d.name);
      if (!fndef) {
        console.log(ctx);
        throw new ExecutionError(`Function '${fndef}' does not exist.`, ctx);
      }
      if (fndef.type === "internal") {
        ctx = fndef.def.call(ctx);
      } else {
        ctx = fndef.def(ctx, this.d.args);
      }
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.args, cb);
    }
    type(ctx) {
      const functionTypeSig = ctx.getFunctionTypes(this.d.name, this);
      if (!functionTypeSig.success)
        return functionTypeSig;
      if (functionTypeSig.args.length !== this.d.args.length)
        return {
          success: false,
          why: [{
            node: this,
            msg: `The function '${this.d.name}' takes ${functionTypeSig.args.length} arguments, but you supplied ${this.d.args.length} arguments.`
          }]
        };
      const maybeArgTypes = this.d.args.map((arg) => arg.type(ctx));
      const [errs, argTypes] = organizeTypeErrors(maybeArgTypes);
      if (errs)
        return errs;
      const badargs = [];
      for (let i = 0; i < functionTypeSig.args.length; i++) {
        if (!isStruct(argTypes[i]) && !isStruct(functionTypeSig[i]))
          continue;
        if (argTypes[i].definition !== functionTypeSig[i].definition)
          badargs.push(`Argument ${i + 1} should be of type '${typeToString(argTypes[i])}', but you put '${typeToString(functionTypeSig[i])}'.`);
      }
      return {
        type: functionTypeSig.returns,
        success: true
      };
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
  };

  // src/nodes/FunctionDefNode.tsx
  var FunctionDefNode = class extends ParseNode {
    debug() {
      return `(fndef ${this.d.args.map((arg) => `(${arg.debug()})`).join(" ")} ${this.d.body.map((v) => v.debug()).join(" ")})`;
    }
    exec(ctx) {
      ctx.addFunctionDefinition(this.d.returnTypeAndName.d.name, this);
      return ctx;
    }
    call(ctx) {
      ctx = ctx.clone();
      const ret = ctx.types.get(this.d.returnTypeAndName.d.type.d.name);
      if (!ret)
        throw new ExecutionError(`Return type '${this.d.returnTypeAndName.d.type.d.name}' does not exist.`, ctx);
      const frame = {
        base: ctx.esp,
        bindings: /* @__PURE__ */ new Map(),
        temporaries: [],
        functionDefinitions: /* @__PURE__ */ new Map(),
        freed: false,
        returnType: {
          definition: ret,
          pointers: this.d.returnTypeAndName.d.type.d.pointers
        },
        argc: this.d.args.length,
        blocks: [{
          bindings: /* @__PURE__ */ new Map()
        }]
      };
      let offset = 0;
      for (const arg of this.d.args.slice().reverse()) {
        const type = ctx.types.get(arg.d.type.d.name);
        if (!type)
          throw new ExecutionError(`Error with argument '${arg.d.name}': Type '${arg.d.type.d.name}' does not exist.`, ctx);
        const fnargType = {
          definition: type,
          pointers: arg.d.type.d.pointers
        };
        offset += ctx.sizeof(fnargType);
        frame.bindings.set(arg.d.name, {
          offset: ctx.esp - offset,
          type: fnargType,
          name: arg.d.name
        });
      }
      ctx.stack.push(frame);
      ctx = handleStatementList(ctx, this.d.body).ctx;
      if (!frame.freed) {
        ctx = ctx.clone();
        ctx.popStackFrame();
      }
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.args, cb);
      automap(this.d.returnTypeAndName, cb);
      automap(this.d.body, cb);
    }
    *checkInner(ctx) {
      const checks = [];
      ctx.withStackFrame(() => {
        for (const stmt of this.d.args)
          checks.push(...stmt.check(ctx));
      });
      yield checks;
    }
  };

  // src/nodes/IdentifierNode.tsx
  var IdentifierNode = class extends ParseNode {
    debug() {
      return this.d.name;
    }
    exec(ctx) {
      ctx = ctx.clone();
      const data = ctx.getvar(this.d.name);
      if (!data) {
        throw new ExecutionError(`Identifier '${this.d.name}' does not exist.`, ctx);
      }
      ctx.pushAnonymous(data.type, ctx.getVar(data));
      return ctx;
    }
    execLValue(ctx) {
      ctx = ctx.clone();
      const data = ctx.getvar(this.d.name);
      if (!data) {
        throw new ExecutionError(`Identifier '${this.d.name}' does not exist.`, ctx);
      }
      ctx.pushAnonymous({
        definition: data.type.definition,
        pointers: data.type.pointers + 1
      }, data.offset);
      return ctx;
    }
    mapInner() {
    }
    type(ctx) {
      return ctx.getVariableType(this);
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
  };

  // src/nodes/IfNode.tsx
  var IfNode = class extends ParseNode {
    debug() {
      return `(if ${this.d.condition.debug()} (${autodebug(this.d.body)}) ${this.d.elseif ? `else ${autodebug(this.d.elseif)}` : ""})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx = this.d.condition.exec(ctx);
      const top = ctx.popTempValueAndGetData();
      if (top == 0) {
        ctx = this.d.elseif?.exec(ctx) ?? ctx;
      } else {
        ctx = handleStatementList(ctx, this.d.body).ctx;
      }
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.condition, cb);
      automap(this.d.body, cb);
      automap(this.d.elseif, cb);
    }
    *checkInner(ctx) {
      yield this.d.condition.check(ctx);
      const checks = [];
      ctx.withBlock(() => {
        for (const stmt of this.d.body)
          checks.push(...stmt.check(ctx));
      });
      yield checks;
      yield this.d.elseif?.check(ctx);
    }
  };

  // src/nodes/LoopNode.tsx
  var LoopNode = class extends ParseNode {
    debug() {
      if (this.d.conditions.type === "for") {
        return `(for (${autodebug(this.d.conditions.start)} ${autodebug(this.d.conditions.condition)} ${autodebug(this.d.conditions.iter)}) ${autodebug(this.d.body)})`;
      } else {
        return `(while ${this.d.conditions.condition.debug()} ${autodebug(this.d.body)})`;
      }
    }
    // TODO: deal with stack misalignment from statements that contain expressions but don't do anything with them
    exec(ctx) {
      ctx = ctx.clone();
      const stacktop = ctx.stacktop();
      const end = () => {
        ctx.popBlock();
        return ctx;
      };
      if (this.d.conditions.type === "while") {
        ctx.pushBlock();
        while (true) {
          ctx = this.d.conditions.condition.exec(ctx);
          const cond = ctx.popTempValueAndGetBoth();
          if (cond.value == 0) {
            break;
          }
          const list = handleStatementList(ctx, this.d.body);
          ctx = list.ctx;
          if (list.returned)
            return ctx;
        }
      } else {
        ctx = this.d.conditions.start?.exec(ctx) ?? ctx;
        if (stacktop.freed)
          return ctx;
        while (true) {
          ctx = this.d.conditions.condition?.exec(ctx) ?? ctx;
          if (stacktop.freed)
            return ctx;
          const cond = this.d.conditions.condition ? ctx.popTempValueAndGetBoth() : void 0;
          if (cond && cond.value == 0)
            break;
          const list = handleStatementList(ctx, this.d.body);
          ctx = list.ctx;
          if (list.returned)
            return ctx;
          ctx = this.d.conditions.iter?.exec(ctx) ?? ctx;
          if (stacktop.freed)
            return ctx;
        }
      }
      return end();
    }
    mapInner(cb) {
      automap(this.d.body, cb);
      if (this.d.conditions.type === "while") {
        automap(this.d.conditions.condition, cb);
      } else {
        automap(this.d.conditions.condition, cb);
        automap(this.d.conditions.iter, cb);
        automap(this.d.conditions.start, cb);
      }
    }
    // TODO: impl this
    *checkInner(ctx) {
    }
  };

  // src/nodes/NumberNode.tsx
  var NumberNode = class extends ParseNode {
    debug() {
      return this.d.num.toString();
    }
    determineNumericType() {
      const bytes = this.d.bytes ?? (this.d.type === "f" ? 8 : 4);
      switch (this.d.type) {
        case "f":
          return {
            definition: FloatsBySize[bytes],
            pointers: 0
          };
        case "u":
          return {
            definition: UintsBySize[bytes],
            pointers: 0
          };
        case "i":
        default:
          return {
            definition: IntsBySize[bytes],
            pointers: 0
          };
      }
    }
    exec(ctx) {
      const ctx2 = ctx.clone();
      ctx2.pushAnonymous(this.determineNumericType(), this.d.num);
      return ctx2;
    }
    mapInner() {
    }
    type(ctx) {
      return {
        success: true,
        type: this.determineNumericType()
      };
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
  };

  // src/nodes/ReturnStatementNode.tsx
  var ReturnStatementNode = class extends ParseNode {
    debug() {
      return `(return ${autodebug(this.d.expr)})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx = this.d.expr?.exec(ctx) ?? ctx;
      let outputValue = 0;
      if (this.d.expr) {
        const output = ctx.popTempValueAndGetBoth();
        outputValue = output.value;
      }
      const frame = ctx.popStackFrame();
      ctx.pushAnonymous(frame.returnType, outputValue);
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.expr, cb);
    }
    // TODO: statically ensure that return type matches enclosing function type
    *checkInner(ctx) {
      if (this.d.expr)
        yield this.d.expr.check(ctx);
    }
  };

  // src/nodes/StatementListNode.tsx
  var StatementListNode = class extends ParseNode {
    debug() {
      return this.d.body.map((s) => s.debug()).join("\n");
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx = handleStatementList(ctx, this.d.body).ctx;
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.body, cb);
    }
    *checkInner(ctx) {
      for (const stmt of this.d.body)
        yield stmt.check(ctx);
    }
  };

  // src/nodes/StringLiteralNode.tsx
  var StringLiteralNode = class extends ParseNode {
    debug() {
      return `${JSON.stringify(this.d.str)}`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx.pushAnonymous(this._type(), this.d.pointer);
      return ctx;
    }
    mapInner(callback) {
    }
    _type() {
      return {
        definition: DefaultPrimitives.char,
        pointers: 1
      };
    }
    type(ctx) {
      return typeSuccess(this._type());
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
  };

  // src/nodes/StructDefinitionNode.tsx
  var StructDefinitionNode = class extends ParseNode {
    debug() {
      return `(struct ${this.d.name} (${autodebug(this.d.fields)}))`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx.types.set(this.d.name, {
        category: "struct",
        name: this.d.name,
        fields: this.d.fields.map((f) => {
          const fieldtype = ctx.types.get(f.d.type.d.name);
          if (!fieldtype)
            throw new ExecutionError(`The struct field '${f.d.name}' has type '${f.d.type}', which does not exist.`, ctx);
          return [f.d.name, {
            definition: fieldtype,
            pointers: f.d.type.d.pointers
          }];
        })
      });
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.fields, cb);
    }
    *checkInner(ctx) {
    }
  };

  // src/nodes/TypecastNode.tsx
  var TypecastNode = class extends ParseNode {
    debug() {
      return `(cast ${this.d.value.debug()} to ${this.d.type.debug()})`;
    }
    exec(ctx) {
      const data = execAndRetrieveData(ctx, this.d.value);
      ctx = data.ctx;
      const type = constructTypeFromNode(ctx, this.d.type);
      ctx.pushAnonymous(type, data.data.value);
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.type, cb);
      automap(this.d.value, cb);
    }
    type(ctx) {
      const type = ctx.getTypeFromName(this.d.type);
      return type;
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
  };

  // src/nodes/UnaryOpNode.tsx
  var UnaryOpNode = class extends ParseNode {
    debug() {
      return `(${this.d.op} ${this.d.value.debug()})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx = this.d.op === "&" ? this.d.value.execLValue(ctx) : this.d.value.exec(ctx);
      const value = ctx.popTempValueAndGetBoth();
      let output;
      let outputType;
      switch (this.d.op) {
        case "!":
          output = value.value == 0 ? 1 : 0;
          outputType = DefaultPrimitives.bool;
          break;
        case "&":
          output = value.value;
          outputType = {
            ...value.typeinfo.type,
            pointers: value.typeinfo.type.pointers + 1
          };
          break;
        case "*":
          outputType = {
            ...value.typeinfo.type,
            pointers: value.typeinfo.type.pointers - 1
          };
          output = ctx.getVar({
            offset: Number(value.value),
            type: outputType
          });
          break;
        case "~":
          output = ~value.value;
          outputType = value.typeinfo.type;
          break;
      }
      ctx.pushAnonymous(outputType, output);
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.value, cb);
    }
    type(ctx) {
      return typecheckUnaryOperation(ctx, this.d.op, this.d.value, this);
    }
    *checkInner(ctx) {
      return defaultExprCheck(this, ctx);
    }
    typeLValue(ctx) {
      if (this.d.op !== "*") {
        return typeErr(this, `This expression cannot be an lvalue.`);
      }
      return typecheckUnaryOperation(ctx, this.d.op, this.d.value, this);
    }
  };

  // src/nodes/VariableDefinitionNode.tsx
  var VariableDefinitionNode = class extends ParseNode {
    debug() {
      return `(let ${this.d.definition.debug()} ${autodebug(this.d.value)})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      const typeOfThisVar = ctx.types.get(this.d.definition.d.type.d.name);
      if (!typeOfThisVar)
        throw new ExecutionError(`The type '${this.d.definition.d.type.d.name}' does not exist.`, ctx);
      let value = 0;
      if (this.d.value) {
        ctx = this.d.value.exec(ctx);
        const thingToAssign = ctx.popTempValueAndGetBoth();
        value = thingToAssign.value;
      }
      ctx.pushNamed({
        definition: typeOfThisVar,
        pointers: this.d.definition.d.type.d.pointers
      }, value, this.d.definition.d.name);
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.definition, cb);
      automap(this.d.value, cb);
    }
    *checkInner(ctx) {
      yield this.d.definition.check(ctx);
      yield this.d.value?.check(ctx);
      const [fail, [type]] = organizeTypeErrors([ctx.getTypeFromName(this.d.definition.d.type)]);
      yield fail?.why;
      ctx.defineVariable(this.d.definition.d.name, type);
    }
  };

  // src/nodes/DefinitionNode.tsx
  var DefinitionNode = class extends ParseNode {
    debug() {
      return `(${this.d.type.debug()} ${this.d.name})`;
    }
    exec(ctx) {
      throw new Error("Type definitions should not be exec()ed.");
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.type, cb);
    }
    *checkInner(ctx) {
      yield this.d.type.check(ctx);
    }
  };

  // src/nodes/ElseNode.tsx
  var ElseNode = class extends ParseNode {
    debug() {
      return `(${autodebug(this.d.body)})`;
    }
    exec(ctx) {
      ctx = ctx.clone();
      ctx = handleStatementList(ctx, this.d.body).ctx;
      return ctx;
    }
    mapInner(cb) {
      automap(this.d.body, cb);
    }
    *checkInner(ctx) {
      const checks = [];
      ctx.withBlock(() => {
        for (const stmt of this.d.body)
          checks.push(...stmt.check(ctx));
      });
      yield checks;
    }
  };

  // src/nodes/TypeAnnotationNode.tsx
  var TypeAnnotationNode = class extends ParseNode {
    debug() {
      return `${this.d.struct ? "struct " : ""}${this.d.name} ${"".padStart(this.d.pointers, "*")}`;
    }
    exec(ctx) {
      throw new Error("Type annotations should not be exec()ed.");
      return ctx;
    }
    mapInner() {
    }
    *checkInner(ctx) {
    }
  };

  // src/ast.tsx
  function autodebug(node) {
    if (Array.isArray(node)) {
      return node.map((n) => n.debug()).join(" ");
    } else if (node) {
      return node.debug();
    }
    return "";
  }
  function handleStatementList(ctx, body) {
    const top = ctx.stacktop();
    ctx.pushBlock();
    for (const stmt of body) {
      ctx = stmt.exec(ctx);
      if (top.freed) {
        return {
          returned: true,
          ctx
        };
      }
    }
    ctx.popBlock();
    return {
      returned: false,
      ctx
    };
  }
  function* defaultExprCheck(expr, ctx) {
    const type = expr.type(ctx);
    if (type.success)
      return [];
    return {
      ...type.why
    };
  }
  var ExecutionError = class extends Error {
    constructor(msg, ctx) {
      super(msg);
      this.ctx = ctx;
    }
  };
  function automap(node, callback) {
    if (Array.isArray(node)) {
      node.map((n) => n.map(callback));
    } else if (node instanceof ParseNode) {
      node.map(callback);
    }
  }

  // src/allocate.tsx
  function concatArrayBuffers(a, b) {
    const concatted = new ArrayBuffer(a.byteLength + b.byteLength);
    const byteArray = new Uint8Array(concatted);
    const byteArrayA = new Uint8Array(a);
    const byteArrayB = new Uint8Array(b);
    for (let i = 0; i < byteArrayA.length; i++) {
      byteArray[i] = byteArrayA[i];
    }
    for (let i = 0; i < byteArrayB.length; i++) {
      byteArray[i + byteArrayA.length] = byteArrayB[i];
    }
    return concatted;
  }
  function allocateStringLiterals(ast, ctx) {
    function recurse(n) {
      if (n instanceof StringLiteralNode) {
        const strbuf = new TextEncoder().encode(n.d.str + "\0");
        n.d.pointer = ctx.mem.byteLength;
        ctx.mem = concatArrayBuffers(ctx.mem, strbuf);
      }
    }
    ast.map(recurse);
  }

  // src/parser.tsx
  function getBindingPowerOfNextToken(s) {
    const str = s.isNext(opRegex);
    if (!str)
      return 0;
    return BindingPowers[str] ?? 0;
  }
  function enclose(start, muts, end, endErr, highlightEnds, callback) {
    const items = [];
    while (!muts.isNext(end)) {
      const item = callback(muts);
      if (item instanceof ErrorNode)
        return item;
      items.push(item);
    }
    if (!muts.expect(end, highlightEnds))
      return muts.err(start, endErr);
    return items;
  }
  function parseTypeAnnotation(s) {
    const muts = s.mut();
    const struct = !!muts.expect("struct", "keyword");
    const unsigned = struct ? void 0 : muts.expect("unsigned", "type");
    let name = muts.expect(identRegex, "type");
    if (!name)
      return muts.err(s, "Expected a type name.");
    if (unsigned)
      name = `unsigned ${name}`;
    let pointers = 0;
    while (muts.isNext("*")) {
      muts.expect("*", "operator");
      pointers++;
    }
    return new TypeAnnotationNode(s, muts.current(), {
      struct,
      name,
      pointers
    });
  }
  function parseDefinition(s) {
    const muts = s.mut();
    const type = muts.parse(parseTypeAnnotation, 0);
    if (type instanceof ErrorNode)
      return type;
    const name = muts.expect(identRegex, "type");
    if (!name)
      return muts.err(s, "Expected an identifier.");
    return new DefinitionNode(s, muts.current(), {
      type,
      name
    });
  }
  function parseStatementList(s) {
    const muts = s.mut();
    const body = [];
    while (true) {
      const stmt = muts.parse(parseStatement, 0);
      if (stmt instanceof ErrorNode)
        break;
      body.push(stmt);
      if (requiresSemicolon(stmt) && !muts.expect(";", "semicolon"))
        return muts.err(s, "Expected ';'");
    }
    return new StatementListNode(s, muts.current(), {
      body
    });
  }
  function getCondition(start, muts) {
    if (!muts.expect("(", "bracket"))
      return muts.err(start, "Expected '('");
    const condition = muts.parse(parseExpr, 0);
    if (!muts.expect(")", "bracket"))
      return muts.err(start, "Expected ')'");
    return condition;
  }
  function parseLoop(s) {
    const muts = s.mut();
    return muts.match([["for", "keyword", () => {
      if (!muts.expect("(", "bracket"))
        return muts.err(s, "Expected '('");
      let start, iter;
      let condition;
      if (!muts.isNext(";")) {
        start = muts.parse(parseStatement, 0);
      }
      if (!muts.expect(";", "semicolon"))
        return muts.err(s, "Expected ';'");
      if (!muts.isNext(";")) {
        condition = muts.parse(parseExpr, 0);
      }
      if (!muts.expect(";", "semicolon"))
        return muts.err(s, "Expected ';'");
      if (!muts.isNext(")")) {
        iter = muts.parse(parseStatement, 0);
      }
      if (!muts.expect(")", "bracket"))
        return muts.err(s, "Expected ')'");
      const body = parseCurlyBracesDelimitedBody(s, muts);
      if (body instanceof ErrorNode)
        return body;
      return new LoopNode(s, muts.current(), {
        body,
        conditions: {
          type: "for",
          start,
          condition,
          iter
        }
      });
    }], ["while", "keyword", () => {
      const condition = getCondition(s, muts);
      if (condition instanceof ErrorNode)
        return condition;
      const body = parseCurlyBracesDelimitedBody(s, muts);
      if (body instanceof ErrorNode)
        return body;
      return new LoopNode(s, muts.current(), {
        conditions: {
          type: "while",
          condition
        },
        body
      });
    }]], () => muts.err(s, "Expected 'for' or 'while'."));
  }
  function parseIfElseStatement(s) {
    let muts = s.mut();
    return muts.match([["if", "keyword", () => {
      const condition = getCondition(s, muts);
      if (condition instanceof ErrorNode)
        return condition;
      const body = parseCurlyBracesDelimitedBody(s, muts);
      if (body instanceof ErrorNode)
        return body;
      let elseif;
      if (muts.isNext("else")) {
        elseif = muts.parse(parseIfElseStatement, 0);
      }
      return new IfNode(s, muts.current(), {
        condition,
        body,
        elseif
      });
    }], ["else", "keyword", () => {
      if (muts.isNext("if"))
        return muts.parse(parseIfElseStatement, 0);
      const body = parseCurlyBracesDelimitedBody(s, muts);
      if (body instanceof ErrorNode)
        return body;
      return new ElseNode(s, muts.current(), {
        body
      });
    }]], () => muts.err(s, "Expected 'if'."));
  }
  function parseCurlyBracesDelimitedBody(start, muts) {
    if (!muts.expect("{", "bracket"))
      return muts.err(start, "Expected '{'");
    return enclose(start, muts, "}", "Expected '}'", "bracket", (muts2) => {
      const stmt = muts2.parse(parseStatement, 0);
      if (stmt instanceof ErrorNode)
        return stmt;
      if (requiresSemicolon(stmt) && !muts2.expect(";", "semicolon"))
        return muts2.err(start, "Expected ';'");
      return stmt;
    });
  }
  function parseStatement(s) {
    let muts = s.mut();
    if (muts.expect("return", "keyword")) {
      let expr = muts.parse(parseExpr, 0);
      if (expr instanceof ErrorNode && muts.isNext(";"))
        expr = void 0;
      if (expr instanceof ErrorNode)
        return expr;
      return new ReturnStatementNode(s, muts.current(), {
        expr
      });
    }
    if (muts.expect("struct", "keyword")) {
      const name = muts.expect(identRegex, "identifier");
      if (!name)
        return muts.err(s, "Expected an identifier.");
      if (!muts.expect("{", "bracket"))
        return muts.err(s, "Expected '{'");
      const fields = enclose(s, muts, "}", "Expected '}'", "bracket", (muts2) => {
        const def2 = muts2.parse(parseDefinition, 0);
        if (!muts2.expect(";", "semicolon"))
          return muts2.err(s, "Expected ';'");
        return def2;
      });
      if (fields instanceof ErrorNode)
        return fields;
      return new StructDefinitionNode(s, muts.current(), {
        fields,
        name
      });
    }
    muts = s.mut();
    const def = muts.parse(parseDefinition, 0);
    if (def instanceof ErrorNode) {
      muts = s.mut();
      if (muts.isNext("if")) {
        const ifStmt = muts.parse(parseIfElseStatement, 0);
        return ifStmt;
      }
      if (muts.isNext("for") || muts.isNext("while")) {
        const loop = muts.parse(parseLoop, 0);
        return loop;
      }
      const assignment = (() => {
        muts = s.mut();
        const left = muts.parse(parseExpr, 200);
        if (left instanceof ErrorNode)
          return;
        const op = muts.expect(opRegex, "operator");
        if (!muts.expect("=", "operator"))
          return;
        const right = muts.parse(parseExpr, 0);
        if (right instanceof ErrorNode)
          return;
        return new AssignmentNode(s, muts.current(), {
          left,
          op,
          right
        });
      })();
      if (!assignment)
        return parseExpr(s);
      return assignment;
    }
    return muts.match([["(", "bracket", () => {
      const args = enclose(s, muts, ")", "Expected ')'", "bracket", (muts2) => {
        const def2 = muts2.parse(parseDefinition, 0);
        if (!muts2.expect(",", "operator") && !muts2.isNext(")"))
          return muts2.err(s, "Expected ','");
        return def2;
      });
      if (args instanceof ErrorNode)
        return args;
      const body = parseCurlyBracesDelimitedBody(s, muts);
      if (body instanceof ErrorNode)
        return body;
      return new FunctionDefNode(s, muts.current(), {
        returnTypeAndName: def,
        args,
        body
      });
    }], ["=", "operator", (name) => {
      const value = muts.parse(parseExpr, 0);
      if (value instanceof ErrorNode)
        return value;
      return new VariableDefinitionNode(s, muts.current(), {
        value,
        definition: def
      });
    }], [";", "semicolon", () => {
      return new VariableDefinitionNode(s, muts.current(), {
        definition: def
      });
    }]], () => {
      return parseExpr(s);
    });
    return parseExpr(s);
  }
  function parseExpr(s) {
    let left = parseInitExpr(s);
    if (left instanceof ErrorNode)
      return left;
    let currentSrc = left.end;
    while (true) {
      const nextBindingPower = getBindingPowerOfNextToken(currentSrc);
      if (nextBindingPower <= s.bindingPower())
        break;
      const consequent = parseConsequentExpr(left.setBindingPower(nextBindingPower));
      if (consequent instanceof ErrorNode)
        break;
      left = consequent;
    }
    return left;
  }
  function decodeString(str) {
    let decodedStr = "";
    for (let i = 0; i < str.length; i++) {
      if (str[i] === "\\") {
        switch (str[++i]) {
          case "t":
          case "r":
          case "n":
          case "0":
          case "\\":
            decodedStr += {
              t: "	",
              r: "\r",
              n: "\n",
              "0": "\0",
              "\\": "\\"
            }[str[i]];
            break;
          case "x": {
            const num = parseInt(str[++i] + str[++i], 16);
            decodedStr += String.fromCharCode(num);
            break;
          }
          case "u": {
            const num = parseInt(str[++i] + str[++i] + str[++i] + str[++i], 16);
            decodedStr += String.fromCharCode(num);
            break;
          }
        }
      } else {
        decodedStr += str[i];
      }
    }
    return decodedStr;
  }
  function parseInitExpr(s) {
    let muts = s.mut();
    return muts.match([
      // char literal
      [charLiteralRegex, "string", (str) => {
        const char = decodeString(str.slice(1, -1));
        return new NumberNode(s, muts.current(), {
          num: Number(char.charCodeAt(0)),
          type: "i",
          bytes: 1
        });
      }],
      // string literal
      [stringLiteralRegex, "string", (inputStr) => {
        const str = decodeString(inputStr.slice(1, -1));
        return new StringLiteralNode(s, muts.current(), {
          str,
          pointer: 0
        });
      }],
      // unary op
      [unaryOpRegex, "operator", (op) => {
        const value = muts.parse(parseExpr, UnaryBindingPowers[op]);
        return new UnaryOpNode(s, muts.current(), {
          value,
          op
        });
      }],
      // parenthesized
      ["(", "bracket", () => {
        let beforeTypecast = muts.current();
        const typecast = muts.parse(parseTypeAnnotation, 0);
        if (typecast instanceof TypeAnnotationNode) {
          const typecastNode = (() => {
            if (!muts.expect(")", "bracket"))
              return;
            const value = muts.parse(parseExpr, 150);
            if (value instanceof ErrorNode)
              return;
            return new TypecastNode(s, muts.current(), {
              type: typecast,
              value
            });
          })();
          console.log("TYPECAST", typecastNode);
          if (typecastNode)
            return typecastNode;
        }
        muts = beforeTypecast.mut();
        const expr = muts.parse(parseExpr, 0);
        if (!muts.expect(")", "bracket"))
          return muts.err(s, "Expected ')'.");
        return expr.setParserPointer(muts.current());
      }],
      // number
      [numberRegex, "number", (num) => {
        const numtype = muts.expect(numberTypeRegex, "number") ?? (num.includes(".") ? "f" : "i");
        return new NumberNode(s, muts.current(), {
          num: Number(num),
          type: numtype
        });
      }],
      [identRegex, "identifier", (ident) => {
        return muts.match(
          [
            // function call
            ["(", "bracket", () => {
              let args = [];
              while (!muts.isNext(")")) {
                const arg = muts.parse(parseExpr, 0);
                if (arg instanceof ErrorNode)
                  return arg;
                args.push(arg);
                if (!muts.expect(",", "comma"))
                  break;
              }
              if (!muts.expect(")", "bracket"))
                return muts.err(s, "Expected a ')'.");
              return new FunctionCallNode(s, muts.current(), {
                args,
                name: ident
              });
            }]
          ],
          // identifier
          () => new IdentifierNode(s, muts.current(), {
            name: ident
          })
        );
      }]
    ], () => muts.err(s, "Expected a number or an identifier."));
  }
  function parseConsequentExpr(left) {
    console.log("LEFT", left);
    const muts = left.end.mut();
    return muts.match([
      // binary op
      [opRegex, "operator", (op) => {
        const bp = getBindingPowerOfNextToken(left.end);
        const right = muts.parse(parseExpr, bp);
        return new BinaryOpNode(left.start, muts.current(), {
          left,
          right,
          op
        });
      }]
    ], () => muts.err(left.start, "Expected a binary operator."));
  }

  // src/index.tsx
  var TEST2 = `

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
  function retrieveNullTerminatedString(mem, i) {
    const uint8array = new Uint8Array(mem);
    const dst = [];
    while (uint8array[i] != 0) {
      dst.push(uint8array[i++]);
    }
    return new Uint8Array(dst).buffer;
  }
  var parseInput = new ParseInput(TEST2, 0, 0);
  var tree = parseStatementList(parseInput);
  console.log(tree);
  console.log(tree.debug());
  var globalMem = {
    mem: new ArrayBuffer(0)
  };
  allocateStringLiterals(tree, globalMem);
  var stdout = "";
  var finalState = tree.exec(new ExecutionContext({
    littleEndian: true,
    memory: concatArrayBuffers(globalMem.mem, new ArrayBuffer(256)),
    stack: [{
      blocks: [{
        bindings: /* @__PURE__ */ new Map()
      }],
      base: globalMem.mem.byteLength,
      bindings: /* @__PURE__ */ new Map(),
      temporaries: [],
      functionDefinitions: /* @__PURE__ */ new Map([["printf", {
        type: "external",
        def(ctx, args) {
          ctx = ctx.clone();
          ctx = args[0].exec(ctx);
          const value = ctx.popTempValueAndGetData();
          const text = new TextDecoder().decode(retrieveNullTerminatedString(ctx.memory, value));
          stdout += text;
          return ctx;
        }
      }], ["putc", {
        type: "external",
        def(ctx, args) {
          ctx = ctx.clone();
          ctx = args[0].exec(ctx);
          const value = ctx.popTempValueAndGetData();
          console.log(value);
          stdout += String.fromCharCode(Number(value));
          return ctx;
        }
      }]]),
      freed: false,
      returnType: {
        definition: DefaultPrimitives.int,
        pointers: 0
      },
      argc: 0
    }],
    esp: globalMem.mem.byteLength,
    types: DefaultTypes
  }));
  console.log(finalState, new Uint8Array(finalState.memory));
  console.log("STDOUT\n", stdout);
})();
//# sourceMappingURL=index.js.map
