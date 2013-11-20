(function (exports) {

  // Extend as needed.
  var lang = exports.lang = {
    Node: {
    },

    Program: {
      extends: "Node",
      fields:  ["@body"]
    },

    Statement: {
      extends: "Node"
    },

    EmptyStatement: {
      extends: "Statement"
    },

    BlockStatement: {
      extends: "Statement",
      fields:  ["@body", "@inline"]
    },

    ExpressionStatement: {
      extends: "Statement",
      fields:  ["@expression"]
    },

    IfStatement: {
      extends: "Statement",
      fields:  ["@test", "@consequent", "@alternate"]
    },

    LabeledStatement: {
      extends: "Statement",
      fields:  ["@label", "@body"]
    },

    BreakStatement: {
      extends: "Statement",
      fields:  ["@label"]
    },

    ContinueStatement: {
      extends: "Statement",
      fields:  ["@label"]
    },

    WithStatement: {
      extends: "Statement",
      fields:  ["@object", "@body"]
    },

    SwitchStatement: {
      extends: "Statement",
      fields:  ["@discriminant", "@cases", "lexical"],
    },

    ReturnStatement: {
      extends: "Statement",
      fields:  ["@argument"]
    },

    ThrowStatement: {
      extends: "Statement",
      fields:  ["@argument"]
    },

    TryStatement: {
      extends: "Statement",
      fields:  ["@block", "@handlers", "@finalizer"]
    },

    WhileStatement: {
      extends: "Statement",
      fields:  ["@test", "@body"]
    },

    DoWhileStatement: {
      extends: "Statement",
      fields:  ["@body", "@test"]
    },

    ForStatement: {
      extends: "Statement",
      fields:  ["@init", "@test", "@update", "@body"]
    },

    ForInStatement: {
      extends: "Statement",
      fields:  ["@left", "@right", "@body", "each"]
    },

    LetStatement: {
      extends: "Statement",
      fields:  ["@head", "@body"]
    },

    DebuggerStatement: {
      extends: "Statement"
    },

    Declaration: {
      extends: "Statement"
    },

    FunctionDeclaration: {
      extends: "Declaration",
      fields:  ["@id", "@modifiers", "@params", "@body", "@decltype", "generator", "expression"]
    },

    VariableDeclaration: {
      extends: "Declaration",
      fields:  ["kind", "@declarations", "global"]
    },

    VariableDeclarator: {
      extends: "Node",
      fields:  ["@id", "@init", "@decltype", "@arguments", "global"]
    },

    Expression: {
      extends: "Pattern"
    },

    ThisExpression: {
      extends: "Expression"
    },

    ArrayExpression: {
      extends: "Expression",
      fields:  ["@elements"]
    },

    ObjectExpression: {
      extends: "Expression",
      fields:  ["@properties"]
    },

    Property: {
      extends: "Node",
      fields:  ["@key", "@value", "kind"],
    },

    FunctionExpression: {
      extends: "Expression",
      fields:  ["@id", "@params", "@body", "@decltype", "generator", "expression"]
    },

    SequenceExpression: {
      extends: "Expression",
      fields:  ["@expressions"]
    },

    UnaryExpression: {
      extends: "Expression",
      fields:  ["operator", "@argument", "prefix"]
    },

    BinaryExpression: {
      extends: "Expression",
      fields:  ["operator", "@left", "@right"]
    },

    AssignmentExpression: {
      extends: "Expression",
      fields:  ["@left", "operator", "@right"]
    },

    UpdateExpression: {
      extends: "Expression",
      fields:  ["operator", "@argument", "prefix"]
    },

    LogicalExpression: {
      extends: "Expression",
      fields:  ["operator", "@left", "@right"]
    },

    ConditionalExpression: {
      extends: "Expression",
      fields:  ["@test", "@consequent", "@alternate"]
    },

    NewExpression: {
      extends: "Expression",
      fields:  ["@callee", "@arguments"]
    },

    CallExpression: {
      extends: "Expression",
      fields:  ["@callee", "@arguments"]
    },

    MemberExpression: {
      extends: "Expression",
      fields:  ["@object", "@property", "computed", "kind"]
    },

    YieldExpression: {
      extends: "Expression",
      fields:  ["@argument"]
    },

    ComprehensionExpression: {
      extends: "Expression",
      fields:  ["@blocks", "@filter"]
    },

    GeneratorExpression: {
      extends: "Expression",
      fields:  ["@blocks", "@filter"]
    },

    LetExpression: {
      extends: "Expression",
      fields:  ["@head", "@body"]
    },

    Pattern: {
      extends: "Node"
    },

    ObjectPattern: {
      extends: "Pattern",
      fields:  ["@properties"]
    },

    ArrayPattern: {
      extends: "Pattern",
      fields:  ["@elements"]
    },

    SwitchCase: {
      extends: "Node",
      fields:  ["@test", "@consequent"]
    },

    CatchClause: {
      extends: "Node",
      fields:  ["@param", "@guard", "@body"]
    },

    Identifier: {
      extends: "Expression",
      fields:  ["name", "kind"]
    },

    Literal: {
      extends: "Expression",
      fields:  ["value", "forceDouble"]
    },

    Type: {
      extends: "Node"
    },

    PointerType: {
      extends: "Type",
      fields: ["@base"]
    },

    ArrayType: {
      extends: "PointerType",
      fields: ["length"]
    },

    StructType: {
      extends: "Type",
      fields: ["@id", "@members", "isUnion"]
    },

    MemberDeclarator: {
      extends: "Node",
      fields: ["modifiers", "@declarator"]
    },

    ArrowType: {
      extends: "Type",
      fields: ["@params", "@return"]
    },

    TypeIdentifier: {
      extends: "Type",
      fields: ["name"]
    },

    TypeAliasDirective: {
      extends: "Node",
      fields: ["@original", "@alias"]
    },

    CastExpression: {
      extends: "Expression",
      fields: ["@as", "@argument"]
    },

    ImportExpression: {
      extends: "Node",
      fields: ["imports", "from"]
    }
  };

  function allFields(spec) {
    // Make the location a special last field.
    var fields = ["leadingComments", "loc"];
    while (spec) {
      if (spec.fields) {
        fields = spec.fields.concat(fields);
      }
      spec = spec.extends ? lang[spec.extends] : null;
    }
    return fields;
  };
  exports.allFields = allFields;

  function prefixUnderscore(s) {
    return "_" + s;
  }

  function ensureConstructor(name, spec) {
    if (!exports[name]) {
      // Make a new constructor if it doesn't exist yet.
      var fields = allFields(spec);
      var children = [];
      var body = ["this.type = \"" + name + "\";"];
      for (var i = 0, j = fields.length; i < j; i++) {
        var fname = fields[i];
        if (fname.charAt(0) === "@") {
          fields[i] = fname = fname.substr(1);
          children.push(fname);
        }
        body.push("this." + fname + " = _" + fname + ";");
      }
      // Prefix parameter names with underscores so keywords work.
      var node = new Function(fields.map(prefixUnderscore), body.join("\n"));

      // Hook up the prototypes.
      if (spec.extends) {
        var pnode = ensureConstructor(spec.extends, lang[spec.extends]);
        node.prototype = Object.create(pnode.prototype);
      }

      Object.defineProperty(node.prototype, "_children",
                            { value: children,
                              writable: true,
                              configurable: true,
                              enumerable: false });

      exports[name] = node;
    }
    return exports[name];
  }

  // Build constructors out of the language spec.
  for (var name in lang) {
    ensureConstructor(name, lang[name]);
  }

  // Make a walk function (map and replace) named |name|. By default it
  // walks the ASexports bottom-up. If different behavior is needed for different
  // nodes, override the walk function explicitly on those nodes.
  //
  // Returning null means "delete this null". Any other falsey values means
  // identity.
  exports.makePass = function makePass(name, prop) {
    return function (o) {
      var trans, arr;
      var child, children = this._children;
      for (var i = 0, j = children.length; i < j; i++) {
        if (!(child = this[children[i]])) {
          continue;
        }

        if (child instanceof Array) {
          arr = this[children[i]] = [];
          for (var k = 0, l = child.length; k < l; k++) {
            if (!child[k]) {
              arr.push(child[k]);
            } else if (typeof child[k][name] === "function") {
              trans = child[k][name](o);
              if (trans !== null) {
                arr.push(trans);
              }
            }
          }
        } else if (typeof child[name] === "function") {
          trans = child[name](o);
          if (trans === null) {
            this[children[i]] = undefined;
          } else {
            this[children[i]] = trans;
          }
        }
      }

      if (typeof this[prop] === "function") {
        if (o.logger && typeof this.loc !== "undefined") {
          o.logger.push(this);
          trans = this[prop](o);
          o.logger.pop();
        } else {
          trans = this[prop](o);
        }
        if (trans === null) {
          return null;
        }
        return trans ? trans : this;
      }

      return this;
    };
  };

  exports.lift = function lift(raw) {
    if (!raw) {
      return raw;
    }
    
    if (raw instanceof Array) {
      return raw.map(function (r) {
        return lift(r);
      });
    }

    var type = raw.type;
    var Node = exports[type];
    if (!Node) {
      throw new Error("unknown node type `" + type + "'");
    }

    var node = new Node();
    node.loc = raw.loc;
    var fields = allFields(lang[type]);
    for (var i = 0, j = fields.length; i < j; i++) {
      var field;
      if (fields[i].charAt(0) === "@") {
        field = fields[i].substr(1);
        if (raw[field]) {
          node[field] = lift(raw[field]);
        }
      } else {
        field = fields[i];
        node[field] = raw[field];
      }
    }

    return node;
  };

  exports.flatten = function flatten(node) {
    if (!node) {
      return node;
    }
    if (node instanceof Array) {
      return node.map(function (n) {
        return flatten(n);
      });
    }

    var type = node.type;
    var raw = { type: type };
    var fields = allFields(lang[type]);
    for (var i = 0, j = fields.length; i < j; i++) {
      var field;
      if (fields[i].charAt(0) === "@") {
        field = fields[i].substr(1);
        if (node[field]) {
          raw[field] = flatten(node[field]);
        } else {
          raw[field] = null;
        }
      } else {
        field = fields[i];
        raw[field] = node[field];
      }
    }

    return raw;
  };

})(typeof exports === "undefined" ? (estransform = {}) : exports);
(function (exports) {
  /**
   * Types.
   */

  function tystr(type, lvl) {
    return type ? type.toString(lvl) : "dyn";
  }

  function TypeAlias(name) {
    this.name = name;
  };

  function PrimitiveType(name, size, defaultValue, signed) {
    this.name = name;
    this.size = size;
    this.signed = signed;
    this.defaultValue = defaultValue;
    this.align = this;
  };

  PrimitiveType.prototype.toString = function () {
    return this.name;
  };

  PrimitiveType.prototype.lint = function () {};

  function StructType(name) {
    this.name = name;
    this.members = [];
    this.fields = [];
    this.offset = 0;
    this.isUnion = false;
    this.staticType = this instanceof StructStaticType ?
                      this : new StructStaticType(this);
  }

  StructType.prototype.toString = function (lvl) {
    lvl = lvl || 0;
    if (lvl > 0) {
      return this.name || "<anon struct>";
    }
    var s = "struct" + (this.name ? (" " + this.name) : " ") + " { ";
    s += this.fields.map(function (f) {
      return tystr(f.type, lvl + 1) + " " + f.name;
    }).join("; ");
    return s + " }";
  };

  StructType.prototype.getMember = function getMember(name) {
    var members = this.members;
    for (var i = 0; i < members.length; i++) {
      if (members[i].name === name) {
        return members[i];
      }
    }
    return null;
  };

  function StructStaticType(type) {
    this.instanceType = type;
    StructType.call(this, type.name + "_Static");
  }

  StructStaticType.prototype = Object.create(StructType.prototype);

  function PointerType(base) {
    this.base = base;
  }

  PointerType.prototype.defaultValue = 0;
  PointerType.prototype.size = 4;

  PointerType.prototype.toString = function (lvl) {
    lvl = lvl || 0;
    return tystr(this.base, lvl + 1) + "*";
  };

  function ArrayType(base, length) {
    PointerType.call(this, base);
    this.length = length;
  }

  ArrayType.prototype = Object.create(PointerType.prototype);
  ArrayType.prototype.toString = function (lvl) {
    lvl = lvl || 0;
    var lengths = "";
    var base = this;
    while (base instanceof ArrayType) {
      lengths += '[' + (base.length !== undefined ? base.length : "*") + ']';
      base = base.base;
    }
    return tystr(base, lvl + 1) + lengths;
  };
  /**
   * Gets the root element type.
   */
  ArrayType.prototype.getRoot = function () {
    var base = this;
    while (base instanceof ArrayType) {
      base = base.base;
    }
    return base;
  };
  ArrayType.prototype.defaultValue = undefined;

  function ArrowType(paramTypes, returnType) {
    this.paramTypes = paramTypes;
    this.returnType = returnType;
  }

  ArrowType.prototype.toString = function () {
    return tystr(this.returnType, 0) + "(" + this.paramTypes.map(function (pty) {
      return tystr(pty, 0);
    }).join(", ") + ")";
  };

  const u8ty  = new PrimitiveType("u8",  1, 0, false);
  const i8ty  = new PrimitiveType("i8",  1, 0, true);
  const u16ty = new PrimitiveType("u16", 2, 0, false);
  const i16ty = new PrimitiveType("i16", 2, 0, true);
  const u32ty = new PrimitiveType("u32", 4, 0, false);
  const i32ty = new PrimitiveType("i32", 4, 0, true);
  const f32ty = new PrimitiveType("f32", 4, 0, undefined);
  const f64ty = new PrimitiveType("f64", 8, 0, undefined);

  const wordTy = u32ty;
  const voidTy = new PrimitiveType("void", 0, 0, undefined);
  const nullTy = new PrimitiveType("null", 0, 0, undefined);
  const bytePointerTy = new PointerType(u8ty);
  const spTy = new PointerType(u32ty);

  const mallocTy = new ArrowType([u32ty], bytePointerTy);
  const freeTy = new ArrowType([bytePointerTy], voidTy);

  function createMemcpyType(pointerTy) {
    return new ArrowType([pointerTy, pointerTy, u32ty], pointerTy);
  }

  function createMemsetType(pointerTy) {
    return new ArrowType([pointerTy, pointerTy.base, u32ty], voidTy);
  }

  const memcpyTy  = createMemcpyType(bytePointerTy);
  // const memcpy2Ty = createMemcpyType(new PointerType(u16ty));
  // const memcpy4Ty = createMemcpyType(new PointerType(u32ty));
  const memsetTy  = createMemsetType(bytePointerTy);
  // const memset2Ty = createMemsetType(new PointerType(u16ty));
  // const memset4Ty = createMemsetType(new PointerType(u32ty));

  u8ty.integral = u8ty.numeric = true;
  i8ty.integral = i8ty.numeric = true;
  u16ty.integral = u16ty.numeric = true;
  i16ty.integral = i16ty.numeric = true;
  u32ty.integral = u32ty.numeric = true;
  i32ty.integral = i32ty.numeric = true;
  f32ty.numeric = true;
  f64ty.numeric = true;

  var builtinTypes = {
    u8:     u8ty,
    i8:     i8ty,
    u16:    u16ty,
    i16:    i16ty,
    u32:    u32ty,
    i32:    i32ty,
    f32:    f32ty,
    f64:    f64ty,

    num:    f64ty,
    int:    i32ty,
    uint:   u32ty,
    bool:   i32ty,
    float:  f32ty,
    double: f64ty,

    byte:   u8ty,
    word:   u32ty,

    void:   voidTy,
    null:   nullTy,
    dyn:    undefined
  };

  PointerType.prototype.align = u32ty;

  exports.TypeAlias = TypeAlias;
  exports.PrimitiveType = PrimitiveType;
  exports.StructType = StructType;
  exports.StructStaticType = StructStaticType;
  exports.PointerType = PointerType;
  exports.ArrayType = ArrayType;
  exports.ArrowType = ArrowType;
  exports.builtinTypes = builtinTypes;

  exports.tystr = tystr;

  exports.u8ty  = u8ty;
  exports.i8ty  = i8ty;
  exports.u16ty = u16ty;
  exports.i16ty = i16ty;
  exports.u32ty = u32ty;
  exports.i32ty = i32ty;
  exports.f32ty = f32ty;
  exports.f64ty = f64ty;

  exports.wordTy = wordTy;
  exports.voidTy = voidTy;
  exports.nullTy = nullTy;
  exports.bytePointerTy = bytePointerTy;
  exports.spTy = spTy;

  exports.mallocTy = mallocTy;
  exports.freeTy = freeTy;

  exports.memsetTy = memsetTy;
  // exports.memset2Ty = memset2Ty;
  // exports.memset4Ty = memset4Ty;

  exports.memcpyTy = memcpyTy;
  // exports.memcpy2Ty = memcpy2Ty;
  // exports.memcpy4Ty = memcpy4Ty;

}).call(this, typeof exports === "undefined" ? (Types = {}) : exports);
(function (exports) {
  var T, Types;
  if (typeof process !== "undefined") {
    T = require("./estransform.js");
    Types = require("./types.js");
  } else {
    T = estransform;
    Types = this.Types;
  }

  const CastExpression = T.CastExpression;
  const BinaryExpression = T.BinaryExpression;
  const Literal = T.Literal;
  const MemberExpression = T.MemberExpression;
  const SequenceExpression = T.SequenceExpression;
  const UnaryExpression = T.UnaryExpression;

  function realign(expr, lalign) {
    assert(expr.ty instanceof Types.PointerType);
    var ralign = expr.ty.base.align.size;

    if (lalign === ralign) {
      return expr;
    }

    var ratio, op;
    if (lalign < ralign) {
      ratio = ralign / lalign;
      op = "<<";
    } else {
      ratio = lalign / ralign;
      op = ">>";
    }

    //return expr;
    return new BinaryExpression(op, expr, new Literal(log2(ratio)), expr.loc);
  }

  function alignAddress(base, byteOffset, ty) {
    //var address = realign(base, ty.align.size);
    var address = base;

    if(byteOffset != 0) {
      assert(isAlignedTo(byteOffset, ty.align.size), "unaligned byte offset " + byteOffset +
             " for type " + quote(ty) + " with alignment " + ty.align.size);
      address = forceType(
        new BinaryExpression("+",
                             address,
                             new Literal(byteOffset), address.loc),
        Types.i32ty
      );
    }

    // asm.js requires a byte pointer to be shifted the appropriate
    // amount to access typed arrays

    address = new BinaryExpression(
      ">>",
      address,
      new Literal(log2(ty.align.size)),
      address.loc
    );

    // Remember (coerce) the type of the address for realign, but *do not* cast.
    address.ty = new Types.PointerType(ty);
    return address;
  }

  function dereference(address, byteOffset, ty, scope, loc) {
    assert(scope);
    address = copy(address, address.ty);
    address = alignAddress(address, byteOffset, ty);

    var expr;
    if (ty instanceof Types.ArrayType) {
      // Remove the bitshift to access the raw byte pointer
      expr = address.left;
    } else {
      expr = new MemberExpression(scope.getView(ty), address, true, loc);
    }
    // Remember (coerce) the type so we can realign, but *do not* cast.
    expr.ty = ty;
    return expr;
  }

  function forceType(expr, type, forceSigned) {
    if(type || expr.ty) {
      type = type || expr.ty;

      if(type.numeric && !type.integral) {
        return cast(new UnaryExpression('+', expr), type);
      }
      else if((type.numeric && type.signed) || forceSigned || type instanceof Types.PointerType) {
        if(!(expr instanceof BinaryExpression &&
             expr.operator == '|' &&
             expr.right instanceof Literal &&
             expr.right.value == 0)) {
          return cast(new BinaryExpression('|', expr, new Literal(0)), type);
        }
        else {
          return cast(expr, type);
        }
      }
      else if(type.numeric) {
        return cast(new BinaryExpression('>>>', expr, new Literal(0)), type);
      }
    }

    return expr;
  }

  function cast(node, ty, force, really) {
    if ((node.ty || force) && (node.ty !== ty || really)) {
      node = new CastExpression(undefined, node, node.loc);
      node.force = force;
    }
    node.ty = ty;
    return node;
  }

  function isInteger(x) {
    return (parseInt(x) | 0) === Number(x);
  }

  function isPowerOfTwo(x) {
    return x && ((x & (x - 1)) === 0);
  }

  function log2(x) {
    assert (isPowerOfTwo(x), "Value " + x + " is not a power of two.");
    return Math.log(x) / Math.LN2;
  }

  function div4(x) {
    assert (x % 4 === 0, "Value " + x + " is not divisible by four.");
    return x / 4;
  }

  function isAlignedTo(offset, alignment) {
    return offset & ~(alignment - 1);
  }

  function alignTo(offset, alignment) {
    return (offset + (alignment - 1)) & ~(alignment - 1);
  }

  function assert(condition, message) {
    if (!condition) {
      throw new Error(message);
    }
  }

  function clone(obj) {
    var o = {};
    for (var key in obj) {
      o[key] = obj[key];
    }
    return o;
  }

  function extend(old, props) {
    var newObj = Object.create(old);
    if (props) {
      for (var key in props) {
        newObj[key] = props[key];
      }
    }
    return newObj;
  }

  function quote(s) {
    return "`" + s + "'";
  }

  function paren(s) {
    return "(" + s + ")";
  }

  function unparen(s) {
    if (s[0] === "(" && s[s.length - 1] === ")") {
      return s.substring(1, s.length - 1);
    }
    return s;
  }

  function copy(node, ty) {
    node = new SequenceExpression([node], node.loc);
    node.ty = ty;
    return node;
  }

  var OptParser = (function () {
    function OptParser(flatspec) {
      // ['a', 'arg', default, 'help string']
      var longs = this.longs = {};
      var shorts = this.shorts = {};
      this.spec = flatspec.map(function (s) {
        var o = { name: s[1], short: s[0], default: s[2], help: s[3] };
        if (s[1]) {
          longs[s[1]] = o;
        }
        if (s[0]) {
          shorts[s[0]] = o;
        }
        return o;
      });
    }

    function flushLeft(s, width) {
      var str = s;
      for (var i = 0, j = width - str.length; i < j; i++) {
        str += " ";
      }
      return str;
    }

    OptParser.prototype = {
      // Count options whose default value is a number, and swallow up
      // following argument of options whose default value is a
      // string.
      //
      // The regexps are lifted from node-optimist [1],
      // Copyright (c) 2010 James Halliday, MIT license.
      //
      // [1] https://github.com/substack/node-optimist
      parse: function (argv) {
        var error = (typeof console === "undefined") ? print : console.error;
        var spec = this.spec;
        var opts = {};
        var argc = argv.length;
        var finished = 0;

        for (var i = 0; i < argc; i++) {
          var arg = argv[i];
          var match;

          if (arg.charAt(0) === "-" && finished > 0) {
            error("malformed options");
            return null;
          }

          if (arg.match(/^--.+=/)) {
            match = arg.match(/^--([^=]+)=(.*)/);
            if (!this.longs[match[1]]) {
              error("unknown option --" + match[1]);
              return null;
            }
            opts[match[1]] = match[2];
          } else if (arg.match(/^--.+/)) {
            match = arg.match(/^--(.+)/);
            if (!this.longs[match[1]]) {
              error("unknown option --" + match[1]);
              return null;
            }
            var lspec = this.longs[match[1]];
            if (typeof lspec.default === "number") {
              if (!opts[match[1]]) {
                opts[match[1]] = 1;
              } else {
                opts[mathc[1]]++;
              }
            } else if (typeof lspec.default === "string" &&
                     (argv[i + 1] && argv[i + 1].charAt(0) !== "-")) {
              opts[match[1]] = argv[i + 1];
              i++;
            } else {
              opts[match[1]] = true;
            }
          } else if (arg.match(/^-[^-]+/)) {
            var sspec;
            match = arg.match(/^-(.+)/);
            if (sspec = this.shorts[match[1]]) {
              var optname = sspec.name ? sspec.name : match[1];

              if (typeof sspec.default === "number") {
                if (!opts[optname]) {
                  opts[optname] = 1;
                } else {
                  opts[optname]++;
                }
              } else if (typeof sspec.default === "string" &&
                         (argv[i + 1] && argv[i + 1].charAt(0) !== "-")) {
                opts[optname] = argv[i + 1];
                i++;
              } else {
                opts[optname] = true;
              }
            } else {
              var letters = arg.slice(1).split('');
              for (var j = 0, k = letters.length; j < k; j++) {
                var sspec = this.shorts[letters[j]];
                if (!sspec) {
                  error("unknown option -" + letters[j]);
                  return null;
                }
                if (typeof sspec.default === "number") {
                  if (!opts[sspec.name]) {
                    opts[sspec.name] = 1;
                  } else {
                    opts[sspec.name]++;
                  }
                } else {
                  opts[sspec.name] = true;
                }
              }
            }
          }
        }

        finished = i - 1;

        for (var i = 0, j = spec.length; i < j; i++) {
          var s = spec[i];
          if (!(s.name in opts)) {
            opts[s.name] = s.default;
          }
        }

        return { options: opts, rest: argv.slice(finished) };
      },

      usage: function () {
        var spec = this.spec;
        var str = "\nOptions:\n";
        var indent = "  ";
        for (var i = 0, j = spec.length; i < j; i++) {
          var s = spec[i];
          str += indent;
          if (s.name) {
            if (s.short) {
              str += flushLeft("-" + s.short, 4) + flushLeft("--" + s.name, 18);
            } else {
              str += flushLeft("", 4) + flushLeft("--" + s.name, 18);
            }
          } else {
            str += flushLeft("-" + s.short, 22);
          }
          str += s.help + "\n";
        }
        return str;
      }
    }

    return OptParser;
  })();

  /**
   * Logger
   */

  var Logger = (function () {
    var info, warn, error;
    if (typeof console === "undefined") {
      info = warn = error = print;
    } else {
      info = console.info;
      warn = console.warn;
      error = console.error;
    }

    const black   = 0;
    const red     = 1;
    const green   = 2;
    const yellow  = 3;
    const blue    = 4;
    const magenta = 5;
    const cyan    = 6;
    const white   = 7;

    const normal  = 0;
    const bold    = 1;

    const startANSI = '\033[';
    const clearANSI = startANSI + '0m';

    function ansi(s, style, fg, bg) {
      var a = '\033[';

      var modifiers = []
      if (style) {
        modifiers.push(style);
      }
      if (fg) {
        modifiers.push("3" + fg);
      }
      if (bg) {
        modifiers.push("4" + fg);
      }
      return startANSI + modifiers.join(";") + 'm' + s + clearANSI;
    }

    function Logger(program, name, source, options) {
      this.id = 1;
      this.program = program;
      this.name = name;
      this.options = options;
      this.verbosity = options.trace ? 3 : (options.warn ? 2 : 1);
      this.buffer = [];
      this.context = [];
      if (typeof source !== "string" && !(source instanceof String)) {
        this.source = String(source).split("\n");
      } else {
        this.source = source.split("\n");
      }
    }

    function compareLocations(a, b) {
      var cmp = a.start.line - b.start.line;
      if (cmp === 0) {
        cmp = a.end.line - b.end.line;
        if (cmp === 0) {
          cmp = a.start.column - b.start.column;
          if (cmp === 0) {
            cmp = a.end.column - b.end.column;
          }
        }
      }
      return cmp;
    }

    const severity = { info: 1, warn: 2, error: 3 };

    Logger.prototype = {
      push: function (node) {
        this.context.push(node);
      },

      pop: function () {
        this.context.pop();
      },

      _format: function (prefix, kind, message) {
        if (this.options["simple-log"]) {
          return prefix + " " + kind + " " + message;
        }

        switch (kind) {
        case "info":
          kind = ansi("info:", bold);
          break;
        case "warn":
          kind = ansi("warning:", bold, magenta);
          break;
        case "error":
          kind = ansi("error:", bold, red);
          break;
        }

        return ansi(prefix, bold) + " " + kind + " " + ansi(message, bold);
      },

      _underlinedSnippet: function (loc) {
        const indent = "  ";
        var underline = "";
        var line = this.source[loc.start.line - 1];

        for (var i = 0, j = line.length; i < j; i++) {
          var c;
          if (i === loc.start.column) {
            underline += "^";
          } else if (loc.end.line > loc.start.line ||
                     (i > loc.start.column && i <= loc.end.column - 1 &&
                      !(c = line.charAt(i)).match(/\s/))) {
            underline += "~";
          } else {
            underline += " ";
          }
        }

        return indent + line + "\n" + indent + ansi(underline, bold, green);
      },

      _bufferMessage: function (kind, message, loc) {
        if (!loc) {
          var node = this.context[this.context.length - 1];
          if (node && node.loc) {
            loc = node.loc;
          }
        }
        this.buffer.push({ loc: loc, kind: kind, message: message, id: this.id++ });
      },

      info: function (message, loc) {
        if (this.verbosity >= 3) {
          this._bufferMessage("info", message, loc);
        }
      },

      warn: function (message, loc) {
        if (this.verbosity >= 2) {
          this._bufferMessage("warn", message, loc);
        }
      },

      error: function (message, loc) {
        if (this.verbosity >= 1) {
          this._bufferMessage("error", message, loc);
        }
      },

      flush: function () {
        const humanReadable = !this.options["simple-log"];

        // Sort by location. Messages without location are sorted by the order
        // in which they're added.
        var buf = this.buffer.sort(function (a, b) {
          var aloc = a.loc, bloc = b.loc;

          if (!aloc && !bloc) {
            return a.id - b.id;
          }
          if (!aloc && bloc) {
            return -1;
          }
          if (aloc && !bloc) {
            return 1;
          }

          var cmp = compareLocations(aloc, bloc);
          if (cmp === 0) {
            cmp = severity[a.kind] - severity[b.kind];
          }
          return cmp;
        });

        var prev;
        for (var i = 0, buflen = buf.length; i < buflen; i++) {
          var b = buf[i];
          var loc = b.loc;

          var prefix = this.name + ":";
          if (loc) {
            prefix += loc.start.line + ":" + loc.start.column + ":";

            if (prev && prev.loc && compareLocations(loc, prev.loc) === 0 && humanReadable) {
              var spacer = "";
              for (var j = 0, k = prefix.length; j < k; j++) {
                spacer += " ";
              }
              prefix = spacer;
            }
          }

          var formatted = this._format(prefix, b.kind, b.message);
          switch (b.kind) {
          case "info":
            info(formatted);
            break;
          case "warn":
            warn(formatted);
            break;
          case "error":
            error(formatted);
            break;
          }

          if (loc && humanReadable) {
            var next = buf[i + 1];
            if (!next || (next.loc && compareLocations(loc, next.loc) !== 0)) {
              info(this._underlinedSnippet(loc));
            }
          }

          prev = b;
        }
      }
    };

    return Logger;
  })();

  exports.OptParser = OptParser;
  exports.Logger = Logger;
  exports.assert = assert;
  exports.quote = quote;
  exports.clone = clone;
  exports.extend = extend;
  exports.cast = cast;
  exports.isInteger = isInteger;
  exports.isPowerOfTwo = isPowerOfTwo;
  exports.log2 = log2;
  exports.div4 = div4;
  exports.isAlignedTo = isAlignedTo;
  exports.alignTo = alignTo;
  exports.dereference = dereference;
  exports.realign = realign;
  exports.forceType = forceType;

}(typeof exports === 'undefined' ? (util = {}) : exports));
(function (exports) {
  var util, T, Types;
  if (typeof process !== "undefined") {
    util = require("./util.js");
    T = require("./estransform.js");
    Types = require("./types.js");
  } else {
    util = this.util;
    T = estransform;
    Types = this.Types;
  }

  /**
   * Import nodes.
   */
  const Node = T.Node;
  const Literal = T.Literal;
  const Identifier = T.Identifier;
  const VariableDeclaration = T.VariableDeclaration;
  const VariableDeclarator = T.VariableDeclarator;
  const MemberExpression = T.MemberExpression;
  const BinaryExpression = T.BinaryExpression;
  const SequenceExpression = T.SequenceExpression;
  const CallExpression = T.CallExpression;
  const AssignmentExpression = T.AssignmentExpression;
  const ExpressionStatement = T.ExpressionStatement;
  const ReturnStatement = T.ReturnStatement;
  const Program = T.Program;
  const FunctionDeclaration = T.FunctionDeclaration;
  const FunctionExpression = T.FunctionExpression;
  const ConditionalExpression = T.ConditionalExpression;
  const ObjectExpression = T.ObjectExpression;
  const UnaryExpression = T.UnaryExpression;
  const NewExpression = T.NewExpression;
  const UpdateExpression = T.UpdateExpression;
  const ForStatement = T.ForStatement;
  const BlockStatement = T.BlockStatement;
  const CatchClause = T.CatchClause;
  const ThisExpression = T.ThisExpression;
  const TypeAliasDirective = T.TypeAliasDirective;
  const CastExpression = T.CastExpression;

  /**
   * Import utilities.
   */
  const assert = util.assert;
  const cast = util.cast;
  const alignTo = util.alignTo;
  const dereference = util.dereference;
  const forceType = util.forceType;

  /**
   * Import types.
   */
  const TypeAlias = Types.TypeAlias;
  const PrimitiveType = Types.PrimitiveType;
  const StructType = Types.StructType;
  const PointerType = Types.PointerType;
  const ArrayType = Types.ArrayType;
  const ArrowType = Types.ArrowType;

  /**
   * Scopes and Variables
   */

  function Variable(name, type, global) {
    this.name = name;
    this.type = type;
    this.global = global;
    this.isStackAllocated = (type instanceof StructType || type instanceof ArrayType);
  }

  Variable.prototype.toString = function () {
    return Types.tystr(this.type, 0) + " " + this.name;
  };

  Variable.prototype.getStackAccess = function getStackAccess(scope, loc) {
    assert(this.isStackAllocated);
    assert(typeof this.byteOffset !== "undefined", "stack-allocated variable offset not computed.");
    var byteOffset = this.byteOffset;
    var sp;
    if(this.global) {
      sp = forceType(
        new BinaryExpression(
          '-',
          new Identifier('totalSize'),
          new Identifier('globalSP')
        ),
        Types.i32ty
      );
    }
    else {
      sp = scope.SP();
    }

    return dereference(sp, byteOffset, this.type, scope, loc);
  };

  function Scope(parent, name) {
    this.name = name;
    this.parent = parent;
    this.root = parent.root;
    this.variables = Object.create(null);
    this.frame = parent.frame;

    assert(this.frame instanceof Frame);
  }

  Scope.prototype.getVariable = function getVariable(name, local) {
    var variable = this.variables[name];
    if (variable instanceof Variable) {
      return variable;
    }

    if (this.parent && !local) {
      return this.parent.getVariable(name);
    }

    return null;
  };

  Scope.prototype.addVariable = function addVariable(variable, external) {
    assert(variable);
    assert(!variable.frame);
    assert(!this.variables[variable.name], "Scope already has a variable named " + variable.name);
    variable.frame = this.frame;

    var variables = this.variables;
    var name = variable.name;

    variables[name] = variable;
    if (!external) {
      variable.name = this.freshName(name, variable);
      this.frame.scopedVariables[variable.name] = variable;
    }

    //console.log("added variable " + variable + " to scope " + this);
 };

  Scope.prototype.freshName = function freshName(name, variable) {
    var mangles = this.frame.mangles;
    var fresh = 0;
    var freshName = name;

    // Mangle the name if it clases with anything in the root's scope
    // because it gives us control over the scope (easier for asm.js)
    while (mangles[freshName] || this.root.mangles[freshName]) {
      freshName = name + "$" + ++fresh;
    }
    if (variable) {
      mangles[freshName] = variable;
    }
    return freshName;
  };

  Scope.prototype.freshVariable = function freshVariable(name, type) {
    var variable = new Variable(name, type);
    variable.name = this.freshName(name, variable);
    return variable;
  };

  Scope.prototype.freshTemp = function freshTemp(ty, loc, inDeclarator) {
    var t = this.freshVariable("_", ty);
    var id = cast(new Identifier(t.name), ty);
    if (!inDeclarator) {
      var cachedLocals = this.frame.cachedLocals;
      cachedLocals[t.name] = new VariableDeclarator(id);
    }
    return id;
  };

  Scope.prototype.cacheReference = function cacheReference(node) {
    assert(node);

    var def, use;

    if (node instanceof MemberExpression && !(node.object instanceof Identifier)) {
      assert(!node.computed);
      var t = this.freshTemp(node.object.ty, node.object.loc);
      node.object = new AssignmentExpression(t, "=", node.object, node.object.loc);
      var use = new MemberExpression(t, node.property, false, "[]", node.property.loc);
      use.ty = node.ty;
      return { def: node, use: use };
    }

    return { def: node, use: node };
  };

  Scope.prototype.MEMORY = function MEMORY() {
    return this.root.MEMORY();
  };

  Scope.prototype.getView = function getView(type) {
    return this.frame.getView(type);
  };

  Scope.prototype.MALLOC = function MALLOC() {
    return this.frame.MALLOC();
  };

  Scope.prototype.FREE = function FREE() {
    return this.frame.FREE();
  };

  Scope.prototype.MEMCPY = function MEMCPY(size) {
    return this.frame.MEMCPY(size);
  };

  Scope.prototype.MEMSET = function MEMSET(size) {
    return this.frame.MEMSET(size);
  };

  // Scope.prototype.MEMCHECK_CALL_PUSH = function MEMCHECK_CALL_PUSH() {
  //   return this.frame.MEMCHECK_CALL_PUSH();
  // };

  // Scope.prototype.MEMCHECK_CALL_RESET = function MEMCHECK_CALL_RESET() {
  //   return this.frame.MEMCHECK_CALL_RESET();
  // };

  // Scope.prototype.MEMCHECK_CALL_POP = function MEMCHECK_CALL_POP() {
  //   return this.frame.MEMCHECK_CALL_POP();
  // };

  Scope.prototype.toString = function () {
    return this.name;
  };

  function Frame(parent, name) {
    this.name = name;
    this.parent = parent;
    this.root = parent ? parent.root : this;
    this.variables = Object.create(null);
    this.cachedLocals = Object.create(null);
    this.frame = this;
    this.mangles = Object.create(null);
    this.scopedVariables = Object.create(null);
  }

  Frame.prototype = Object.create(Scope.prototype);

  function getBuiltin(frame, name, ty) {
    return cast(new Identifier(frame.root.getVariable(name).name), ty);
  }

  // Frame.prototype.MEMORY = function MEMORY() {
  //   assert(this.root === this);
  //   if (!this.cachedMEMORY) {
  //     this.cachedMEMORY = new Identifier(this.freshVariable("$M").name);
  //   }
  //   return this.cachedMEMORY;
  // };

  Frame.prototype.MALLOC = function MALLOC() {
    return getBuiltin(this, 'malloc', Types.mallocTy);
  };

  Frame.prototype.FREE = function FREE() {
    return getBuiltin(this, 'free', Types.freeTy);
  };

  Frame.prototype.MEMCPY = function MEMCPY(size) {
    return getBuiltin(this, 'memcpy', Types.memcpyTy);
  };

  Frame.prototype.MEMSET = function MEMSET(size) {
    return getBuiltin(this, 'memset', Types.memsetTy);
  };

  // Frame.prototype.MEMCHECK_CALL_PUSH = function MEMCHECK_CALL_PUSH() {
  //   return getCachedLocal(this, "memcheck_call_push", "dyn");
  // };

  // Frame.prototype.MEMCHECK_CALL_RESET = function MEMCHECK_CALL_RESET() {
  //   return getCachedLocal(this, "memcheck_call_reset", "dyn");
  // };

  // Frame.prototype.MEMCHECK_CALL_POP = function MEMCHECK_CALL_POP() {
  //   return getCachedLocal(this, "memcheck_call_pop", "dyn");
  // };

  Frame.prototype.getView = function getView(ty) {
    assert(ty);
    assert(ty.align);

    var alignType = ty.align;
    if (typeof alignType.signed === "undefined") {
      return getBuiltin(this, "F" + alignType.size);
    }
    return getBuiltin(this, (alignType.signed ? "I" : "U") + alignType.size);
  };

  Frame.prototype.SP = function SP() {
    if (!this.cachedSP) {
      this.cachedSP = cast(new Identifier(this.freshVariable("$SP").name), Types.spTy);
    }
    return this.cachedSP;
  };

  Frame.prototype.realSP = function realSP() {
    return cast(new MemberExpression(this.getView(Types.builtinTypes.uint), new Literal(1), true), Types.spTy);
  };

  Frame.prototype.close = function close() {
    const wordSize = Types.wordTy.size;
    var byteOffset = 0;
    var mangles = this.mangles;

    // The SP and frame sizes are in *bytes*, but the alignment is by
    // *double word*, to fit doubles.
    for (var name in mangles) {
      var variable = mangles[name];
      if (mangles[name].isStackAllocated) {
        var size = variable.type.size;
        variable.byteOffset = byteOffset;
        byteOffset += alignTo(size, wordSize * 2);
      }
    }

    this.frameSize = byteOffset;
  };

  exports.Variable = Variable;
  exports.Scope = Scope;
  exports.Frame = Frame;
  //exports.getCachedLocal = getCachedLocal;

}).call(this, typeof exports === "undefined" ? (scope = {}) : exports);

(function (exports) {
  var util, T, S, Types;
  if (typeof process !== "undefined") {
    util = require("./util.js");
    T = require("./estransform.js");
    S = require("./scope.js");
    Types = require("./types.js");
  } else if (typeof snarf !== "undefined") {
    util = this.util;
    T = estransform;
    load("./types.js");
    Types = this.Types;
    load("./scope.js");
    S = scope;
  } else {
    util = this.util;
    T = estransform;
    S = scope;
    Types = this.Types;
  }


  /**
   * Import nodes.
   */
  const Node = T.Node;
  const Literal = T.Literal;
  const Identifier = T.Identifier;
  const ArrayExpression = T.ArrayExpression;
  const VariableDeclaration = T.VariableDeclaration;
  const VariableDeclarator = T.VariableDeclarator;
  const MemberDeclarator = T.MemberDeclarator;
  const MemberExpression = T.MemberExpression;
  const BinaryExpression = T.BinaryExpression;
  const SequenceExpression = T.SequenceExpression;
  const CallExpression = T.CallExpression;
  const AssignmentExpression = T.AssignmentExpression;
  const ExpressionStatement = T.ExpressionStatement;
  const ReturnStatement = T.ReturnStatement;
  const Program = T.Program;
  const FunctionDeclaration = T.FunctionDeclaration;
  const FunctionExpression = T.FunctionExpression;
  const ConditionalExpression = T.ConditionalExpression;
  const ObjectExpression = T.ObjectExpression;
  const UnaryExpression = T.UnaryExpression;
  const NewExpression = T.NewExpression;
  const UpdateExpression = T.UpdateExpression;
  const LogicalExpression = T.LogicalExpression;
  const IfStatement = T.IfStatement;
  const ForStatement = T.ForStatement;
  const BlockStatement = T.BlockStatement;
  const CatchClause = T.CatchClause;
  const ThisExpression = T.ThisExpression;
  const TypeAliasDirective = T.TypeAliasDirective;
  const CastExpression = T.CastExpression;
  const ImportExpression = T.ImportExpression;

  function literal(x) {
    return new Literal(x);
  }

  /**
   * Import utilities.
   */
  const assert = util.assert;
  const quote = util.quote;
  const clone = util.clone;
  const extend = util.extend;
  const cast = util.cast;
  const isInteger = util.isInteger;
  const isPowerOfTwo = util.isPowerOfTwo;
  const log2 = util.log2;
  const div4 = util.div4;
  const isAlignedTo = util.isAlignedTo;
  const alignTo = util.alignTo;
  const dereference = util.dereference;
  const realign = util.realign;
  const forceType = util.forceType;

  /**
   * Import scopes.
   */
  const Variable = S.Variable;
  const Scope = S.Scope;
  const Frame = S.Frame;
  const getCachedLocal = S.getCachedLocal;

  /**
   * Import types.
   */
  const TypeAlias = Types.TypeAlias;
  const PrimitiveType = Types.PrimitiveType;
  const StructType = Types.StructType;
  const StructStaticType = Types.StructStaticType;
  const PointerType = Types.PointerType;
  const ArrayType = Types.ArrayType;
  const ArrowType = Types.ArrowType;

  /**
   * Misc utility functions.
   */

  function check(condition, message, warn) {
    if (!condition) {
      if (warn) {
        logger.warn(message);
      } else {
        logger.error(message);

        var e = new Error(message);
        var loc = logger.context[logger.context.length - 1].loc;
        if (loc) {
          e.lineNumber = loc.start.line;
        }
        e.logged = true;
        throw e;
      }
    }
  }

  /**
   * Pass 1: resolve type synonyms and do some type sanity checking
   */

  T.Type.prototype.reflect = function (o) {
    var ty = this.construct().resolve(o.types);
    if (ty !== undefined) {
      ty.lint();
    }
    return ty;
  };

  T.TypeIdentifier.prototype.construct = function () {
    var ty = new TypeAlias(this.name);
    ty.node = this;
    return ty;
  };

  T.PointerType.prototype.construct = function () {
    var ty = new PointerType(this.base.construct());
    ty.node = this;

    if (this.arraySize) {
      ty.arraySize = this.arraySize;
    }
    return ty;
  };

  T.ArrayType.prototype.construct = function () {
    var ty = new ArrayType(this.base.construct());
    ty.node = this;
    if (this.length) {
      ty.length = this.length;
    }
    return ty;
  };

  T.MemberDeclarator.prototype.construct = function () {
    return {
      name: this.declarator.id.name,
      isStatic: this.modifiers.indexOf("static") >= 0,
      type: this.declarator.decltype.construct()
    };
  };

  T.StructType.prototype.construct = function construct() {
    var ty = new StructType(this.id ? this.id.name : undefined);
    var sty = ty.staticType;
    ty.node = this;
    this.members.forEach(function (m) {
      var member = m.construct();
      (member.isStatic ? sty : ty).members.push(member);
    });
    ty.isUnion = this.isUnion;
    logger.info("Constructed Type: " + ty.name);
    return ty;
  };

  T.ArrowType.prototype.construct = function () {
    return new ArrowType(this.params.map(function (p) { return p.construct(); }),
                         this.return.construct());
  };

  function startResolving(ty) {
    if (ty._resolving) {
      console.error("infinite type");
    }
    ty._resolving = true;
  };

  function finishResolving(ty) {
    delete ty._resolving;
    ty._resolved = true;
  };

  PrimitiveType.prototype.resolve = function () {
    return this;
  };

  TypeAlias.prototype.resolve = function (types, inPointer) {
    startResolving(this);
    check(this.name in types, "unable to resolve type name " + quote(this.name));
    var ty = types[this.name];
    finishResolving(this);
    if (inPointer && ty instanceof TypeAlias) {
      ty = ty.resolve(types, inPointer);
    }
    return ty;
  };

  PointerType.prototype.resolve = function (types) {
    if (this._resolved) {
      return this;
    }
    startResolving(this);
    this.base = this.base.resolve(types, true);
    if (this.arraySize) {
      this.size = this.base.size * this.arraySize;
    }
    finishResolving(this);
    return this;
  };

  ArrayType.prototype.resolve = function (types) {
    if (this._resolved) {
      return this;
    }
    startResolving(this);
    this.base = this.base.resolve(types, true);
    if (this.length) {
      this.size = this.base.size * this.length;
    }
    finishResolving(this);
    return this;
  };

  StructType.prototype.resolve = function (types) {
    if (this._resolved) {
      return this;
    }
    startResolving(this);
    var member, members = this.members;
    for (var i = 0, j = members.length; i < j; i++) {
      member = members[i];
      if (member.type) {
        member.type = member.type.resolve(types);
        if (member.type instanceof ArrowType) {
          member.type.paramTypes.unshift(new PointerType(this));
        }
      }
    }
    if (this.staticType !== this) {
      this.staticType.resolve(types);
    }
    finishResolving(this);
    return this;
  };

  ArrowType.prototype.resolve = function (types) {
    if (this._resolved) {
      return this;
    }

    var paramTypes = this.paramTypes;
    for (var i = 0, j = paramTypes.length; i < j; i++) {
      if (paramTypes[i]) {
        paramTypes[i] = paramTypes[i].resolve(types);
      }
    }
    if (this.returnType) {
      this.returnType = this.returnType.resolve(types);
    }
    return this;
  };

  PointerType.prototype.lint = function () {
    check(this.base, "pointer without base type");
    // check(this.base.size, "cannot take pointer of size 0 type " + quote(Types.tystr(this.base, 0)));
  };

  ArrayType.prototype.lint = function () {
    check(this.base, "array without element type");
    this.base.lint();
    this.align = this.base.align;
  };

  StructType.prototype.lint = function () {
    var maxAlignSize = 1;
    var maxAlignSizeType = Types.u8ty;
    var members = this.members;
    var field, type;
    var prev = { offset: 0, type: { size: 0 } };
    for (var i = 0, j = members.length; i < j; i++) {
      // Ignore member instance functions.
      if (members[i].type instanceof ArrowType) {
        continue;
      }

      this.fields.push(field = members[i]);
      type = field.type;

      // Recursively lint field types.
      if (type) {
        type.lint();
      }

      check(type, "cannot have untyped field");
      check(type.size, "cannot have fields of size 0 type " + quote(Types.tystr(type, 0)));

      if (type.align.size > maxAlignSize) {
        maxAlignSize = type.align.size;
        maxAlignSizeType = type.align;
      }
      if (this.isUnion) {
        field.offset = 0;
      } else {
        field.offset = alignTo(prev.offset + prev.type.size, type.size);
        prev = field;
      }
    }
    if (field) {
      this.size = alignTo(field.offset + field.type.size, maxAlignSize);
    } else {
      this.size = 0;
    }
    this.align = maxAlignSizeType;
    if (this.staticType !== this) {
      this.staticType.lint();
    }
  };

  ArrowType.prototype.lint = function () {
    var paramTypes = this.paramTypes;
    for (var i = 0, j = paramTypes.length; i < j; i++) {
      if (paramTypes[i]) {
        paramTypes[i].lint();
      }
    }
    if (this.returnType) {
      this.returnType.lint();
    }
  };

  function resolveAndLintTypes(root, types) {
    var s, stmts = root.body;
    var alias, aliases = [];
    var ty;
    for (var i = 0, j = stmts.length; i < j; i++) {
      s = stmts[i];
      if (s instanceof TypeAliasDirective) {
        alias = s.alias.name;
        if ((s.original instanceof T.StructType) && s.original.id) {
          types[alias] = types[s.original.id.name] = s.original.construct();
          aliases.push(s.original.id.name);
        } else {
          types[alias] = s.original.construct();
        }
        aliases.push(alias);
      } else if ((s instanceof T.StructType) && s.id) {
        types[s.id.name] = s.construct();
        aliases.push(s.id.name);
      }
    }

    for (var i = 0, j = aliases.length; i < j; i++) {
      ty = types[aliases[i]];
      logger.push(ty.node);
      ty = ty.resolve(types);
      ty.lint();
      types[aliases[i]] = ty;
      logger.pop();
    }

    return types;
  }

  /**
   * Pass 2: build scope information and lint inline types
   */

  function isNull(node) {
    return node instanceof Literal && (node.value === null || node.value === 0);
  }

  Node.prototype.scan = T.makePass("scan", "scanNode");

  function scanList(list, o) {
    for (var i = 0, j = list.length; i < j; i++) {
      if (list[i]) {
        list[i].scan(o);
      }
    }
  }

  T.Type.prototype.scan = function (o) {
    return this;
  };

  Program.prototype.scan = function (o) {
    o = extend(o);

    var types = o.types;
    var scope = new Frame(null, "Program");
    o.scope = this.frame = scope;

    // scope.addVariable(new Variable("exports"), true);
    // scope.addVariable(new Variable("require"), true);
    // scope.addVariable(new Variable("load"), true);

    // scope.addVariable(scope.freshVariable("malloc", Types.mallocTy), true);
    // scope.addVariable(scope.freshVariable("free", Types.freeTy), true);
    scope.addVariable(scope.freshVariable("memcpy", Types.memcpyTy), true);
    scope.addVariable(scope.freshVariable("memset", Types.memsetTy), true);

    scope.addVariable(scope.freshVariable("U1", new Types.ArrayType(Types.u8ty)), true);
    scope.addVariable(scope.freshVariable("I1", new Types.ArrayType(Types.i8ty)), true);
    scope.addVariable(scope.freshVariable("U2", new Types.ArrayType(Types.u16ty)), true);
    scope.addVariable(scope.freshVariable("I2", new Types.ArrayType(Types.i16ty)), true);
    scope.addVariable(scope.freshVariable("U4", new Types.ArrayType(Types.u32ty)), true);
    scope.addVariable(scope.freshVariable("I4", new Types.ArrayType(Types.i32ty)), true);
    scope.addVariable(scope.freshVariable("F4", new Types.ArrayType(Types.f32ty)), true);
    scope.addVariable(scope.freshVariable("F8", new Types.ArrayType(Types.f64ty)), true);

    scope.addVariable(
        scope.freshVariable("sqrt", new Types.ArrowType([Types.f32ty], Types.f32ty)), true
    );

    scope.addVariable(
        scope.freshVariable("abs", new Types.ArrowType([Types.f32ty], Types.f32ty)), true
    );

    scope.addVariable(
        scope.freshVariable("sin", new Types.ArrowType([Types.f32ty], Types.f32ty)), true
    );

    scope.addVariable(
        scope.freshVariable("cos", new Types.ArrowType([Types.f32ty], Types.f32ty)), true
    );

    logger.push(this);
    scanList(this.body, o);
    logger.pop();

    return this;
  };

  MemberDeclarator.prototype.scan = function (o) {
    if (this.declarator instanceof FunctionDeclaration) {
      this.declarator.scan(o);
     }
  };

  TypeAliasDirective.prototype.scan = function (o) {
    if (this.original instanceof T.StructType) {
      var structName = this.original.id.name;
      var structType = o.types[structName];
      o.scope.addVariable(new Variable(structType.name, structType.staticType));
      var io = extend(o, {
        thisTy: new PointerType(structType),
        scope: new Frame(o.scope, "Struct " + structName)
      });
      var so = extend(o, {
        thisTy: new PointerType(structType.staticType),
        scope: new Frame(o.scope, "Static Struct " + structName)
      });
      var members = this.original.members;
      for (var i = 0; i < members.length; i++) {
        var isStatic = members[i].modifiers.indexOf("static");
        members[i].scan(isStatic >= 0 ? so : io);
      }
    }
    return this;
  };

  FunctionExpression.prototype.scan =
  FunctionDeclaration.prototype.scan = function (o) {
    logger.push(this);
    var scope = o.scope;

    var ty;
    if (this.decltype) {
      ty = this.decltype.reflect(o);
    }
    if (this.id) {
      logger.push(this.id);
      scope.addVariable(new Variable(this.id.name, ty));
      logger.pop();
    }

    o = extend(o);
    scope = new Frame(scope, "Function " + (this.id ? this.id.name : "anonymous"));
    scope.returnType = ty.returnType;
    o.scope = this.frame = scope;

    if (o.thisTy) {
      var variable = new Variable("this", o.thisTy);
      scope.addVariable(variable);
    }

    var params = this.params;
    var parameters = this.parameters = [];
    var variable;
    for (var i = 0, j = params.length; i < j; i++) {
      logger.push(params[i]);
      variable = new Variable(params[i].name, ty.paramTypes[i]);
      scope.addVariable(variable);
      parameters.push(variable);
      logger.pop();
    }

    assert(this.body instanceof BlockStatement);
    scanList(this.body.body, o);

    logger.pop();
    return this;
  };

  VariableDeclaration.prototype.scan = function (o) {
    logger.push(this);

    check(this.kind === "let" || this.kind === "const" || this.kind === "extern",
          "Only block scoped variable declarations are allowed, use the " + quote("let") + " keyword instead.");

    /* Only emit vars, we mangle names ourselves. */
    if (this.kind === "let") {
      this.kind = "var";
    }

    scanList(this.declarations, extend(o, { declkind: this.kind }));

    logger.pop();
    return this;
  };

  VariableDeclarator.prototype.scanNode = function (o) {
    var types = o.types;
    var scope = o.scope;

    var name = this.id.name;
    var ty = this.decltype ? this.decltype.reflect(o) : undefined;

    check(!scope.getVariable(name, true),
          "Variable " + quote(name) + " is already declared in local scope.");

    scope.addVariable(new Variable(name, ty), o.declkind === "extern");
  };

  ForStatement.prototype.scan = function (o) {
    o = extend(o);
    o.scope = this.scope = new Scope(o.scope, "ForStatement", "block");
    Node.prototype.scan.call(this, o);
    return this;
  };

  // Note: Does not handle conditional catch clauses
  // Then again, neither does esprima
  CatchClause.prototype.scan = function (o) {
    logger.push(this);

    this.body.scan(o);

    logger.push(this.param);
    this.body.scope.addVariable(new Variable(this.param.name, undefined));
    logger.pop();

    logger.pop();
    return this;
  };

  BlockStatement.prototype.scan = function (o) {
    o = extend(o);
    o.scope = this.scope = new Scope(o.scope, "BlockStatement", "block");
    scanList(this.body, o);
    return this;
  };

  /**
   * Pass 3: type transform
   */

  PrimitiveType.prototype.assignableFrom = function (other) {
    if(this === Types.voidTy) {
      return other === Types.voidTy;
    }

    if (other instanceof PrimitiveType ||
        other instanceof PointerType) {
      return true;
    }
    return false;
  };

  StructType.prototype.assignableFrom = function (other) {
    return this === other;
  };

  PointerType.prototype.assignableFrom = function (other) {
    if (other === Types.nullTy
        || (other instanceof PointerType && this.base.assignableFrom(other.base))
        || (other instanceof PrimitiveType && other.integral)) {
      return true;
    }
    return false;
  };

  ArrowType.prototype.assignableFrom = function (other) {
    if (!(other instanceof ArrowType)) {
      return false;
    }

    var paramTypes = this.paramTypes;
    var otherParamTypes = other.paramTypes;

    for (var i = 0, j = paramTypes.length; i < j; i++) {
      if (otherParamTypes.length <= i) {
        if (paramTypes[i] !== undefined) {
          // Other arrow has too few params, and this param isn't dyn
          return false;
        } else {
          continue;
        }
      }

      if (paramTypes[i] === undefined) {
        if (otherParamTypes[i] !== undefined) {
          return false;
        }
      } else {
        if (!paramTypes[i].assignableFrom(otherParamTypes[i])) {
          return false;
        }
      }
    }

    for (var i = paramTypes.length, j = otherParamTypes.length; i < j; i++) {
      if (otherParamTypes[i] !== undefined) {
        // Other arrow has more typed params
        return false;
      }
    }

    if (this.returnType === undefined) {
      return other.returnType === undefined;
    }

    return this.returnType.assignableFrom(other.returnType);
  };

  Node.prototype.transform = T.makePass("transform", "transformNode");

  function compileList(list, o) {
    var translist = [];
    var trans;
    for (var i = 0, j = list.length; i < j; i++) {
      trans = list[i].transform(o);
      if (trans !== null) {
        translist.push(trans ? trans : list[i]);
      }
    }
    return translist;
  }

  TypeAliasDirective.prototype.transform = function (o) {
    var functions = [];
    if (this.original instanceof T.StructType) {
      var members = this.original.members;
      for (var i = 0; i < members.length; i++) {
        var name = this.original.id.name;
        var member = members[i];
        if (member.declarator instanceof FunctionDeclaration) {
          if (members[i].modifiers.indexOf("static") >= 0) {
            name += "_Static";
          }
          var functionDeclaration = member.declarator;
          var fnName = functionDeclaration.id.name;

          functionDeclaration.params.unshift(
              new Identifier('thisPtr')
          );
          functionDeclaration.id.name = name + "$" + fnName;
          functions.push(functionDeclaration.transform(o));
        }
      }
    }
    if (functions.length) {
      return new BlockStatement(functions, true);
    }
    return null;
  };

  Program.prototype.transform = function (o) {
    o = extend(o);
    o.scope = this.frame;

    // Move all global variable declarations to the top
    var decls = [];
    var body = [];
    for(var i=0; i<this.body.length; i++) {
      var node = this.body[i];

      if(node instanceof VariableDeclaration) {
        if(node.kind == 'extern') {
          continue;
        }

        for(var j=0; j<node.declarations.length; j++) {
          var decl = node.declarations[j];
          var variable = o.scope.getVariable(decl.id.name);

          if(!variable.isStackAllocated) {
            logger.push(decl);
            check(decl.init, ('Global variable ' + quote(decl.id) +
                              ' must have an initializer'));
            check(decl.init instanceof Literal,
                  'Global variable ' + quote(decl.id) +
                  ' must be a constant literal');
            logger.pop();
          }

          variable.global = true;
          decl.global = true;
        }

        node.global = true;
        decls.push(node);
      }
      else {
        body.push(node);
      }
    }

    this.body = compileList(decls.concat(body), o);
    this.frame.close();

    if(o.asmjs) {
      var globalSP = new VariableDeclaration(
        'var',
        [new VariableDeclarator(new Identifier('globalSP'),
                                new Literal(this.frame.frameSize),
                                Types.i32ty)]
      );

      this.body.unshift(globalSP);
    }
    return this;
  };

  FunctionExpression.prototype.transform =
  FunctionDeclaration.prototype.transform = function (o) {
    o = extend(o);
    o.scope = this.frame;

    assert(this.body instanceof BlockStatement);
    this.body.body = compileList(this.body.body, o);
    this.frame.close();

    return cast(this, this.decltype.reflect(o));
  };

  IfStatement.prototype.transformNode = function(o) {
    if(o.asmjs &&
       this.test instanceof LogicalExpression &&
       (this.test.operator == '&&' || this.test.operator == '||')) {
      var test = this.test;

      var tmp = o.scope.freshTemp(Types.i32ty, test.loc);
      var code = [new ExpressionStatement(
          new AssignmentExpression(tmp, '=', literal(0))
      )];

      var assignTrue = new BlockStatement([
        new ExpressionStatement(new AssignmentExpression(tmp, '=', literal(1)))
      ]);

      if(test.operator == '&&') {
        var block = new BlockStatement([new IfStatement(
          test.right,
          assignTrue
        )]);
        block.scope = o.scope;

        code.push(new IfStatement(test.left, block).transform(o));
      }
      else if(test.operator == '||') {
        code.push(new BlockStatement([
          new IfStatement(test.left, assignTrue).transform(o),
          new IfStatement(test.right, assignTrue).transform(o)
        ]));
      }

      code.push(new BlockStatement([new IfStatement(
        tmp,
        this.consequent,
        this.alternate
      )]));

      return new BlockStatement(code);
    }
  };

  ForStatement.prototype.transform = function (o) {
    o = extend(o);
    o.scope = this.scope;
    return Node.prototype.transform.call(this, o);
  };

  ForStatement.prototype.transformNode = function(o) {
    if(this.init instanceof ExpressionStatement) {
      this.init = this.init.expression.expressions[0];
    }
  };

  BlockStatement.prototype.transform = function (o) {
    o = extend(o);
    o.scope = this.scope;
    this.body = compileList(this.body, o);
    return this;
  };

  CatchClause.prototype.transform = function (o) {
    o = extend(o);
    this.body.transform(o);
    return this;
  };

  CastExpression.prototype.transform = function (o) {
    if (this.as && !(this.ty = this.as.reflect(o))) {
      return this.argument.transform(o);
    }

    o = extend(o);
    o.wantsToBe = this.ty;
    this.argument = this.argument.transform(o);

    return this;
  };

  Literal.prototype.transformNode = function (o) {
    if (this.value === null) {
      return cast(this, Types.nullTy);
    }

    if (typeof this.value === "number") {
      return cast(this, isInteger(this.value) ? Types.i32ty : Types.f64ty);
    }
  };

  ThisExpression.prototype.transformNode = function (o) {
    var scope = o.scope;
    var variable = scope.getVariable("this");
    if (variable) {
      // `this` is actually just a pointer variable named something
      // else that is passed as the first argument
      return cast(new Identifier("thisPtr"), variable.type);
    }
  };

  Identifier.prototype.transformNode = function (o) {
    if (this.kind === "variable" && !this.variable) {
      var scope = o.scope;
      var variable = scope.getVariable(this.name);

      if(o.asmjs) {
        check(variable, "unknown identifier " + quote(this.name) + " in scope " + scope);
      }
      // if (!(variable.type instanceof StructStaticType)) {
      //   check(variable.isStackAllocated ? variable.frame === scope.frame : true,
      //         "cannot close over stack-allocated variables");
      // }

      if(variable) {
        this.name = variable.name;
        this.variable = variable;

        return cast(this, variable.type);
      }
    }
  };

  VariableDeclaration.prototype.transform = function (o) {
    if (this.kind === "extern") {
      return null;
    }
    else if(!this.declarations[0].decltype) {
        return;
    }

    var decl, decls = this.declarations;
    var transformed = [];

    for(var i=0; i<decls.length; i++) {
      if((decl = decls[i].transform(o))) {
        transformed.push(decl);
      }
    }

    this.declarations = transformed;
    return this.transformNode(o);
  };

  VariableDeclaration.prototype.transformNode = function (o) {
    if(!this.global || this.kind == 'extern') {
      // We shouldn't have any variable declarations here, they are
      // created in the prologue (asm.js requirement). Convert these to
      // AssignmentExpression-s if they have an initializer.
      if(this.declarations.length) {
        return new ExpressionStatement(new SequenceExpression(this.declarations));
      }

      return null;
    }
  };

  VariableDeclarator.prototype.transformNode = function (o) {
    var variable = this.id.variable;
    var type = variable.type;

    if (!type) {
        if(this.init) {
            return new AssignmentExpression(this.id, "=", this.init, this.init.loc).transform(o);
        }
        else {
            return null;
        }
    }

    if (this.arguments) {
      // Does the variable declaration call the constructor?
      var member = type.getMember(type.name);
      if (member) {
        var constructor = new Identifier(type.name + "$" + type.name);
        constructor = cast(constructor, member.type, true);
        var obj = new UnaryExpression("&", this.id, this.loc).transform(o);
        var callConstructor = new CallExpression(constructor, [obj].concat(this.arguments), this.loc).transform(o);
        // TODO: This is kind of retarded.
        return new SequenceExpression([callConstructor, this.id]);
      }
    }
    else if (this.init) {
      if (this.init instanceof ArrayExpression) {
        var rootPointer = cast(this.id, new PointerType(type.getRoot()), true);
        // this.id = o.scope.freshTemp(type, this.loc);
        var elementInitializers = new SequenceExpression([]);
        var generated = 0;
        function generateInitializers(type, elements) {
          check(type.length === elements.length, "Incompatible array initializer.");
          for (var i = 0; i < elements.length; i++) {
            var element = elements[i];
            if (element instanceof ArrayExpression) {
              generateInitializers(type.base, element.elements);
            } else {
              elementInitializers.expressions.push (
                new AssignmentExpression (
                  new UnaryExpression("*",
                    new BinaryExpression("+",
                      rootPointer,
                      literal(generated++)
                    )
                  ),
                  "=", element, element.loc
                )
              );
            }
          }
        }
        generateInitializers(type, this.init.elements);
        return elementInitializers.transform(o);
      }
      else {
        var left = this.id;
        var right = this.init;

        if(right instanceof Literal &&
           (type.name == 'f32' || type.name == 'f64')) {
          right.forceDouble = true;
        }

        var assn = (new AssignmentExpression(left, "=", right, this.init.loc)).transform(o);

        if(this.global) {
          this.id = assn.left;
          this.init = assn.right;
        }
        else {
          return assn;
        }
      }
    }
    else if (variable.isStackAllocated) {
      //return o.scope.freshTemp(type, this.loc);
      return null;
    }
    else {
      return null;
    }
  };

  ReturnStatement.prototype.transformNode = function (o) {
    var frame = o.scope.frame;
    var returnType = frame.returnType;
    var arg = this.argument;
    var ty = arg ? arg.ty : undefined;
    if (returnType) {
      check(returnType.assignableFrom(ty), "incompatible types: returning " +
            quote(Types.tystr(ty, 0)) + " as " + quote(Types.tystr(returnType, 0)));
      if (arg) {
        this.argument = cast(arg, returnType, false, true);
      }
    }
  };

  const BINOP_ARITHMETIC = ["+", "-", "*", "/", "%"];
  const BINOP_BITWISE    = ["<<", ">>", ">>>", "~", "&", "|"];
  const BINOP_COMPARISON = ["==", "!=", "===", "!==", "<", ">", "<=", ">="];

  ConditionalExpression.prototype.transformNode = function (o) {
    var ty;
    var lty = this.consequent.ty;
    var rty = this.alternate.ty;

    if (typeof lty === "undefined" || typeof rty === "undefined") {
      return this;
    }

    if (lty.assignableFrom(rty)) {
      ty = lty;
    } else if (rty.assignableFrom(lty)) {
      ty = rty;
    }

    return cast(this, ty);
  };

  BinaryExpression.prototype.transformNode = function (o) {
    var ty;
    var lty = this.left.ty;
    var rty = this.right.ty;
    var op = this.operator;

    if (lty instanceof PointerType && (op === "+" || op === "-")) {
      if (rty instanceof PrimitiveType && rty.integral) {
        ty = lty;
      } else if (rty instanceof PointerType && op === "-") {
        check(lty.base.size === rty.base.size,
              "subtraction with incompatible pointer types " +
              quote(lty) + " and " + quote(rty));
        ty = Types.i32ty;
      }
    } else if (BINOP_COMPARISON.indexOf(op) >= 0) {
      if (lty instanceof PointerType && isNull(this.right)) {
        this.right = cast(this.right, lty);
      } else if (rty instanceof PointerType && isNull(this.left)) {
        this.left = cast(this.left, rty);
      }
      else {
        this.left = cast(this.left, lty, true, true);
        this.right = cast(this.right, rty, true, true);
      }

      ty = Types.i32ty;
    } else if (BINOP_BITWISE.indexOf(op) >= 0) {
      ty = Types.i32ty;
    } else if (BINOP_ARITHMETIC.indexOf(op) >= 0 &&
               (lty instanceof PrimitiveType && lty.numeric) &&
               (rty instanceof PrimitiveType && rty.numeric)) {
      // Arithmetic on ints now begets ints, unless it wants to be a wider
      // primitive from an outside cast.
      var wantsToBe;
      if (lty.integral && rty.integral &&
          !(((wantsToBe = o.wantsToBe) instanceof PrimitiveType) &&
            wantsToBe.size > Types.i32ty.size)) {
        // Force a CastExpression here so we convert it during lowering
        // without warnings.

        if(op == '*') {
          // Multiplication of integers is special in asm.js, and
          // requires a call to `imul` instead. See
          // http://asmjs.org/spec/latest/#intish
          return cast(new CallExpression(new Identifier('imul'), [this.left, this.right]),
                      Types.i32ty,
                      true);
        }

        this.left = cast(this.left, lty, true, true);
        this.right = cast(this.right, rty, true, true);
        return cast(this, Types.i32ty, true);
      }

      // Force the operands to be casted (asm.js is strict about this)
      this.left = cast(this.left, Types.f64ty, true);
      this.right = cast(this.right, Types.f64ty, true);
      ty = Types.f64ty;
    }

    if (ty) {
      return cast(this, ty);
    }
  };

  UnaryExpression.prototype.transform = function (o) {
    var ty;
    var op = this.operator;

    if (op === "sizeof") {
      ty = this.argument.reflect(o);
      return cast(literal(ty.size, this.argument.loc), Types.i32ty);
    }

    var arg = this.argument = this.argument.transform(o);
    ty = arg.ty;

    if (op === "delete" && ty) {
      check(ty instanceof PointerType, "cannot free non-pointer type");
      return (new CallExpression(o.scope.FREE(),
                                 [cast(this.argument, Types.bytePointerTy)], this.loc)).transform(o);
    }

    if (op === "*") {
      check(ty instanceof PointerType, "cannot dereference non-pointer type " + quote(Types.tystr(ty, 0)));
      return cast(this, ty.base);
    }

    if (op === "&") {
      check(ty, "cannot take address of untyped expression");
      if (arg.variable) {
        arg.variable.isStackAllocated = true;
      }
      return cast(this, new PointerType(ty), true);
    }

    if (op === "!" || op === "~") {
      return cast(this, Types.i32ty);
    }

    if (op === "-") {
      if (arg.ty && arg.ty.numeric) {
        return cast(this, arg.ty);
      }
      return cast(this, Types.f64ty);
    }

    return this;
  };

  NewExpression.prototype.transform = function (o) {
    var ty;
    if (this.callee instanceof Identifier && (ty = o.types[this.callee.name])) {
      var pty = new PointerType(ty)
      var allocation = new CallExpression(o.scope.MALLOC(),
                                          [cast(literal(ty.size), Types.i32ty)], this.loc);
      allocation = cast(allocation.transform(o), pty);
      // Check if we have a constructor ArrowType.
      if (ty instanceof StructType) {
        var member = ty.getMember(ty.name);
        if (member) {
          assert (member.type instanceof ArrowType);
          logger.push(this);
          var tmp = o.scope.freshTemp(pty, this.loc);
          var assignment = new AssignmentExpression(tmp, "=", allocation, this.loc);
          var constructor = new MemberExpression(new Identifier(ty.name + "$" + ty.name), new Identifier("call"), false);
          constructor = cast(constructor, member.type, true);
          var callConstructor = new CallExpression(constructor, [assignment].concat(this.arguments), this.loc).transform(o);
          allocation = new SequenceExpression([callConstructor, tmp], this.loc);
          logger.pop();
          return cast(allocation, pty, true);
        }
      }
      return allocation;
    } else if (this.callee instanceof MemberExpression &&
               this.callee.computed &&
               (ty = o.types[this.callee.object.name])) {
      var size = new BinaryExpression("*", literal(ty.size), this.callee.property, this.loc);
      var allocation = new CallExpression(o.scope.MALLOC(),
                                          [cast(size, Types.i32ty)], this.loc);
      return cast(allocation.transform(o), new PointerType(ty));
    }
    return Node.prototype.transform.call(this, o);
  };

  SequenceExpression.prototype.transformNode = function (o) {
    assert(this.expressions.length);
    var last = this.expressions[this.expressions.length - 1];
    return cast(this, last.ty);
  };

  UpdateExpression.prototype.transformNode = function (o) {
    var arg = this.argument;
    var ty = arg.ty
    if (ty && (ty.integral || ty instanceof PointerType)) {
      var scope = o.scope;
      var op = this.operator === "++" ? "+" : "-";
      var ref = scope.cacheReference(arg);
      var right = new BinaryExpression(op, ref.use, literal(1), this.loc);
      if (this.prefix) {
        return (new AssignmentExpression(ref.def, "=", right, this.loc)).transform(o);
      }
      var t = scope.freshTemp(ty, arg.loc);
      var assn = new AssignmentExpression(t, "=", ref.def, this.loc);
      var incdec = (new AssignmentExpression(ref.use, "=", right, this.loc)).transform(o);
      return cast(new SequenceExpression([assn, incdec, t], this.loc), ty);
    }
  };

  AssignmentExpression.prototype.transformNode = function (o) {
    var lty = this.left.ty;
    var rty = this.right.ty;
    this.left.lvalue = true;

    if (!lty) {
      return;
    }

    var scope = o.scope;
    var op = this.operator;

    if (op !== "=") {
      var binop = op.substr(0, op.indexOf("="));
      var ref = scope.cacheReference(this.left);
      // This forceType is needed if it references the heap. need to
      // figure out why exactly.
      var right = new BinaryExpression(binop, forceType(ref.use), this.right, this.right.loc);
      return (new AssignmentExpression(ref.def, "=", right, this.loc)).transform(o);
    }

    check(lty.assignableFrom(rty), "incompatible types: assigning " +
          quote(Types.tystr(rty, 0)) + " to " + quote(Types.tystr(lty, 0)));

    if (lty instanceof StructType) {
      var mc = scope.MEMCPY();
      var size = lty.size;
      var memcpyTy = mc.ty.paramTypes[0];
      var left = cast(new UnaryExpression("&", this.left, this.left.loc), memcpyTy, true);
      var right = cast(new UnaryExpression("&", this.right, this.right.loc), memcpyTy, true);
      return cast(new CallExpression(mc, [left, right, literal(size)]), lty).transform(o);
    } else {
      this.right = cast(this.right, lty);
      return cast(this, lty);
    }
  };

  function MemberFunctionCall (object, member) {
    assert (object.ty instanceof PointerType);
    this.object = object;
    this.member = member;
  }

  MemberExpression.prototype.transformNode = function (o) {
    var obj = this.object;
    var prop = this.property;
    var oty = obj.ty;

    if (!oty) {
      return;
    }

    if (this.computed) {
      check(oty instanceof PointerType, "cannot use [] operator on non-pointer type.");
      return new UnaryExpression("*", new BinaryExpression("+", obj, prop)).transform(o);
    }

    if (this.kind === "->") {
      check(oty instanceof PointerType && (oty.base instanceof StructType),
            "base of struct dereference must be struct or union type.");
      oty = oty.base;
    } else {
      check(!(oty instanceof PointerType), "cannot use . operator on pointer type.");
      if (!(oty instanceof StructType || oty instanceof StructStaticType)) {
        return;
      }
    }

    check(prop instanceof Identifier, "invalid property name.");
    var member = oty.getMember(prop.name);
    check(member, "Unknown member " + quote(prop.name) + " of type " + quote(Types.tystr(oty, 0)));

    // Expressions of the form |o->f(x, y)| need to be translated into |f(o, x, y)|. Here we
    // see the |o->f| part so we mark it as a |MemberFunctionCall| and take care of it when we
    // transform the CallExpression.
    if (member.type instanceof ArrowType) {
      // Normalize the form |o.f| into |&o->f| to simplify things.
      if (!(obj.ty instanceof PointerType)) {
        obj = new UnaryExpression("&", obj, this.loc).transform(o);
      }
      return new MemberFunctionCall(obj, member);
    }

    this.structField = member;
    return cast(this, member.type);
  };

  CallExpression.prototype.transformNode = function (o) {
    if (this.callee instanceof MemberFunctionCall) {
      var obj = this.callee.object;
      var member = this.callee.member;
      var name = obj.ty.base.name + "$" + member.name;
      var fn = cast(new Identifier(name), member.type, true);
      return new CallExpression (
        fn, [this.callee.object].concat(this.arguments)
      ).transform(o);
    }

    var fty = this.callee.ty;
    var args = this.arguments;

    if (!fty) {
      if(this.callee instanceof Identifier &&
         o.types[this.callee.name]) {
        fty = o.types[this.callee.name];
      }
      else {
        for(var i=0; i<args.length; i++) {
          var unary = args[i] instanceof UnaryExpression;

          if(o.asmjs) {
            // TODO: clean up this hack. if we are calling a function and
            // don't know its types (extern), force the types only on
            // certain expressions to make asm.js happy
            if(args[i] instanceof Identifier ||
               (unary && args[i].operator == '*' ||
                unary && args[i].operator == '&')) {
              args[i] = forceType(args[i]);
            }
          }
        }
        return;
      }
    }

    check(fty instanceof ArrowType, "trying to call non-function type");

    var paramTys = fty.paramTypes;

    check(paramTys.length === args.length,
          "Argument/parameter count mismatch, expected: " + paramTys.length +
          ", received: " + args.length, true);

    for (var i = 0, j = paramTys.length; i < j; i++) {
      var arg = args[i];
      var pty = paramTys[i];
      var aty = arg ? arg.ty : undefined;
      if (pty) {
        if (arg) {
          logger.push(arg);
        }
        check(pty.assignableFrom(aty), "incompatible types: passing " +
              quote(Types.tystr(aty, 0)) + " to " + quote(Types.tystr(pty, 0)));
        logger.pop();
        args[i] = cast(arg, pty);
      }
    }

    return forceType(this, fty.returnType);
  };

  ImportExpression.prototype.transformNode = function(o) {
    var from = this.from.value;
    var decls = [];

    for(var i=0; i<this.imports.length; i++) {
      var name = this.imports[i].name;

      if(o.types[name] instanceof Types.ArrowType) {
        decls.push(new VariableDeclarator(
          new Identifier(name),
          new MemberExpression(new Identifier(from),
                               new Identifier(name))));
      }
    }

    return decls.length ? new VariableDeclaration('var', decls) : null;
  };

  /**
   * Pass 4: lowering
   */

  PrimitiveType.prototype.convert = function (expr, force, warn) {
    assert(expr);

    var rty = expr.ty;

    // TODO: this needs to be cleaned up. currently we produce too
    // many annotaions (like `x | 0 | 0`) and it's generally unclear
    // when the annotations are generated. clean the following code up
    // and resolve any differences with `forceType` in util.js

    // asm.js requires every expression to be casted
    // if (this === rty) {
    //   return expr;
    // }

    if (!force) {
      check(!(rty instanceof PointerType), "conversion from pointer to " +
            quote(Types.tystr(rty, 0)) + " without cast", true);
    }

    if (!this.numeric) {
      return expr;
    }

    if (!this.integral) {
      // If converting from an integer, force it to signed/unsigned
      // (required by asm.js). This breaks literals however, since
      // right now 6.0 is parsed as an integer, so guard against that.
      if(rty && rty.integral && !(expr instanceof Literal)) {
        expr = forceType(expr);
      }

      return new UnaryExpression("+", expr);

      // if (rty && rty.numeric) {
      //     return expr;
      // }
      // return new CallExpression(new Identifier("Number"), [expr], expr.loc);
    }

    var conversion;
    var lwidth = this.size << 3;
    var rwidth = rty ? rty.size << 3 : 8;
    var lsigned = this.signed;
    var rsigned = rty ? rty.signed : undefined;
    var mask = (1 << lwidth) - 1;
    var shift = 32 - lwidth;
    var errorPrefix;

    // If we're converting a constant, check if it fits.
    if (expr instanceof Literal ||
        (expr instanceof UnaryExpression && expr.argument instanceof Literal)) {
      var val, val2;
      if (expr instanceof Literal) {
        val = expr.value;
      } else {
        switch (expr.operator) {
        case "-":
          val = -expr.argument.value;
          break;
        case "~":
          val = ~expr.argument.value;
          break;
        case "!":
          val = Number(!expr.argument.value);
          break;
        default:
          console.error("oops");
        }
      }

      if (lwidth !== 32 && lwidth < rwidth) {
        val2 = val & mask;
        if (lsigned) {
          val2 = (val2 << shift) >> shift;
        }
      } else if (lwidth !== rwidth || lsigned != rsigned) {
        if (lsigned) {
          val2 = val | 0;
        } else {
          val2 = val >>> 0;
        }
      } else {
        val2 = val;
      }

      if (val === val2) {
        return expr;
      }

      errorPrefix = "constant conversion to " + quote(Types.tystr(this, 0)) + " alters its ";
    } else {
      errorPrefix = "conversion from " + quote(Types.tystr(rty, 0)) + " to " + quote(Types.tystr(this, 0)) + " may alter its ";
    }

    // Allow conversions from dyn without warning.
    if (!force && warn.conversion) {
      check(lwidth === rwidth, errorPrefix + "value", true);
      check(lsigned === rsigned, errorPrefix + "sign", true);
    }


    // Do we need to truncate? Bitwise operators automatically truncate to 32
    // bits in JavaScript so if the width is 32, we don't need to do manual
    // truncation.
    var loc = expr.loc;
    if (lwidth !== 32 && lwidth < rwidth) {
      conversion = new BinaryExpression("&", expr, literal(mask), loc);
      // Do we need to sign extend?
      if (lsigned) {
        conversion = new BinaryExpression("<<", conversion, literal(shift), loc);
        conversion = new BinaryExpression(">>", conversion, literal(shift), loc);
      }
    } else {
      if(rty && !rty.integral) {
        conversion = new UnaryExpression('~~', expr);
      }
      else if(lsigned) {
        conversion = forceType(expr, Types.i32ty);
      }
      else {
        conversion = new BinaryExpression(">>>", expr,
                                          literal(0), loc);
      }
    }

    return conversion;
  };

  PointerType.prototype.convert = function (expr, force, warn) {
    // This is important for TI. Returning null here would result in the site
    // being dimorphic.
    if (isNull(expr)) {
      expr.value = 0;
      return expr;
    }

    var rty = expr.ty;
    if (this === rty || !(rty instanceof PointerType)) {
      if (!force) {
        check(!(rty instanceof PrimitiveType && rty.integral), "conversion from " +
              quote(Types.tystr(rty, 0)) + " to pointer without cast", true);
      }
      return expr;
    }

    if (warn.conversion) {
      check(rty.base.align.size >= this.base.align.size, "incompatible pointer conversion from " +
            rty.base.align.size + "-byte aligned " + quote(Types.tystr(rty, 0)) + " to " +
            this.base.align.size + "-byte aligned " + quote(Types.tystr(this, 0)), true);
    }

    return forceType(expr, Types.i32ty);
  };

  StructType.prototype.convert = function (expr) {
    return expr;
  };

  ArrowType.prototype.convert = function (expr) {
    return expr;
  };

  function addMainInitializers(node, o) {
    // Add the stack and heap initializers to the main function
    return [
      new ExpressionStatement(
        new AssignmentExpression(
          new MemberExpression(o.scope.getView(Types.u32ty), literal(1), true),
          '=',
          new BinaryExpression(
            '-',
            new Identifier('totalSize'),
            new Literal(o.frame.root.frameSize)
          )
        )
      ),

      new ExpressionStatement(
        new AssignmentExpression(
          new MemberExpression(o.scope.getView(Types.u32ty), literal(0), true),
          '=',
          literal(4)
        )
      )
    ];
  }

  function createVariableDecls(node, o) {
    var decls = [];
    var frame = o.frame;

    // Any temporary variables
    var cachedLocals = frame.cachedLocals;
    for(var local in cachedLocals) {
      var v = frame.mangles[local];
      var decl = new VariableDeclarator(new Identifier(v.name),
                                        new Literal(v.type.defaultValue || 0));
      if(v.type.numeric && !v.type.integral) {
        decl.init.forceDouble = true;
      }

      decls.push(decl);
    }

    // All the declared variables. We need to declare every single
    // variable in the whole function, so we need to scan all the
    // nodes for all the scopes.
    for(v in frame.scopedVariables) {
      var variable = frame.scopedVariables[v];
      var ty = variable.type;

      // Don't process the `this` variable or any of the arguments
      if(ty &&
         (!(ty instanceof Types.ArrowType) || ty.returnType) &&
         variable.name != 'this' &&
         node.parameters.indexOf(variable) === -1) {



        var decl = new VariableDeclarator(new Identifier(variable.name),
                                          new Literal((ty && ty.defaultValue) || 0));
        if(ty && ty.numeric && !ty.integral) {
          decl.init.forceDouble = true;
        }

        decls.push(decl);
      }
    }

    if(o.asmjs) {
      decls.push(new VariableDeclarator(new Identifier('$SP'), new Literal(0)));
    }

    return decls.length && new VariableDeclaration("var", decls);
  }

  function createPrologue(node, o) {
    assert(node.frame);

    var frame = node.frame;
    var code = [];

    var local, v;
    var constants = [];
    var variables = [];

    if (node.parameters) {
      var params = node.parameters;

      if(node.params.length && node.params[0].name == 'thisPtr') {
        var assn = new AssignmentExpression(
          new Identifier('thisPtr'), '=',
          new BinaryExpression("|", new Identifier('thisPtr'), literal(0))
        );
        code.push(new ExpressionStatement(assn));
      }

      for (var i = 0, j = params.length; i < j; i++) {
        var p = params[i];
        var ty = p.type;

        if(!ty) {
          continue;
        }

        if(ty.name == 'f32' || ty.name == 'f64') {
          var assn = new AssignmentExpression(
            new Identifier(p.name), '=',
            forceType(new Identifier(p.name), Types.f64ty)
          );
          code.push(new ExpressionStatement(assn));
        }
        else {
          var assn = new AssignmentExpression(
            new Identifier(p.name), '=',
            forceType(new Identifier(p.name), Types.i32ty)
          );
          code.push(new ExpressionStatement(assn));
        }

        // I don't know what the following code is trying to do. Disable
        // passing structs by value.
        if(p.isStackAllocated) {
          throw new Error("cannot pass stack-allocated objects yet");
        }

        //if (!p.isStackAllocated) {
        //  continue;
        //}
        // var assn = new AssignmentExpression(p.getStackAccess(frame), "=", new Identifier(p.name));
        // code.push(new ExpressionStatement(assn));
      }
    }

    var decls = createVariableDecls(node, o);
    if(decls) {
      code.push(decls);
    }

    if(node.id && node.id.name == 'main') {
      code = code.concat(addMainInitializers(node, o));
    }

    var frameSize = frame.frameSize;
    if(frameSize) {
      var allocStack = new AssignmentExpression(
        frame.realSP(), "=",
        forceType(new BinaryExpression("-", forceType(frame.realSP()), literal(frameSize)),
                  Types.i32ty)
      );

      logger.push(allocStack);
      allocStack.lower(o);
      logger.pop();

      code.push(new ExpressionStatement(allocStack));

      var spDecl = new AssignmentExpression(frame.SP(), "=", forceType(frame.realSP()));
      code.push(new ExpressionStatement(spDecl));
    }

    return code;
  }

  function createEpilogue(node, o) {
    assert(node.frame);

    var frame = node.frame;
    var frameSize = frame.frameSize;
    if (frameSize) {
      var exprs = [
        new ExpressionStatement(
          new AssignmentExpression(
            frame.realSP(), "=",
            new BinaryExpression("+", forceType(frame.realSP()),
                                 literal(frameSize))))
      ];

      var retType = node.ty.returnType;

      if(retType && retType != Types.voidTy) {
        var unifyRetType = new ReturnStatement(
          new Literal((retType.integral || retType instanceof Types.PointerType) ? 0 : 0.0)
        );
        unifyRetType.argument.forceDouble = !node.ty.returnType.integral;

        exprs.push(unifyRetType);
      }
      return exprs;
    }
    return [];
  }

  function lowerList(list, o) {
    var translist = [];
    var trans;
    for (var i = 0, j = list.length; i < j; i++) {
      trans = list[i].lower(o);
      if (trans !== null) {
        translist.push(trans ? trans : list[i]);
      }
    }
    return translist;
  }


  Node.prototype.lower = T.makePass("lower", "lowerNode");

  Program.prototype.lower = function (o) {
    o = extend(o);
    o.scope = o.frame = this.frame;

    this.body = lowerList(this.body, o);
    // var prologue = createPrologue(this, o);
    // var epilogue = createEpilogue(this, o);
    //this.body = prologue.concat(this.body).concat(epilogue);

    return this;
  };

  FunctionExpression.prototype.lower =
  FunctionDeclaration.prototype.lower = function (o) {
    var memcheckName;
    o = extend(o);
    o.scope = o.frame = this.frame;


    if (o.memcheck) {
      if (this.id && this.id.name) {
        memcheckName = this.id.name;
      } else {
        memcheckName = "<anonymous>";
      }
      this.frame.memcheckFnLoc = {name: memcheckName, line: this.loc.start.line, column: this.loc.start.column};
      // this.body.body.unshift(new ExpressionStatement(new CallExpression(this.frame.MEMCHECK_CALL_PUSH(),
      //                                                                   [literal(memcheckName),
      //                                                                    literal(o.name),
      //                                                                    literal(this.loc.start.line),
      //                                                                    literal(this.loc.start.column)])));

      //if (this.body.body[this.body.body.length-1].type !== 'ReturnStatement') {
        //this.body.body.push(new ExpressionStatement(new CallExpression(this.frame.MEMCHECK_CALL_POP(), [])));
      //}
    }
    this.body.body = lowerList(this.body.body, o);

    var prologue = createPrologue(this, o);
    var epilogue = createEpilogue(this, o);
    this.body.body = prologue.concat(this.body.body).concat(epilogue);

    return this;
  };

  ForStatement.prototype.lower = function (o) {
    o = extend(o);
    o.scope = this.scope;
    return Node.prototype.lower.call(this, o);
  };

  BlockStatement.prototype.lower = function (o) {
    o = extend(o);
    o.scope = this.scope;
    this.body = lowerList(this.body, o);
    return this;
  };

  function findParentFun(scope) {
    var name;
    while(scope.parent) {
      if(scope.name.indexOf("Function") === 0) {
        name = scope.name.split(" ")[1];
      }
    }
  }

  CatchClause.prototype.lower = function(o) {
    o = extend(o);
    // if(o.memcheck) {
    //   var fnLoc = o.scope.frame.memcheckFnLoc;
    //   this.body.body.unshift(new ExpressionStatement(new CallExpression(o.scope.frame.MEMCHECK_CALL_RESET(),
    //                                                                     [literal(fnLoc.name),
    //                                                                      literal(fnLoc.line),
    //                                                                      literal(fnLoc.column)])));
    // }
    return Node.prototype.lower.call(this, o);
  };


  Identifier.prototype.lowerNode = function (o) {
    var variable = this.variable;
    if (variable && variable.isStackAllocated) {
      return variable.getStackAccess(o.frame, this.loc);
    }
  };

  VariableDeclaration.prototype.lowerNode = function (o) {
    if (this.declarations.length === 0) {
      return null;
    }
  };

  VariableDeclarator.prototype.lowerNode = function (o) {
    if (this.id.ty && this.id.ty.arraySize) {
      return null;
    }

    if (!(this.id instanceof Identifier)) {
      if (this.init) {
        this.init = new AssignmentExpression(this.id, "=", this.init, this.init.loc);
        this.id = o.scope.freshTemp(undefined, this.id.loc, true);
      } else {
        return null;
      }
    }
  };

  ReturnStatement.prototype.lowerNode = function (o) {
    var scope = o.scope;
    var frameSize = scope.frame.frameSize;
    if (frameSize || o.memcheck) {
      var arg = this.argument;
      var t = scope.freshTemp(arg.ty, arg.loc);
      var assn = new AssignmentExpression(t, "=", arg, arg.loc);
      var exprList = [assn];
      if(o.asmjs && frameSize) {
        var restoreStack = new AssignmentExpression(
            scope.frame.realSP(), "=",
            forceType(
                new BinaryExpression(
                    "+", forceType(scope.frame.realSP()), literal(frameSize)
                ),
                Types.i32ty
            )
        );
          exprList.push(restoreStack);
      }
      if(o.memcheck) {
        var popMemcheck = new CallExpression(scope.MEMCHECK_CALL_POP(), []);
        exprList.push(popMemcheck);
      }
      exprList.push(t);

      // asm.js won't let you return unsigned, so we force
      // a signed type (pass `true` as 3rd arg)
      this.argument = forceType(
        new SequenceExpression(exprList, arg.loc),
        arg.ty,
        true
      );
    }
  };

  BinaryExpression.prototype.lowerNode = function (o) {
    var ty = this.ty;
    var op = this.operator;

    if (ty instanceof PointerType && (op === "+" || op === "-")) {
      var lty = this.left.ty;
      var rty = this.right.ty;
      if (rty instanceof PrimitiveType && rty.integral) {
        var scale = lty.base.size;
        if (scale > 1) {
          this.right = forceType(cast(new BinaryExpression("*", this.right, literal(scale), this.right.loc),
                                      Types.i32ty));
        }
      }
    }
  };

  UnaryExpression.prototype.lowerNode = function (o) {
    var arg = this.argument;

    if (this.operator === "*") {
      return dereference(arg, 0, this.ty, o.scope, this.loc);
    }

    if (this.operator === "&") {
      // We already have an aligned lookup, just grab the pointer
      // out of it though (see `alignAddress` in util.js)
      if(arg instanceof BinaryExpression) {
        arg = arg.left;
      }

      return arg.property.left;
    }
  };

  MemberExpression.prototype.lowerNode = function (o) {
    var field = this.structField;
    if(!field) {
      return;
    }

    var address, view;
    if (this.kind === "->") {
      address = this.object;
    } else {
      // The identifer has already been aligned, so we just need to
      // pick out the unaligned address
      var obj = this.object;
      if(obj instanceof BinaryExpression) {
        obj = obj.left;
      }

      assert(obj instanceof MemberExpression);
      assert(obj.property instanceof BinaryExpression);
      address = obj.property.left;
    }

    var def = dereference(address, field.offset, field.type, o.scope, this.loc);

    if(!this.lvalue) {
      return forceType(def);
    }
    return def;
  };

  CastExpression.prototype.lowerNode = function (o) {
    // Treat user casts as forced casts so we don't emit warnings.
    var lowered = this.ty.convert(this.argument, (!!this.as || this.force), o.warn);
    // Remember (coerce) the type for nested conversions.
    lowered.ty = this.ty;
    return lowered;
  };

  function extractExterns(node) {
    var externs = [];

    if(node instanceof Program) {
      for(var i=0; i<node.body.length; i++) {
        var expr = node.body[i];

        if(expr instanceof VariableDeclaration && expr.kind == 'extern') {
          for(var j=0; j<expr.declarations.length; j++) {
            externs.push(expr.declarations[j].id.name);
          }
        }
      }
    }

    return externs;
  }

  function warningOptions(options) {
    var warn = {};
    for (var p in options) {
      if (p.charAt(0) === "W") {
        warn[p.substr(1)] = true;
      }
    }
    return warn;
  }

  var logger;

  function getTypes(names, node, _types, _logger) {
    logger = _logger;

    node = T.lift(node);
    var types = _types || Types.builtinTypes;
    var imported = resolveAndLintTypes(node, clone(types));

    for(var i=0; i<names.length; i++) {
      var name = names[i];
      if(imported[name]) {
        types[name] = imported[name];
      }
    }

    node.scan({ types: types });

    for(var i=0, l=node.body.length; i<l; i++) {
      var expr = node.body[i];

      if(expr instanceof FunctionDeclaration && names.indexOf(expr.id.name) !== -1) {
        types[expr.id.name] = expr.decltype.reflect({ types: types, warn: true });
      }
    }

    for(var i=0; i<names.length; i++) {
      if(!types[names[i]]) {
        throw new Error('imported type not found: ' + names[i]);
      }
    }

    return types;
  }

  function compile(node, name, _logger, options, types) {
    // The logger is closed over by all the functions.
    logger = _logger;

    // Lift into constructors.
    node = T.lift(node);

    // Extract global externs
    var externs = extractExterns(node);

    // Pass 1.
    logger.info("Pass 1");
    types = resolveAndLintTypes(node, types || clone(Types.builtinTypes));
    var o = { types: types, name: name, logger: _logger, memcheck: false,
              asmjs: options.asmjs, warn: warningOptions(options) };

    // Pass 2.
    logger.info("Pass 2");
    node.scan(o);

    // Pass 3.
    logger.info("Pass 3");
    node = node.transform(o);

    // Pass 4.
    logger.info("Pass 4");
    node = node.lower(o);

    return {
      externs: externs,
      node: T.flatten(node)
    };
  }

  exports.getTypes = getTypes;
  exports.compile = compile;

})(typeof exports === 'undefined' ? (compiler = {}) : exports);
/*
 Copyright (C) 2012 Ariya Hidayat <ariya.hidayat@gmail.com>
 Copyright (C) 2012 Mathias Bynens <mathias@qiwi.be>
 Copyright (C) 2012 Joost-Wim Boekesteijn <joost-wim@boekesteijn.nl>
 Copyright (C) 2012 Kris Kowal <kris.kowal@cixar.com>
 Copyright (C) 2012 Yusuke Suzuki <utatane.tea@gmail.com>
 Copyright (C) 2012 Arpad Borsos <arpad.borsos@googlemail.com>
 Copyright (C) 2011 Ariya Hidayat <ariya.hidayat@gmail.com>

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 notice, this list of conditions and the following disclaimer in the
 documentation and/or other materials provided with the distribution.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
 DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/*jslint bitwise:true plusplus:true */
/*global esprima:true, exports:true,
 throwError: true, createLiteral: true, generateStatement: true,
 parseAssignmentExpression: true, parseBlock: true, parseExpression: true,
 parseFunctionDeclaration: true, parseFunctionExpression: true,
 parseFunctionSourceElements: true, parseVariableIdentifier: true,
 parseLeftHandSideExpression: true,
 parseStatement: true, parseSourceElement: true */

(function (exports) {
  'use strict';

  var Token,
      TokenName,
      Syntax,
      PropertyKind,
      Messages,
      Regex,
      Types,
      maybeCasts,
      source,
      strict,
      index,
      lineNumber,
      lineStart,
      length,
      buffer,
      state,
      extra;

  Token = {
    BooleanLiteral: 1,
    EOF: 2,
    Identifier: 3,
    Keyword: 4,
    NullLiteral: 5,
    NumericLiteral: 6,
    Punctuator: 7,
    StringLiteral: 8
  };

  TokenName = {};
  TokenName[Token.BooleanLiteral] = 'Boolean';
  TokenName[Token.EOF] = '<end>';
  TokenName[Token.Identifier] = 'Identifier';
  TokenName[Token.Keyword] = 'Keyword';
  TokenName[Token.NullLiteral] = 'Null';
  TokenName[Token.NumericLiteral] = 'Numeric';
  TokenName[Token.Punctuator] = 'Punctuator';
  TokenName[Token.StringLiteral] = 'String';

  Syntax = {
    AssignmentExpression: 'AssignmentExpression',
    ArrayExpression: 'ArrayExpression',
    BlockStatement: 'BlockStatement',
    BinaryExpression: 'BinaryExpression',
    BreakStatement: 'BreakStatement',
    CallExpression: 'CallExpression',
    CatchClause: 'CatchClause',
    ConditionalExpression: 'ConditionalExpression',
    ContinueStatement: 'ContinueStatement',
    DoWhileStatement: 'DoWhileStatement',
    DebuggerStatement: 'DebuggerStatement',
    EmptyStatement: 'EmptyStatement',
    ExpressionStatement: 'ExpressionStatement',
    ForStatement: 'ForStatement',
    ForInStatement: 'ForInStatement',
    FunctionDeclaration: 'FunctionDeclaration',
    FunctionExpression: 'FunctionExpression',
    Identifier: 'Identifier',
    IfStatement: 'IfStatement',
    Literal: 'Literal',
    LabeledStatement: 'LabeledStatement',
    LogicalExpression: 'LogicalExpression',
    MemberExpression: 'MemberExpression',
    NewExpression: 'NewExpression',
    ObjectExpression: 'ObjectExpression',
    Program: 'Program',
    Property: 'Property',
    ReturnStatement: 'ReturnStatement',
    SequenceExpression: 'SequenceExpression',
    SwitchStatement: 'SwitchStatement',
    SwitchCase: 'SwitchCase',
    ThisExpression: 'ThisExpression',
    ThrowStatement: 'ThrowStatement',
    TryStatement: 'TryStatement',
    UnaryExpression: 'UnaryExpression',
    UpdateExpression: 'UpdateExpression',
    VariableDeclaration: 'VariableDeclaration',
    VariableDeclarator: 'VariableDeclarator',
    WhileStatement: 'WhileStatement',
    WithStatement: 'WithStatement',

    PointerType: 'PointerType',
    ArrayType: 'ArrayType',
    StructType: 'StructType',
    ArrowType: 'ArrowType',
    TypeIdentifier: 'TypeIdentifier',
    MemberDeclarator: 'MemberDeclarator',
    TypeAliasDirective: 'TypeAliasDirective',
    CastExpression: 'CastExpression',

    ImportExpression: 'ImportExpression'
  };

  PropertyKind = {
    Data: 1,
    Get: 2,
    Set: 4
  };

  // Error messages should be identical to V8.
  Messages = {
    UnexpectedToken:  'Unexpected token %0',
    UnexpectedNumber:  'Unexpected number',
    UnexpectedString:  'Unexpected string',
    UnexpectedIdentifier:  'Unexpected identifier',
    UnexpectedReserved:  'Unexpected reserved word',
    UnexpectedEOS:  'Unexpected end of input',
    NewlineAfterThrow:  'Illegal newline after throw',
    InvalidRegExp: 'Invalid regular expression',
    UnterminatedRegExp:  'Invalid regular expression: missing /',
    InvalidLHSInAssignment:  'Invalid left-hand side in assignment',
    InvalidLHSInForIn:  'Invalid left-hand side in for-in',
    NoCatchOrFinally:  'Missing catch or finally after try',
    UnknownLabel: 'Undefined label \'%0\'',
    Redeclaration: '%0 \'%1\' has already been declared',
    IllegalContinue: 'Illegal continue statement',
    IllegalBreak: 'Illegal break statement',
    IllegalReturn: 'Illegal return statement',
    StrictModeWith:  'Strict mode code may not include a with statement',
    StrictCatchVariable:  'Catch variable may not be eval or arguments in strict mode',
    StrictVarName:  'Variable name may not be eval or arguments in strict mode',
    StrictParamName:  'Parameter name eval or arguments is not allowed in strict mode',
    StrictParamDupe: 'Strict mode function may not have duplicate parameter names',
    StrictFunctionName:  'Function name may not be eval or arguments in strict mode',
    StrictOctalLiteral:  'Octal literals are not allowed in strict mode.',
    StrictDelete:  'Delete of an unqualified identifier in strict mode.',
    StrictDuplicateProperty:  'Duplicate data property in object literal not allowed in strict mode',
    AccessorDataProperty:  'Object literal may not have data and accessor property with the same name',
    AccessorGetSet:  'Object literal may not have multiple get/set accessors with the same name',
    StrictLHSAssignment:  'Assignment to eval or arguments is not allowed in strict mode',
    StrictLHSPostfix:  'Postfix increment/decrement may not have eval or arguments operand in strict mode',
    StrictLHSPrefix:  'Prefix increment/decrement may not have eval or arguments operand in strict mode',
    StrictReservedWord:  'Use of future reserved word in strict mode',
    CastingNothing:  'Body of cast expression empty',
    ArraySizeIntegral: 'Stack allocated array size must be an integer literal'
  };

  // See also tools/generate-unicode-regex.py.
  Regex = {
    NonAsciiIdentifierStart: new RegExp('[\xaa\xb5\xba\xc0-\xd6\xd8-\xf6\xf8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0370-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05d0-\u05ea\u05f0-\u05f2\u0620-\u064a\u066e\u066f\u0671-\u06d3\u06d5\u06e5\u06e6\u06ee\u06ef\u06fa-\u06fc\u06ff\u0710\u0712-\u072f\u074d-\u07a5\u07b1\u07ca-\u07ea\u07f4\u07f5\u07fa\u0800-\u0815\u081a\u0824\u0828\u0840-\u0858\u08a0\u08a2-\u08ac\u0904-\u0939\u093d\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097f\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bd\u09ce\u09dc\u09dd\u09df-\u09e1\u09f0\u09f1\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a59-\u0a5c\u0a5e\u0a72-\u0a74\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abd\u0ad0\u0ae0\u0ae1\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3d\u0b5c\u0b5d\u0b5f-\u0b61\u0b71\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bd0\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d\u0c58\u0c59\u0c60\u0c61\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbd\u0cde\u0ce0\u0ce1\u0cf1\u0cf2\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d\u0d4e\u0d60\u0d61\u0d7a-\u0d7f\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0e01-\u0e30\u0e32\u0e33\u0e40-\u0e46\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb0\u0eb2\u0eb3\u0ebd\u0ec0-\u0ec4\u0ec6\u0edc-\u0edf\u0f00\u0f40-\u0f47\u0f49-\u0f6c\u0f88-\u0f8c\u1000-\u102a\u103f\u1050-\u1055\u105a-\u105d\u1061\u1065\u1066\u106e-\u1070\u1075-\u1081\u108e\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u16ee-\u16f0\u1700-\u170c\u170e-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176c\u176e-\u1770\u1780-\u17b3\u17d7\u17dc\u1820-\u1877\u1880-\u18a8\u18aa\u18b0-\u18f5\u1900-\u191c\u1950-\u196d\u1970-\u1974\u1980-\u19ab\u19c1-\u19c7\u1a00-\u1a16\u1a20-\u1a54\u1aa7\u1b05-\u1b33\u1b45-\u1b4b\u1b83-\u1ba0\u1bae\u1baf\u1bba-\u1be5\u1c00-\u1c23\u1c4d-\u1c4f\u1c5a-\u1c7d\u1ce9-\u1cec\u1cee-\u1cf1\u1cf5\u1cf6\u1d00-\u1dbf\u1e00-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u2071\u207f\u2090-\u209c\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cee\u2cf2\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d80-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2e2f\u3005-\u3007\u3021-\u3029\u3031-\u3035\u3038-\u303c\u3041-\u3096\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua61f\ua62a\ua62b\ua640-\ua66e\ua67f-\ua697\ua6a0-\ua6ef\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua801\ua803-\ua805\ua807-\ua80a\ua80c-\ua822\ua840-\ua873\ua882-\ua8b3\ua8f2-\ua8f7\ua8fb\ua90a-\ua925\ua930-\ua946\ua960-\ua97c\ua984-\ua9b2\ua9cf\uaa00-\uaa28\uaa40-\uaa42\uaa44-\uaa4b\uaa60-\uaa76\uaa7a\uaa80-\uaaaf\uaab1\uaab5\uaab6\uaab9-\uaabd\uaac0\uaac2\uaadb-\uaadd\uaae0-\uaaea\uaaf2-\uaaf4\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabe2\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d\ufb1f-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe70-\ufe74\ufe76-\ufefc\uff21-\uff3a\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc]'),
    NonAsciiIdentifierPart: new RegExp('[\xaa\xb5\xba\xc0-\xd6\xd8-\xf6\xf8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0300-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u0483-\u0487\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u0591-\u05bd\u05bf\u05c1\u05c2\u05c4\u05c5\u05c7\u05d0-\u05ea\u05f0-\u05f2\u0610-\u061a\u0620-\u0669\u066e-\u06d3\u06d5-\u06dc\u06df-\u06e8\u06ea-\u06fc\u06ff\u0710-\u074a\u074d-\u07b1\u07c0-\u07f5\u07fa\u0800-\u082d\u0840-\u085b\u08a0\u08a2-\u08ac\u08e4-\u08fe\u0900-\u0963\u0966-\u096f\u0971-\u0977\u0979-\u097f\u0981-\u0983\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bc-\u09c4\u09c7\u09c8\u09cb-\u09ce\u09d7\u09dc\u09dd\u09df-\u09e3\u09e6-\u09f1\u0a01-\u0a03\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a3c\u0a3e-\u0a42\u0a47\u0a48\u0a4b-\u0a4d\u0a51\u0a59-\u0a5c\u0a5e\u0a66-\u0a75\u0a81-\u0a83\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abc-\u0ac5\u0ac7-\u0ac9\u0acb-\u0acd\u0ad0\u0ae0-\u0ae3\u0ae6-\u0aef\u0b01-\u0b03\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3c-\u0b44\u0b47\u0b48\u0b4b-\u0b4d\u0b56\u0b57\u0b5c\u0b5d\u0b5f-\u0b63\u0b66-\u0b6f\u0b71\u0b82\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bbe-\u0bc2\u0bc6-\u0bc8\u0bca-\u0bcd\u0bd0\u0bd7\u0be6-\u0bef\u0c01-\u0c03\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d-\u0c44\u0c46-\u0c48\u0c4a-\u0c4d\u0c55\u0c56\u0c58\u0c59\u0c60-\u0c63\u0c66-\u0c6f\u0c82\u0c83\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbc-\u0cc4\u0cc6-\u0cc8\u0cca-\u0ccd\u0cd5\u0cd6\u0cde\u0ce0-\u0ce3\u0ce6-\u0cef\u0cf1\u0cf2\u0d02\u0d03\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d-\u0d44\u0d46-\u0d48\u0d4a-\u0d4e\u0d57\u0d60-\u0d63\u0d66-\u0d6f\u0d7a-\u0d7f\u0d82\u0d83\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0dca\u0dcf-\u0dd4\u0dd6\u0dd8-\u0ddf\u0df2\u0df3\u0e01-\u0e3a\u0e40-\u0e4e\u0e50-\u0e59\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb9\u0ebb-\u0ebd\u0ec0-\u0ec4\u0ec6\u0ec8-\u0ecd\u0ed0-\u0ed9\u0edc-\u0edf\u0f00\u0f18\u0f19\u0f20-\u0f29\u0f35\u0f37\u0f39\u0f3e-\u0f47\u0f49-\u0f6c\u0f71-\u0f84\u0f86-\u0f97\u0f99-\u0fbc\u0fc6\u1000-\u1049\u1050-\u109d\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u135d-\u135f\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u16ee-\u16f0\u1700-\u170c\u170e-\u1714\u1720-\u1734\u1740-\u1753\u1760-\u176c\u176e-\u1770\u1772\u1773\u1780-\u17d3\u17d7\u17dc\u17dd\u17e0-\u17e9\u180b-\u180d\u1810-\u1819\u1820-\u1877\u1880-\u18aa\u18b0-\u18f5\u1900-\u191c\u1920-\u192b\u1930-\u193b\u1946-\u196d\u1970-\u1974\u1980-\u19ab\u19b0-\u19c9\u19d0-\u19d9\u1a00-\u1a1b\u1a20-\u1a5e\u1a60-\u1a7c\u1a7f-\u1a89\u1a90-\u1a99\u1aa7\u1b00-\u1b4b\u1b50-\u1b59\u1b6b-\u1b73\u1b80-\u1bf3\u1c00-\u1c37\u1c40-\u1c49\u1c4d-\u1c7d\u1cd0-\u1cd2\u1cd4-\u1cf6\u1d00-\u1de6\u1dfc-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u200c\u200d\u203f\u2040\u2054\u2071\u207f\u2090-\u209c\u20d0-\u20dc\u20e1\u20e5-\u20f0\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d7f-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2de0-\u2dff\u2e2f\u3005-\u3007\u3021-\u302f\u3031-\u3035\u3038-\u303c\u3041-\u3096\u3099\u309a\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua62b\ua640-\ua66f\ua674-\ua67d\ua67f-\ua697\ua69f-\ua6f1\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua827\ua840-\ua873\ua880-\ua8c4\ua8d0-\ua8d9\ua8e0-\ua8f7\ua8fb\ua900-\ua92d\ua930-\ua953\ua960-\ua97c\ua980-\ua9c0\ua9cf-\ua9d9\uaa00-\uaa36\uaa40-\uaa4d\uaa50-\uaa59\uaa60-\uaa76\uaa7a\uaa7b\uaa80-\uaac2\uaadb-\uaadd\uaae0-\uaaef\uaaf2-\uaaf6\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabea\uabec\uabed\uabf0-\uabf9\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe00-\ufe0f\ufe20-\ufe26\ufe33\ufe34\ufe4d-\ufe4f\ufe70-\ufe74\ufe76-\ufefc\uff10-\uff19\uff21-\uff3a\uff3f\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc]')
  };

  // A list of call expressions to be checked for if they really should be
  // cast expressions or call expressions.
  maybeCasts = [];

  // Ensure the condition is true, otherwise throw an error.
  // This is only to have a better contract semantic, i.e. another safety net
  // to catch a logic error. The condition shall be fulfilled in normal case.
  // Do NOT use this to enforce a certain condition on any user input.

  function assert(condition, message) {
    if (!condition) {
      throw new Error('ASSERT: ' + message);
    }
  }

  function sliceSource(from, to) {
    return source.slice(from, to);
  }

  if (typeof 'esprima'[0] === 'undefined') {
    sliceSource = function sliceArraySource(from, to) {
      return source.slice(from, to).join('');
    };
  }

  function isDecimalDigit(ch) {
    return '0123456789'.indexOf(ch) >= 0;
  }

  function isHexDigit(ch) {
    return '0123456789abcdefABCDEF'.indexOf(ch) >= 0;
  }

  function isOctalDigit(ch) {
    return '01234567'.indexOf(ch) >= 0;
  }

  // 7.2 White Space

  function isWhiteSpace(ch) {
    return (ch === ' ') || (ch === '\u0009') || (ch === '\u000B') ||
      (ch === '\u000C') || (ch === '\u00A0') ||
      (ch.charCodeAt(0) >= 0x1680 &&
       '\u1680\u180E\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200A\u202F\u205F\u3000\uFEFF'.indexOf(ch) >= 0);
  }

  // 7.3 Line Terminators

  function isLineTerminator(ch) {
    return (ch === '\n' || ch === '\r' || ch === '\u2028' || ch === '\u2029');
  }

  // 7.6 Identifier Names and Identifiers

  function isIdentifierStart(ch) {
    return (ch === '$') || (ch === '_') || (ch === '\\') ||
      (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
      ((ch.charCodeAt(0) >= 0x80) && Regex.NonAsciiIdentifierStart.test(ch));
  }

  function isIdentifierPart(ch) {
    return (ch === '$') || (ch === '_') || (ch === '\\') ||
      (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
      ((ch >= '0') && (ch <= '9')) ||
      ((ch.charCodeAt(0) >= 0x80) && Regex.NonAsciiIdentifierPart.test(ch));
  }

  // 7.6.1.2 Future Reserved Words

  function isFutureReservedWord(id) {
    switch (id) {

      // Future reserved words.
    case 'class':
    case 'enum':
    case 'export':
    case 'extends':
    case 'super':
      return true;
    }

    return false;
  }

  function toType(expr) {
    if (expr.type === Syntax.Identifier && Types.hasOwnProperty(expr.name)) {
      return {
        type: Syntax.TypeIdentifier,
        name: expr.name,
        range: expr.range
      };
    }

    if (expr.type === Syntax.PointerType) {
      return expr;
    }

    return null;
  }

  function isStrictModeReservedWord(id) {
    switch (id) {

      // Strict Mode reserved words.
    case 'implements':
    case 'interface':
    case 'package':
    case 'private':
    case 'protected':
    case 'public':
    case 'static':
    case 'yield':
    case 'let':
      return true;
    }

    return false;
  }

  function isRestrictedWord(id) {
    return id === 'eval' || id === 'arguments';
  }

  // 7.6.1.1 Keywords


  function isKeyword(id) {
    var keyword = false;
    switch (id.length) {
    case 2:
      keyword = (id === 'if') || (id === 'in') || (id === 'do');
      break;
    case 3:
      keyword = (id === 'var') || (id === 'for') || (id === 'new') || (id === 'try');
      break;
    case 4:
      keyword = (id === 'this') || (id === 'else') || (id === 'case') || (id === 'void') || (id === 'with') || (id === 'from');
      break;
    case 5:
      keyword = (id === 'while') || (id === 'break') || (id === 'catch') || (id === 'throw') || (id === 'union') ||
                (id === 'const');
      break;
    case 6:
      keyword = (id === 'return') || (id === 'typeof') || (id === 'delete') || (id === 'switch') ||
                (id === 'struct') || (id === 'sizeof') || (id === 'extern') || (id === 'public') || (id === 'static') ||
                (id === 'import');
      break;
    case 7:
      keyword = (id === 'default') || (id === 'finally') || (id === 'typedef') || (id === 'private');
      break;
    case 8:
      keyword = (id === 'function') || (id === 'continue') || (id === 'debugger');
      break;
    case 10:
      keyword = (id === 'instanceof');
      break;
    }

    if (keyword) {
      return true;
    }

    switch (id) {
      // Future reserved words.
      // 'const' is specialized as Keyword in V8.
    case 'const':
      return true;

      // For compatiblity to SpiderMonkey and ES.next
    case 'yield':
    case 'let':
      return true;
    }

    if (strict && isStrictModeReservedWord(id)) {
      return true;
    }

    return isFutureReservedWord(id);
  }

  // Return the next character and move forward.

  function nextChar() {
    return source[index++];
  }

  // 7.4 Comments

  function skipComment() {
    var ch, blockComment, lineComment;

    blockComment = false;
    lineComment = false;

    while (index < length) {
      ch = source[index];

      if (lineComment) {
        ch = nextChar();
        if (isLineTerminator(ch)) {
          lineComment = false;
          if (ch === '\r' && source[index] === '\n') {
            ++index;
          }
          ++lineNumber;
          lineStart = index;
        }
      } else if (blockComment) {
        if (isLineTerminator(ch)) {
          if (ch === '\r' && source[index + 1] === '\n') {
            ++index;
          }
          ++lineNumber;
          ++index;
          lineStart = index;
          if (index >= length) {
            throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
          }
        } else {
          ch = nextChar();
          if (index >= length) {
            throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
          }
          if (ch === '*') {
            ch = source[index];
            if (ch === '/') {
              ++index;
              blockComment = false;
            }
          }
        }
      } else if (ch === '/') {
        ch = source[index + 1];
        if (ch === '/') {
          index += 2;
          lineComment = true;
        } else if (ch === '*') {
          index += 2;
          blockComment = true;
          if (index >= length) {
            throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
          }
        } else {
          break;
        }
      } else if (isWhiteSpace(ch)) {
        ++index;
      } else if (isLineTerminator(ch)) {
        ++index;
        if (ch ===  '\r' && source[index] === '\n') {
          ++index;
        }
        ++lineNumber;
        lineStart = index;
      } else {
        break;
      }
    }
  }

  function scanHexEscape(prefix) {
    var i, len, ch, code = 0;

    len = (prefix === 'u') ? 4 : 2;
    for (i = 0; i < len; ++i) {
      if (index < length && isHexDigit(source[index])) {
        ch = nextChar();
        code = code * 16 + '0123456789abcdef'.indexOf(ch.toLowerCase());
      } else {
        return '';
      }
    }
    return String.fromCharCode(code);
  }

  function scanIdentifier() {
    var ch, start, id, restore;

    ch = source[index];
    if (!isIdentifierStart(ch)) {
      return;
    }

    start = index;
    if (ch === '\\') {
      ++index;
      if (source[index] !== 'u') {
        return;
      }
      ++index;
      restore = index;
      ch = scanHexEscape('u');
      if (ch) {
        if (ch === '\\' || !isIdentifierStart(ch)) {
          return;
        }
        id = ch;
      } else {
        index = restore;
        id = 'u';
      }
    } else {
      id = nextChar();
    }

    while (index < length) {
      ch = source[index];
      if (!isIdentifierPart(ch)) {
        break;
      }
      if (ch === '\\') {
        ++index;
        if (source[index] !== 'u') {
          return;
        }
        ++index;
        restore = index;
        ch = scanHexEscape('u');
        if (ch) {
          if (ch === '\\' || !isIdentifierPart(ch)) {
            return;
          }
          id += ch;
        } else {
          index = restore;
          id += 'u';
        }
      } else {
        id += nextChar();
      }
    }

    // There is no keyword or literal with only one character.
    // Thus, it must be an identifier.
    if (id.length === 1) {
      return {
        type: Token.Identifier,
        value: id,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    if (isKeyword(id)) {
      return {
        type: Token.Keyword,
        value: id,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    // 7.8.1 Null Literals

    if (id === 'null') {
      return {
        type: Token.NullLiteral,
        value: id,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    // 7.8.2 Boolean Literals

    if (id === 'true' || id === 'false') {
      return {
        type: Token.BooleanLiteral,
        value: id,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    return {
      type: Token.Identifier,
      value: id,
      lineNumber: lineNumber,
      lineStart: lineStart,
      range: [start, index]
    };
  }

  // 7.7 Punctuators

  function scanPunctuator() {
    var start = index,
        ch1 = source[index],
        ch2,
        ch3,
        ch4;

    // Check for most common single-character punctuators.

    if (ch1 === ';' || ch1 === '{' || ch1 === '}') {
      ++index;
      return {
        type: Token.Punctuator,
        value: ch1,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    if (ch1 === ',' || ch1 === '(' || ch1 === ')') {
      ++index;
      return {
        type: Token.Punctuator,
        value: ch1,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    // Dot (.) can also start a floating-point number, hence the need
    // to check the next character.

    ch2 = source[index + 1];
    if (ch1 === '.' && !isDecimalDigit(ch2)) {
      return {
        type: Token.Punctuator,
        value: nextChar(),
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    // Peek more characters.

    ch3 = source[index + 2];
    ch4 = source[index + 3];

    // 4-character punctuator: >>>=

    if (ch1 === '>' && ch2 === '>' && ch3 === '>') {
      if (ch4 === '=') {
        index += 4;
        return {
          type: Token.Punctuator,
          value: '>>>=',
          lineNumber: lineNumber,
          lineStart: lineStart,
          range: [start, index]
        };
      }
    }

    // 3-character punctuators: === !== >>> <<= >>=

    if (ch1 === '=' && ch2 === '=' && ch3 === '=') {
      index += 3;
      return {
        type: Token.Punctuator,
        value: '===',
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    if (ch1 === '!' && ch2 === '=' && ch3 === '=') {
      index += 3;
      return {
        type: Token.Punctuator,
        value: '!==',
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    if (ch1 === '>' && ch2 === '>' && ch3 === '>') {
      index += 3;
      return {
        type: Token.Punctuator,
        value: '>>>',
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    if (ch1 === '<' && ch2 === '<' && ch3 === '=') {
      index += 3;
      return {
        type: Token.Punctuator,
        value: '<<=',
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    if (ch1 === '>' && ch2 === '>' && ch3 === '=') {
      index += 3;
      return {
        type: Token.Punctuator,
        value: '>>=',
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    // 2-character punctuators: <= >= == != ++ -- << >> && ||
    // += -= *= %= &= |= ^= /= ->

    if (ch2 === '=') {
      if ('<>=!+-*%&|^/'.indexOf(ch1) >= 0) {
        index += 2;
        return {
          type: Token.Punctuator,
          value: ch1 + ch2,
          lineNumber: lineNumber,
          lineStart: lineStart,
          range: [start, index]
        };
      }
    }

    if (ch1 === ch2 && ('+-<>&|'.indexOf(ch1) >= 0)) {
      if ('+-<>&|'.indexOf(ch2) >= 0) {
        index += 2;
        return {
          type: Token.Punctuator,
          value: ch1 + ch2,
          lineNumber: lineNumber,
          lineStart: lineStart,
          range: [start, index]
        };
      }
    }

    if (ch1 === '-' && ch2 === '>') {
      index += 2;
      return {
        type: Token.Punctuator,
        value: ch1 + ch2,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }

    // The remaining 1-character punctuators.

    if ('[]<>+-*%&|^!~?:=/'.indexOf(ch1) >= 0) {
      return {
        type: Token.Punctuator,
        value: nextChar(),
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [start, index]
      };
    }
  }

  // 7.8.3 Numeric Literals

  function scanNumericLiteral() {
    var number, start, ch;

    ch = source[index];
    assert(isDecimalDigit(ch) || (ch === '.'),
           'Numeric literal must start with a decimal digit or a decimal point');

    start = index;
    number = '';
    if (ch !== '.') {
      number = nextChar();
      ch = source[index];

      // Hex number starts with '0x'.
      // Octal number starts with '0'.
      if (number === '0') {
        if (ch === 'x' || ch === 'X') {
          number += nextChar();
          while (index < length) {
            ch = source[index];
            if (!isHexDigit(ch)) {
              break;
            }
            number += nextChar();
          }

          if (number.length <= 2) {
            // only 0x
            throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
          }

          if (index < length) {
            ch = source[index];
            if (isIdentifierStart(ch)) {
              throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
            }
          }
          return {
            type: Token.NumericLiteral,
            value: parseInt(number, 16),
            lineNumber: lineNumber,
            lineStart: lineStart,
            range: [start, index]
          };
        } else if (isOctalDigit(ch)) {
          number += nextChar();
          while (index < length) {
            ch = source[index];
            if (!isOctalDigit(ch)) {
              break;
            }
            number += nextChar();
          }

          if (index < length) {
            ch = source[index];
            if (isIdentifierStart(ch) || isDecimalDigit(ch)) {
              throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
            }
          }
          return {
            type: Token.NumericLiteral,
            value: parseInt(number, 8),
            octal: true,
            lineNumber: lineNumber,
            lineStart: lineStart,
            range: [start, index]
          };
        }

        // decimal number starts with '0' such as '09' is illegal.
        if (isDecimalDigit(ch)) {
          throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
        }
      }

      while (index < length) {
        ch = source[index];
        if (!isDecimalDigit(ch)) {
          break;
        }
        number += nextChar();
      }
    }

    if (ch === '.') {
      number += nextChar();
      while (index < length) {
        ch = source[index];
        if (!isDecimalDigit(ch)) {
          break;
        }
        number += nextChar();
      }
    }

    if (ch === 'e' || ch === 'E') {
      number += nextChar();

      ch = source[index];
      if (ch === '+' || ch === '-') {
        number += nextChar();
      }

      ch = source[index];
      if (isDecimalDigit(ch)) {
        number += nextChar();
        while (index < length) {
          ch = source[index];
          if (!isDecimalDigit(ch)) {
            break;
          }
          number += nextChar();
        }
      } else {
        ch = 'character ' + ch;
        if (index >= length) {
          ch = '<end>';
        }
        throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
      }
    }

    if (index < length) {
      ch = source[index];
      if (isIdentifierStart(ch)) {
        throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
      }
    }

    return {
      type: Token.NumericLiteral,
      value: parseFloat(number),
      lineNumber: lineNumber,
      lineStart: lineStart,
      range: [start, index]
    };
  }

  // 7.8.4 String Literals

  function scanStringLiteral() {
    var str = '', quote, start, ch, code, unescaped, restore, octal = false;

    quote = source[index];
    assert((quote === '\'' || quote === '"'),
           'String literal must starts with a quote');

    start = index;
    ++index;

    while (index < length) {
      ch = nextChar();

      if (ch === quote) {
        quote = '';
        break;
      } else if (ch === '\\') {
        ch = nextChar();
        if (!isLineTerminator(ch)) {
          switch (ch) {
          case 'n':
            str += '\n';
            break;
          case 'r':
            str += '\r';
            break;
          case 't':
            str += '\t';
            break;
          case 'u':
          case 'x':
            restore = index;
            unescaped = scanHexEscape(ch);
            if (unescaped) {
              str += unescaped;
            } else {
              index = restore;
              str += ch;
            }
            break;
          case 'b':
            str += '\b';
            break;
          case 'f':
            str += '\f';
            break;
          case 'v':
            str += '\v';
            break;

          default:
            if (isOctalDigit(ch)) {
              code = '01234567'.indexOf(ch);

              // \0 is not octal escape sequence
              if (code !== 0) {
                octal = true;
              }

              if (index < length && isOctalDigit(source[index])) {
                octal = true;
                code = code * 8 + '01234567'.indexOf(nextChar());

                // 3 digits are only allowed when string starts
                // with 0, 1, 2, 3
                if ('0123'.indexOf(ch) >= 0 &&
                    index < length &&
                    isOctalDigit(source[index])) {
                  code = code * 8 + '01234567'.indexOf(nextChar());
                }
              }
              str += String.fromCharCode(code);
            } else {
              str += ch;
            }
            break;
          }
        } else {
          ++lineNumber;
          if (ch ===  '\r' && source[index] === '\n') {
            ++index;
          }
        }
      } else if (isLineTerminator(ch)) {
        break;
      } else {
        str += ch;
      }
    }

    if (quote !== '') {
      throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
    }

    return {
      type: Token.StringLiteral,
      value: str,
      octal: octal,
      lineNumber: lineNumber,
      lineStart: lineStart,
      range: [start, index]
    };
  }

  function scanRegExp() {
    var str = '', ch, start, pattern, flags, value, classMarker = false, restore;

    buffer = null;
    skipComment();

    start = index;
    ch = source[index];
    assert(ch === '/', 'Regular expression literal must start with a slash');
    str = nextChar();

    while (index < length) {
      ch = nextChar();
      str += ch;
      if (classMarker) {
        if (ch === ']') {
          classMarker = false;
        }
      } else {
        if (ch === '\\') {
          str += nextChar();
        }
        if (ch === '/') {
          break;
        }
        if (ch === '[') {
          classMarker = true;
        }
        if (isLineTerminator(ch)) {
          throwError({}, Messages.UnterminatedRegExp);
        }
      }
    }

    if (str.length === 1) {
      throwError({}, Messages.UnterminatedRegExp);
    }

    // Exclude leading and trailing slash.
    pattern = str.substr(1, str.length - 2);

    flags = '';
    while (index < length) {
      ch = source[index];
      if (!isIdentifierPart(ch)) {
        break;
      }

      ++index;
      if (ch === '\\' && index < length) {
        ch = source[index];
        if (ch === 'u') {
          ++index;
          restore = index;
          ch = scanHexEscape('u');
          if (ch) {
            flags += ch;
            str += '\\u';
            for (; restore < index; ++restore) {
              str += source[restore];
            }
          } else {
            index = restore;
            flags += 'u';
            str += '\\u';
          }
        } else {
          str += '\\';
        }
      } else {
        flags += ch;
        str += ch;
      }
    }

    try {
      value = new RegExp(pattern, flags);
    } catch (e) {
      throwError({}, Messages.InvalidRegExp);
    }

    return {
      literal: str,
      value: value,
      range: [start, index]
    };
  }

  function isIdentifierName(token) {
    return token.type === Token.Identifier ||
      token.type === Token.Keyword ||
      token.type === Token.BooleanLiteral ||
      token.type === Token.NullLiteral;
  }

  function advance() {
    var ch, token;

    skipComment();

    if (index >= length) {
      return {
        type: Token.EOF,
        lineNumber: lineNumber,
        lineStart: lineStart,
        range: [index, index]
      };
    }

    token = scanPunctuator();
    if (typeof token !== 'undefined') {
      return token;
    }

    ch = source[index];

    if (ch === '\'' || ch === '"') {
      return scanStringLiteral();
    }

    if (ch === '.' || isDecimalDigit(ch)) {
      return scanNumericLiteral();
    }

    token = scanIdentifier();
    if (typeof token !== 'undefined') {
      return token;
    }

    throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
  }

  function lex() {
    var token;

    if (buffer) {
      index = buffer.range[1];
      lineNumber = buffer.lineNumber;
      lineStart = buffer.lineStart;
      token = buffer;
      buffer = null;
      return token;
    }

    buffer = null;
    return advance();
  }

  function lookahead() {
    var pos, line, start;

    if (buffer !== null) {
      return buffer;
    }

    pos = index;
    line = lineNumber;
    start = lineStart;
    buffer = advance();
    index = pos;
    lineNumber = line;
    lineStart = start;

    return buffer;
  }

  function mark() {
    return {buffer: buffer, index: index, lineNumber: lineNumber, lineStart: lineStart};
  }

  function reset(m) {
    buffer = m.buffer;
    index = m.index;
    lineStart = m.lineStart;
    lineNumber = m.lineNumber;
  }

  // Return true if there is a line terminator before the next token.

  function peekLineTerminator() {
    var pos, line, start, found;

    pos = index;
    line = lineNumber;
    start = lineStart;
    skipComment();
    found = lineNumber !== line;
    index = pos;
    lineNumber = line;
    lineStart = start;

    return found;
  }

  // Throw an exception

  function throwError(token, messageFormat) {
    var error,
        args = Array.prototype.slice.call(arguments, 2),
        msg = messageFormat.replace(
            /%(\d)/g,
          function (whole, index) {
            return args[index] || '';
          }
        );

    if (typeof token.lineNumber === 'number') {
      error = new Error(msg);
      error.index = token.range[0];
      error.lineNumber = token.lineNumber;
      error.column = token.range[0] - lineStart + 1;
    } else {
      error = new Error(msg);
      error.index = index;
      error.lineNumber = lineNumber;
      error.column = index - lineStart + 1;
    }

    throw error;
  }

  function throwErrorTolerant() {
    var error;
    try {
      throwError.apply(null, arguments);
    } catch (e) {
      if (extra.errors) {
        extra.errors.push(e);
      } else {
        throw e;
      }
    }
  }


  // Throw an exception because of the token.

  function throwUnexpected(token) {
    var s;

    if (token.type === Token.EOF) {
      throwError(token, Messages.UnexpectedEOS);
    }

    if (token.type === Token.NumericLiteral) {
      throwError(token, Messages.UnexpectedNumber);
    }

    if (token.type === Token.StringLiteral) {
      throwError(token, Messages.UnexpectedString);
    }

    if (token.type === Token.Identifier) {
      throwError(token, Messages.UnexpectedIdentifier);
    }

    if (token.type === Token.Keyword) {
      if (isFutureReservedWord(token.value)) {
        throwError(token, Messages.UnexpectedReserved);
      } else if (strict && isStrictModeReservedWord(token.value)) {
        throwError(token, Messages.StrictReservedWord);
      }
      throwError(token, Messages.UnexpectedToken, token.value);
    }

    // BooleanLiteral, NullLiteral, or Punctuator.
    throwError(token, Messages.UnexpectedToken, token.value);
  }

  // Expect the next token to match the specified punctuator.
  // If not, an exception will be thrown.

  function expect(value) {
    var token = lex();
    if (token.type !== Token.Punctuator || token.value !== value) {
      throwUnexpected(token);
    }
  }

  // Expect the next token to match the specified keyword.
  // If not, an exception will be thrown.

  function expectKeyword(keyword) {
    var token = lex();
    if (token.type !== Token.Keyword || token.value !== keyword) {
      throwUnexpected(token);
    }
  }

  // Return true if the next token matches the specified punctuator.

  function match(value) {
    var token = lookahead();
    return token.type === Token.Punctuator && token.value === value;
  }

  // Return true if the next token matches the specified keyword

  function matchKeyword(keyword) {
    var token = lookahead();
    return token.type === Token.Keyword && token.value === keyword;
  }

  // Return true if the next token is an assignment operator

  function matchAssign() {
    var token = lookahead(),
        op = token.value;

    if (token.type !== Token.Punctuator) {
      return false;
    }
    return op === '=' ||
      op === '*=' ||
      op === '/=' ||
      op === '%=' ||
      op === '+=' ||
      op === '-=' ||
      op === '<<=' ||
      op === '>>=' ||
      op === '>>>=' ||
      op === '&=' ||
      op === '^=' ||
      op === '|=';
  }

  function consumeSemicolon() {
    var token, line;

    // Catch the very common case first.
    if (source[index] === ';') {
      lex();
      return;
    }

    line = lineNumber;
    skipComment();
    if (lineNumber !== line) {
      return;
    }

    if (match(';')) {
      lex();
      return;
    }

    token = lookahead();
    if (token.type !== Token.EOF && !match('}')) {
      throwUnexpected(token);
    }
    return;
  }

  // Return true if provided expression is LeftHandSideExpression

  function isLeftHandSide(expr) {
    switch (expr.type) {
    case 'AssignmentExpression':
    case 'BinaryExpression':
    case 'ConditionalExpression':
    case 'LogicalExpression':
    case 'SequenceExpression':
    case 'UpdateExpression':
      return false;
    case 'UnaryExpression':
      return expr.operator === '*';
    }
    return true;
  }

  // 11.1.4 Array Initialiser

  function parseArrayInitialiser() {
    var elements = [],
        undef;

    expect('[');

    while (!match(']')) {
      if (match(',')) {
        lex();
        elements.push(undef);
      } else {
        elements.push(parseAssignmentExpression());

        if (!match(']')) {
          expect(',');
        }
      }
    }

    expect(']');

    return {
      type: Syntax.ArrayExpression,
      elements: elements
    };
  }

  // 11.1.5 Object Initialiser

  function parsePropertyFunction(param, first) {
    var previousStrict, body;

    previousStrict = strict;
    body = parseFunctionSourceElements();
    if (first && strict && isRestrictedWord(param[0].name)) {
      throwError(first, Messages.StrictParamName);
    }
    strict = previousStrict;

    return {
      type: Syntax.FunctionExpression,
      id: null,
      params: param,
      body: body
    };
  }

  function parseObjectPropertyKey() {
    var token = lex();

    // Note: This function is called only from parseObjectProperty(), where
    // EOF and Punctuator tokens are already filtered out.

    if (token.type === Token.StringLiteral || token.type === Token.NumericLiteral) {
      if (strict && token.octal) {
        throwError(token, Messages.StrictOctalLiteral);
      }
      return createLiteral(token);
    }

    return {
      type: Syntax.Identifier,
      name: token.value
    };
  }

  function parseObjectProperty() {
    var token, key, id, param;

    token = lookahead();

    if (token.type === Token.Identifier) {

      id = parseObjectPropertyKey();

      // Property Assignment: Getter and Setter.

      if (token.value === 'get' && !match(':')) {
        key = parseObjectPropertyKey();
        expect('(');
        expect(')');
        return {
          type: Syntax.Property,
          key: key,
          value: parsePropertyFunction([]),
          kind: 'get'
        };
      } else if (token.value === 'set' && !match(':')) {
        key = parseObjectPropertyKey();
        expect('(');
        token = lookahead();
        if (token.type !== Token.Identifier) {
          throwUnexpected(lex());
        }
        param = [ parseVariableIdentifier() ];
        expect(')');
        return {
          type: Syntax.Property,
          key: key,
          value: parsePropertyFunction(param, token),
          kind: 'set'
        };
      } else {
        expect(':');
        return {
          type: Syntax.Property,
          key: id,
          value: parseAssignmentExpression(),
          kind: 'init'
        };
      }
    } else if (token.type === Token.EOF || token.type === Token.Punctuator) {
      throwUnexpected(token);
    } else {
      key = parseObjectPropertyKey();
      expect(':');
      return {
        type: Syntax.Property,
        key: key,
        value: parseAssignmentExpression(),
        kind: 'init'
      };
    }
  }

  function parseObjectInitialiser() {
    var token, properties = [], property, name, kind, map = {}, toString = String;

    expect('{');

    while (!match('}')) {
      property = parseObjectProperty();

      if (property.key.type === Syntax.Identifier) {
        name = property.key.name;
      } else {
        name = toString(property.key.value);
      }
      kind = (property.kind === 'init') ? PropertyKind.Data : (property.kind === 'get') ? PropertyKind.Get : PropertyKind.Set;
      if (Object.prototype.hasOwnProperty.call(map, name)) {
        if (map[name] === PropertyKind.Data) {
          if (strict && kind === PropertyKind.Data) {
            throwError({}, Messages.StrictDuplicateProperty);
          } else if (kind !== PropertyKind.Data) {
            throwError({}, Messages.AccessorDataProperty);
          }
        } else {
          if (kind === PropertyKind.Data) {
            throwError({}, Messages.AccessorDataProperty);
          } else if (map[name] & kind) {
            throwError({}, Messages.AccessorGetSet);
          }
        }
        map[name] |= kind;
      } else {
        map[name] = kind;
      }

      properties.push(property);

      if (!match('}')) {
        expect(',');
      }
    }

    expect('}');

    return {
      type: Syntax.ObjectExpression,
      properties: properties
    };
  }

  // 11.1 Primary Expressions

  function parsePrimaryExpression() {
    var expr,
        token = lookahead(),
        type = token.type;

    if (type === Token.Identifier) {
      return {
        type: Syntax.Identifier,
        name: lex().value,
        kind: "variable"
      };
    }

    if (type === Token.StringLiteral || type === Token.NumericLiteral) {
      if (strict && token.octal) {
        throwError(token, Messages.StrictOctalLiteral);
      }
      return createLiteral(lex());
    }

    if (type === Token.Keyword) {
      if (matchKeyword('this')) {
        lex();
        return {
          type: Syntax.ThisExpression
        };
      }

      if (matchKeyword('function')) {
        return parseFunctionExpression();
      }
    }

    if (type === Token.BooleanLiteral) {
      lex();
      token.value = (token.value === 'true');
      return createLiteral(token);
    }

    if (type === Token.NullLiteral) {
      lex();
      token.value = null;
      return createLiteral(token);
    }

    if (match('[')) {
      return parseArrayInitialiser();
    }

    if (match('{')) {
      return parseObjectInitialiser();
    }

    if (match('(')) {
      lex();
      state.lastParenthesized = expr = parseExpression();
      expect(')');
      return expr;
    }

    if (match('/') || match('/=')) {
      return createLiteral(scanRegExp());
    }

    return throwUnexpected(lex());
  }

  // 11.2 Left-Hand-Side Expressions

  function parseArguments() {
    var args = [];

    expect('(');

    if (!match(')')) {
      while (index < length) {
        args.push(parseAssignmentExpression());
        if (match(')')) {
          break;
        }
        expect(',');
      }
    }

    expect(')');

    return args;
  }

  function parseNonComputedProperty() {
    var token = lex();

    if (!isIdentifierName(token)) {
      throwUnexpected(token);
    }

    return {
      type: Syntax.Identifier,
      name: token.value
    };
  }

  function parseNonComputedMember(object) {
    return {
      type: Syntax.MemberExpression,
      computed: false,
      object: object,
      property: parseNonComputedProperty()
    };
  }

  function parseComputedMember(object) {
    var property, expr;

    expect('[');
    property = parseExpression();
    expr = {
      type: Syntax.MemberExpression,
      computed: true,
      object: object,
      property: property
    };
    expect(']');
    return expr;
  }

  function parseCallMember(object) {
    return {
      type: Syntax.CallExpression,
      callee: object,
      'arguments': parseArguments()
    };
  }

  function parseNewExpression() {
    var expr;

    expectKeyword('new');

    expr = {
      type: Syntax.NewExpression,
      callee: parseLeftHandSideExpression(),
      'arguments': []
    };

    if (match('(')) {
      expr['arguments'] = parseArguments();
    }

    return expr;
  }

  function parseLeftHandSideExpressionAllowCall() {
    var useNew, expr, m, cast = false, args;

    useNew = matchKeyword('new');
    if (useNew) {
      expr = parseNewExpression();
    } else {
      if (match('(')) {
        m = mark();
        lex();
        if (lookahead().type === Token.Identifier) {
          expr = parseInlineableType();
          if (!match(')') || !expr || expr.type === Syntax.TypeIdentifier) {
            reset(m);
            expr = parsePrimaryExpression();
          } else {
            expect(')');
            cast = true;
          }
        } else {
          reset(m);
          expr = parsePrimaryExpression();
        }
      } else {
        expr = parsePrimaryExpression();
      }
    }

    while (index < length) {
      if (match('.') || match('->')) {
        var token = lookahead();
        lex();
        expr = parseNonComputedMember(expr);
        expr.kind = token.value;
      } else if (match('[')) {
        expr = parseComputedMember(expr);
      } else if (match('(')) {
        if (expr.type === Syntax.Identifier) {
          expr = parseCallMember(expr);
          maybeCasts.push(expr);
        } else {
          expr = parseCallMember(expr);
        }
      } else {
        break;
      }
    }

    if (cast) {
      if (expr.type !== Syntax.CallExpression) {
        throwUnexpected(lookahead());
      }
      disambiguateCast(expr);
    }

    return expr;
  }

  function parseLeftHandSideExpression() {
    var useNew, expr;

    useNew = matchKeyword('new');
    expr = useNew ? parseNewExpression() : parsePrimaryExpression();

    while (index < length) {
      if (match('.') || match('->')) {
        var token = lookahead();
        lex();
        expr = parseNonComputedMember(expr);
        expr.kind = token.value;
      } else if (match('[')) {
        expr = parseComputedMember(expr);
      } else {
        break;
      }
    }

    return expr;
  }

  // 11.3 Postfix Expressions

  function parsePostfixExpression() {
    var expr = parseLeftHandSideExpressionAllowCall();

    if ((match('++') || match('--') && !peekLineTerminator())) {
      // 11.3.1, 11.3.2
      if (strict && expr.type === Syntax.Identifier && isRestrictedWord(expr.name)) {
        throwError({}, Messages.StrictLHSPostfix);
      }
      expr = {
        type: Syntax.UpdateExpression,
        operator: lex().value,
        argument: expr,
        prefix: false
      };
    }

    return expr;
  }

  // 11.4 Unary Operators

  function parseUnaryExpression() {
    var token, expr;

    if (match('++') || match('--')) {
      token = lex();
      expr = parseUnaryExpression();
      // 11.4.4, 11.4.5
      if (strict && expr.type === Syntax.Identifier && isRestrictedWord(expr.name)) {
        throwError({}, Messages.StrictLHSPrefix);
      }
      expr = {
        type: Syntax.UpdateExpression,
        operator: token.value,
        argument: expr,
        prefix: true
      };
      return expr;
    }

    if (match('+') || match('-') || match('~') || match('!') || match('&') || match('*')) {
      expr = {
        type: Syntax.UnaryExpression,
        operator: lex().value,
        argument: parseUnaryExpression()
      };
      return expr;
    }

    if (matchKeyword('sizeof')) {
      lex();
      expect('(');
      expr = {
        type: Syntax.UnaryExpression,
        operator: 'sizeof',
        argument: parseInlineableType(true)
      };
      expect(')');
      return expr;
    }

    if (matchKeyword('delete') || matchKeyword('void') || matchKeyword('typeof')) {
      expr = {
        type: Syntax.UnaryExpression,
        operator: lex().value,
        argument: parseUnaryExpression()
      };
      if (strict && expr.operator === 'delete' && expr.argument.type === Syntax.Identifier) {
        throwError({}, Messages.StrictDelete);
      }
      return expr;
    }

    return parsePostfixExpression();
  }

  // 11.5 Multiplicative Operators

  function parseMultiplicativeExpression() {
    var expr = parseUnaryExpression();

    while (match('*') || match('/') || match('%')) {
      expr = {
        type: Syntax.BinaryExpression,
        operator: lex().value,
        left: expr,
        right: parseUnaryExpression()
      };
    }

    return expr;
  }

  // 11.6 Additive Operators

  function parseAdditiveExpression() {
    var expr = parseMultiplicativeExpression();

    while (match('+') || match('-')) {
      expr = {
        type: Syntax.BinaryExpression,
        operator: lex().value,
        left: expr,
        right: parseMultiplicativeExpression()
      };
    }

    return expr;
  }

  // 11.7 Bitwise Shift Operators

  function parseShiftExpression() {
    var expr = parseAdditiveExpression();

    while (match('<<') || match('>>') || match('>>>')) {
      expr = {
        type: Syntax.BinaryExpression,
        operator: lex().value,
        left: expr,
        right: parseAdditiveExpression()
      };
    }

    return expr;
  }
  // 11.8 Relational Operators

  function parseRelationalExpression() {
    var expr, previousAllowIn;

    previousAllowIn = state.allowIn;
    state.allowIn = true;
    expr = parseShiftExpression();
    state.allowIn = previousAllowIn;

    if (match('<') || match('>') || match('<=') || match('>=')) {
      expr = {
        type: Syntax.BinaryExpression,
        operator: lex().value,
        left: expr,
        right: parseRelationalExpression()
      };
    } else if (state.allowIn && matchKeyword('in')) {
      lex();
      expr = {
        type: Syntax.BinaryExpression,
        operator: 'in',
        left: expr,
        right: parseRelationalExpression()
      };
    } else if (matchKeyword('instanceof')) {
      lex();
      expr = {
        type: Syntax.BinaryExpression,
        operator: 'instanceof',
        left: expr,
        right: parseRelationalExpression()
      };
    }

    return expr;
  }

  // 11.9 Equality Operators

  function parseEqualityExpression() {
    var expr = parseRelationalExpression();

    while (match('==') || match('!=') || match('===') || match('!==')) {
      expr = {
        type: Syntax.BinaryExpression,
        operator: lex().value,
        left: expr,
        right: parseRelationalExpression()
      };
    }

    return expr;
  }

  // 11.10 Binary Bitwise Operators

  function parseBitwiseANDExpression() {
    var expr = parseEqualityExpression();

    while (match('&')) {
      lex();
      expr = {
        type: Syntax.BinaryExpression,
        operator: '&',
        left: expr,
        right: parseEqualityExpression()
      };
    }

    return expr;
  }

  function parseBitwiseXORExpression() {
    var expr = parseBitwiseANDExpression();

    while (match('^')) {
      lex();
      expr = {
        type: Syntax.BinaryExpression,
        operator: '^',
        left: expr,
        right: parseBitwiseANDExpression()
      };
    }

    return expr;
  }

  function parseBitwiseORExpression() {
    var expr = parseBitwiseXORExpression();

    while (match('|')) {
      lex();
      expr = {
        type: Syntax.BinaryExpression,
        operator: '|',
        left: expr,
        right: parseBitwiseXORExpression()
      };
    }

    return expr;
  }

  // 11.11 Binary Logical Operators

  function parseLogicalANDExpression() {
    var expr = parseBitwiseORExpression();

    while (match('&&')) {
      lex();
      expr = {
        type: Syntax.LogicalExpression,
        operator: '&&',
        left: expr,
        right: parseBitwiseORExpression()
      };
    }

    return expr;
  }

  function parseLogicalORExpression() {
    var expr = parseLogicalANDExpression();

    while (match('||')) {
      lex();
      expr = {
        type: Syntax.LogicalExpression,
        operator: '||',
        left: expr,
        right: parseLogicalANDExpression()
      };
    }

    return expr;
  }

  // 11.12 Conditional Operator

  function parseConditionalExpression() {
    var expr, previousAllowIn, consequent;

    expr = parseLogicalORExpression();

    if (match('?')) {
      lex();
      previousAllowIn = state.allowIn;
      state.allowIn = true;
      consequent = parseAssignmentExpression();
      state.allowIn = previousAllowIn;
      expect(':');

      expr = {
        type: Syntax.ConditionalExpression,
        test: expr,
        consequent: consequent,
        alternate: parseAssignmentExpression()
      };
    }

    return expr;
  }

  // 11.13 Assignment Operators

  function parseAssignmentExpression() {
    var expr;

    expr = parseConditionalExpression();

    if (matchAssign()) {
      // LeftHandSideExpression
      if (state.lastParenthesized !== expr && !isLeftHandSide(expr)) {
        throwError({}, Messages.InvalidLHSInAssignment);
      }

      // 11.13.1
      if (strict && expr.type === Syntax.Identifier && isRestrictedWord(expr.name)) {
        throwError({}, Messages.StrictLHSAssignment);
      }

      expr = {
        type: Syntax.AssignmentExpression,
        operator: lex().value,
        left: expr,
        right: parseAssignmentExpression()
      };
    }

    return expr;
  }

  // 11.14 Comma Operator

  function parseExpression() {
    var expr = parseAssignmentExpression();

    if (match(',')) {
      expr = {
        type: Syntax.SequenceExpression,
        expressions: [ expr ]
      };

      while (index < length) {
        if (!match(',')) {
          break;
        }
        lex();
        expr.expressions.push(parseAssignmentExpression());
      }

    }
    return expr;
  }

  // 12.1 Block

  function parseStatementList() {
    var list = [],
        statement;

    while (index < length) {
      if (match('}')) {
        break;
      }
      statement = parseSourceElement();
      if (typeof statement === 'undefined') {
        break;
      }
      list.push(statement);
    }

    return list;
  }

  function parseBlock() {
    var block;

    expect('{');

    block = parseStatementList();

    expect('}');

    return {
      type: Syntax.BlockStatement,
      body: block
    };
  }

  // 12.2 Variable Statement

  function parseVariableIdentifier() {
    var token = lex();

    if (token.type !== Token.Identifier) {
      throwUnexpected(token);
    }

    return {
      type: Syntax.Identifier,
      name: token.value,
      kind: "variable"
    };
  }

  function parsePointerType(base) {
    if (!base) {
      return undefined;
    }
    var ty = base;
    while (match('*')) {
      lex();
      ty = {
        type: Syntax.PointerType,
        base: ty,
        range: ty.range
      };
    }
    return ty;
  }

  function parseInlineableType(force) {
    return parsePointerType(parseTypeIdentifier(force));
  }

  function parseVariableDeclaration(kind, noAssignment, typeIdentifier) {
    var declaredType = parsePointerType(typeIdentifier),
        id = parseVariableIdentifier(),
        init = null;

    var args = undefined;

    if (match('[')) {
      if (kind === 'const') {
        throwError({}, Messages.ArrayConstant);
      }

      var lengths = [];
      while(match('[')) {
        lex();
        if (!match(']')) {
          var token = lex();
          if (token.type !== Token.NumericLiteral || Math.round(token.value) !== token.value) {
            throwError(token, Messages.ArraySizeIntegral);
          }
          lengths.unshift(token.value);
        }
        expect(']');
      }
      lengths.forEach(function (x) {
        declaredType = {
          type: Syntax.ArrayType,
          base: declaredType,
          length: x
        };
      });

    } else if (match('(')) {
      args = parseArguments();
    }

    // 12.2.1
    if (strict && isRestrictedWord(id.name)) {
      throwError({}, Messages.StrictVarName);
    }


    if (kind === 'const') {
      expect('=');
      init = parseAssignmentExpression();
    } else if (!noAssignment && match('=')) {
      lex();
      init = parseAssignmentExpression();
    }

    return {
      type: Syntax.VariableDeclarator,
      decltype: declaredType,
      arguments: args,
      id: id,
      init: init
    };
  }

  function parseVariableDeclarationList(kind, noAssignment, typeIdentifier) {
    var list = [];

    while (index < length) {
      list.push(parseVariableDeclaration(kind, noAssignment, typeIdentifier));
      if (!match(',')) {
        break;
      }
      lex();
    }

    return list;
  }

  function parseTypeIdentifier(force) {
    var m = mark();
    var token = lex();
    if (token.type !== Token.Identifier && token.type !== Token.Keyword) {
      throwUnexpected(token);
    }

    var next = lookahead();
    if (force || next.type === Token.Identifier || next.value === '*') {
      return {
        type: Syntax.TypeIdentifier,
        name: token.value,
        range: token.range
      };
    }

    reset(m);
  }

  function parseTypeDef() {
    expectKeyword('typedef');

    var original;
    var token = lookahead();
    if (token.type === Token.Keyword && (token.value === "struct" || token.value === "union")) {
      var original = parseStructType();
      var alias = parseTypeIdentifier(true);
    } else {
      var original = parseInlineableType(true);
      var alias = parseTypeIdentifier(true);
      // Do we have a typedef of a function type?
      if (match('(')) {
        lex();
        var paramTypes = [];
        if (!match(')')) {
          while (index < length) {
            paramTypes.push(parseInlineableType(true));
            token = lookahead();
            if (match(')')) {
              break;
            }
            expect(',');
          }
        }
        expect(')');

        original = {
          type: Syntax.ArrowType,
          params: paramTypes,
          return: original,
          range: original.range
        };
      }
    }
    Types[alias.name] = true;

    consumeSemicolon();

    return {
      type: Syntax.TypeAliasDirective,
      range: original.range,
      original: original,
      alias: alias
    };
  }

  function parseVariableStatement(noAssignment) {
    var declarations;

    expectKeyword('var');

    declarations = parseVariableDeclarationList(undefined, noAssignment,
                                                parseTypeIdentifier());

    consumeSemicolon();

    var result = {
      type: Syntax.VariableDeclaration,
      declarations: declarations,
      kind: 'var'
    };

    return result;
  }

  // kind may be `const` or `let` or `extern`
  // Both are experimental and not in the specification yet.
  // see http://wiki.ecmascript.org/doku.php?id=harmony:const
  // and http://wiki.ecmascript.org/doku.php?id=harmony:let
  function parseConstLetExternDeclaration(kind) {
    var declarations;
      
    expectKeyword(kind);

    declarations = parseVariableDeclarationList(
        kind, undefined, parseTypeIdentifier()
    );

    consumeSemicolon();

    return {
      type: Syntax.VariableDeclaration,
      declarations: declarations,
      kind: kind
    };
  }

  // 12.3 Empty Statement

  function parseEmptyStatement() {
    expect(';');

    return {
      type: Syntax.EmptyStatement
    };
  }

  // 12.4 Expression Statement

  function parseExpressionStatement() {
    var expr = parseExpression();

    consumeSemicolon();

    return {
      type: Syntax.ExpressionStatement,
      expression: expr
    };
  }

  // 12.5 If statement

  function parseIfStatement() {
    var test, consequent, alternate;

    expectKeyword('if');

    expect('(');

    test = parseExpression();

    expect(')');

    consequent = parseStatement();

    if (matchKeyword('else')) {
      lex();
      alternate = parseStatement();
    } else {
      alternate = null;
    }

    return {
      type: Syntax.IfStatement,
      test: test,
      consequent: consequent,
      alternate: alternate
    };
  }

  // 12.6 Iteration Statements

  function parseDoWhileStatement() {
    var body, test, oldInIteration;

    expectKeyword('do');

    oldInIteration = state.inIteration;
    state.inIteration = true;

    body = parseStatement();

    state.inIteration = oldInIteration;

    expectKeyword('while');

    expect('(');

    test = parseExpression();

    expect(')');

    if (match(';')) {
      lex();
    }

    return {
      type: Syntax.DoWhileStatement,
      body: body,
      test: test
    };
  }

  function parseWhileStatement() {
    var test, body, oldInIteration;

    expectKeyword('while');

    expect('(');

    test = parseExpression();

    expect(')');

    oldInIteration = state.inIteration;
    state.inIteration = true;

    body = parseStatement();

    state.inIteration = oldInIteration;

    return {
      type: Syntax.WhileStatement,
      test: test,
      body: body
    };
  }

  function parseForVariableDeclaration() {
    var token = lex();
    var typeIdentifier = parseTypeIdentifier();
    var result = {
      type: Syntax.VariableDeclaration,
      declarations: parseVariableDeclarationList(undefined, undefined, typeIdentifier),
      kind: token.value
    };
    return result;
  }

  function parseForStatement() {
    var init, test, update, left, right, body, oldInIteration;

    init = test = update = null;

    expectKeyword('for');

    expect('(');

    if (match(';')) {
      lex();
    } else {
      if (matchKeyword('var') || matchKeyword('let')) {
        state.allowIn = false;
        init = parseForVariableDeclaration();
        state.allowIn = true;
        if (init.declarations.length === 1 && matchKeyword('in')) {
          lex();
          left = init;
          right = parseExpression();
          init = null;
        }
      } else {
        state.allowIn = false;
        init = parseExpression();
        state.allowIn = true;

        if (matchKeyword('in')) {
          // LeftHandSideExpression
          if (matchKeyword('in') && (state.lastParenthesized !== init && !isLeftHandSide(init))) {
            throwError({}, Messages.InvalidLHSInForIn);
          }
          lex();
          left = init;
          right = parseExpression();
          init = null;
        }
      }

      if (typeof left === 'undefined') {
        expect(';');
      }
    }

    if (typeof left === 'undefined') {

      if (!match(';')) {
        test = parseExpression();
      }
      expect(';');

      if (!match(')')) {
        update = parseExpression();
      }
    }

    expect(')');

    oldInIteration = state.inIteration;
    state.inIteration = true;

    body = parseStatement();

    state.inIteration = oldInIteration;

    if (typeof left === 'undefined') {
      return {
        type: Syntax.ForStatement,
        init: init,
        test: test,
        update: update,
        body: body
      };
    }

    return {
      type: Syntax.ForInStatement,
      left: left,
      right: right,
      body: body,
      each: false
    };
  }

  // 12.7 The continue statement

  function parseContinueStatement() {
    var token, label = null;

    expectKeyword('continue');

    // Optimize the most common form: 'continue;'.
    if (source[index] === ';') {
      lex();

      if (!state.inIteration) {
        throwError({}, Messages.IllegalContinue);
      }

      return {
        type: Syntax.ContinueStatement,
        label: null
      };
    }

    if (peekLineTerminator()) {
      if (!state.inIteration) {
        throwError({}, Messages.IllegalContinue);
      }

      return {
        type: Syntax.ContinueStatement,
        label: null
      };
    }

    token = lookahead();
    if (token.type === Token.Identifier) {
      label = parseVariableIdentifier();

      if (!Object.prototype.hasOwnProperty.call(state.labelSet, label.name)) {
        throwError({}, Messages.UnknownLabel, label.name);
      }
    }

    consumeSemicolon();

    if (label === null && !state.inIteration) {
      throwError({}, Messages.IllegalContinue);
    }

    return {
      type: Syntax.ContinueStatement,
      label: label
    };
  }

  // 12.8 The break statement

  function parseBreakStatement() {
    var token, label = null;

    expectKeyword('break');

    // Optimize the most common form: 'break;'.
    if (source[index] === ';') {
      lex();

      if (!(state.inIteration || state.inSwitch)) {
        throwError({}, Messages.IllegalBreak);
      }

      return {
        type: Syntax.BreakStatement,
        label: null
      };
    }

    if (peekLineTerminator()) {
      if (!(state.inIteration || state.inSwitch)) {
        throwError({}, Messages.IllegalBreak);
      }

      return {
        type: Syntax.BreakStatement,
        label: null
      };
    }

    token = lookahead();
    if (token.type === Token.Identifier) {
      label = parseVariableIdentifier();

      if (!Object.prototype.hasOwnProperty.call(state.labelSet, label.name)) {
        throwError({}, Messages.UnknownLabel, label.name);
      }
    }

    consumeSemicolon();

    if (label === null && !(state.inIteration || state.inSwitch)) {
      throwError({}, Messages.IllegalBreak);
    }

    return {
      type: Syntax.BreakStatement,
      label: label
    };
  }

  // 12.9 The return statement

  function parseReturnStatement() {
    var token, argument = null;

    expectKeyword('return');

    if (!state.inFunctionBody) {
      throwErrorTolerant({}, Messages.IllegalReturn);
    }

    // 'return' followed by a space and an identifier is very common.
    if (source[index] === ' ') {
      if (isIdentifierStart(source[index + 1])) {
        argument = parseExpression();
        consumeSemicolon();
        return {
          type: Syntax.ReturnStatement,
          argument: argument
        };
      }
    }

    if (peekLineTerminator()) {
      return {
        type: Syntax.ReturnStatement,
        argument: null
      };
    }

    if (!match(';')) {
      token = lookahead();
      if (!match('}') && token.type !== Token.EOF) {
        argument = parseExpression();
      }
    }

    consumeSemicolon();

    return {
      type: Syntax.ReturnStatement,
      argument: argument
    };
  }

  // 12.10 The with statement

  function parseWithStatement() {
    var object, body;

    if (strict) {
      throwErrorTolerant({}, Messages.StrictModeWith);
    }

    expectKeyword('with');

    expect('(');

    object = parseExpression();

    expect(')');

    body = parseStatement();

    return {
      type: Syntax.WithStatement,
      object: object,
      body: body
    };
  }

  // 12.10 The swith statement

  function parseSwitchCase(test) {
    var consequent = [],
        statement;

    while (index < length) {
      if (match('}') || matchKeyword('default') || matchKeyword('case')) {
        break;
      }
      statement = parseStatement();
      if (typeof statement === 'undefined') {
        break;
      }
      consequent.push(statement);
    }

    return {
      type: Syntax.SwitchCase,
      test: test,
      consequent: consequent
    };
  }

  function parseSwitchStatement() {
    var discriminant, cases, test, oldInSwitch;

    expectKeyword('switch');

    expect('(');

    discriminant = parseExpression();

    expect(')');

    expect('{');

    if (match('}')) {
      lex();
      return {
        type: Syntax.SwitchStatement,
        discriminant: discriminant
      };
    }

    cases = [];

    oldInSwitch = state.inSwitch;
    state.inSwitch = true;

    while (index < length) {
      if (match('}')) {
        break;
      }

      if (matchKeyword('default')) {
        lex();
        test = null;
      } else {
        expectKeyword('case');
        test = parseExpression();
      }
      expect(':');

      cases.push(parseSwitchCase(test));
    }

    state.inSwitch = oldInSwitch;

    expect('}');

    return {
      type: Syntax.SwitchStatement,
      discriminant: discriminant,
      cases: cases
    };
  }

  // 12.13 The throw statement

  function parseThrowStatement() {
    var argument;

    expectKeyword('throw');

    if (peekLineTerminator()) {
      throwError({}, Messages.NewlineAfterThrow);
    }

    argument = parseExpression();

    consumeSemicolon();

    return {
      type: Syntax.ThrowStatement,
      argument: argument
    };
  }

  // 12.14 The try statement

  function parseCatchClause() {
    var param;

    expectKeyword('catch');

    expect('(');
    if (!match(')')) {
      param = parseExpression();
      // 12.14.1
      if (strict && param.type === Syntax.Identifier && isRestrictedWord(param.name)) {
        throwError({}, Messages.StrictCatchVariable);
      }
    }
    expect(')');

    return {
      type: Syntax.CatchClause,
      param: param,
      guard: null,
      body: parseBlock()
    };
  }

  function parseTryStatement() {
    var block, handlers = [], finalizer = null;

    expectKeyword('try');

    block = parseBlock();

    if (matchKeyword('catch')) {
      handlers.push(parseCatchClause());
    }

    if (matchKeyword('finally')) {
      lex();
      finalizer = parseBlock();
    }

    if (handlers.length === 0 && !finalizer) {
      throwError({}, Messages.NoCatchOrFinally);
    }

    return {
      type: Syntax.TryStatement,
      block: block,
      handlers: handlers,
      finalizer: finalizer
    };
  }

  // 12.15 The debugger statement

  function parseDebuggerStatement() {
    expectKeyword('debugger');

    consumeSemicolon();

    return {
      type: Syntax.DebuggerStatement
    };
  }

  // 12 Statements

  function parseStatement() {
    var token = lookahead(),
        expr,
        labeledBody;

    if (token.type === Token.EOF) {
      throwUnexpected(token);
    }

    if (token.type === Token.Punctuator) {
      switch (token.value) {
      case ';':
        return parseEmptyStatement();
      case '{':
        return parseBlock();
      case '(':
        return parseExpressionStatement();
      default:
        break;
      }
    }

    if (token.type === Token.Keyword) {
      switch (token.value) {
      case 'break':
        return parseBreakStatement();
      case 'continue':
        return parseContinueStatement();
      case 'debugger':
        return parseDebuggerStatement();
      case 'do':
        return parseDoWhileStatement();
      case 'for':
        return parseForStatement();
      case 'function':
        return parseFunctionDeclaration();
      case 'if':
        return parseIfStatement();
      case 'return':
        return parseReturnStatement();
      case 'switch':
        return parseSwitchStatement();
      case 'throw':
        return parseThrowStatement();
      case 'try':
        return parseTryStatement();
      case 'var':
        return parseVariableStatement();
      case 'while':
        return parseWhileStatement();
      case 'with':
        return parseWithStatement();
      case 'import':
        return parseImportStatement();
      default:
        break;
      }
    }

    expr = parseExpression();

    // 12.12 Labelled Statements
    if ((expr.type === Syntax.Identifier) && match(':')) {
      lex();

      if (Object.prototype.hasOwnProperty.call(state.labelSet, expr.name)) {
        throwError({}, Messages.Redeclaration, 'Label', expr.name);
      }

      state.labelSet[expr.name] = true;
      labeledBody = parseStatement();
      delete state.labelSet[expr.name];

      return {
        type: Syntax.LabeledStatement,
        label: expr,
        body: labeledBody
      };
    }

    consumeSemicolon();

    return {
      type: Syntax.ExpressionStatement,
      expression: expr
    };
  }

  // 13 Function Definition

  function parseFunctionSourceElements() {
    var sourceElement, sourceElements = [], token, directive, firstRestricted,
        oldLabelSet, oldInIteration, oldInSwitch, oldInFunctionBody;

    expect('{');

    while (index < length) {
      token = lookahead();
      if (token.type !== Token.StringLiteral) {
        break;
      }

      sourceElement = parseSourceElement();
      sourceElements.push(sourceElement);
      if (sourceElement.expression.type !== Syntax.Literal) {
        // this is not directive
        break;
      }
      directive = sliceSource(token.range[0] + 1, token.range[1] - 1);
      if (directive === 'use strict') {
        strict = true;
        if (firstRestricted) {
          throwError(firstRestricted, Messages.StrictOctalLiteral);
        }
      } else {
        if (!firstRestricted && token.octal) {
          firstRestricted = token;
        }
      }
    }

    oldLabelSet = state.labelSet;
    oldInIteration = state.inIteration;
    oldInSwitch = state.inSwitch;
    oldInFunctionBody = state.inFunctionBody;

    state.labelSet = {};
    state.inIteration = false;
    state.inSwitch = false;
    state.inFunctionBody = true;

    while (index < length) {
      if (match('}')) {
        break;
      }
      sourceElement = parseSourceElement();
      if (typeof sourceElement === 'undefined') {
        break;
      }
      sourceElements.push(sourceElement);
    }

    expect('}');

    state.labelSet = oldLabelSet;
    state.inIteration = oldInIteration;
    state.inSwitch = oldInSwitch;
    state.inFunctionBody = oldInFunctionBody;

    return {
      type: Syntax.BlockStatement,
      body: sourceElements
    };
  }

  function parseStructDeclaration() {
    var type = parseStructType();
    consumeSemicolon();
    if (type.id) {
      return {
        type: Syntax.TypeAliasDirective,
        range: type.range,
        original: type,
        alias: {
          type: Syntax.TypeIdentifier,
          name: type.id.name,
          range: type.range
        }
      };
    }

    return parseStatement();
  }

  function parseModifiers() {
    var modifiers = [];
    while (matchKeyword('public') || matchKeyword('static') ||
           matchKeyword('private') || matchKeyword('const')) {
      modifiers.push(lookahead().value);
      lex();
    }
    return modifiers;
  }

  function parseStructType() {
    var isUnion = false;
    if (matchKeyword('union')) {
      lex();
      isUnion = true;
    } else {
      expectKeyword('struct');
    }

    if (!match('{')) {
      var id = parseTypeIdentifier(true);
      Types[id.name] = true;
    }
    var statement;
    var members = [];

    expect('{');
    while (index < length) {
      if (match('}')) {
        break;
      }
      var modifiers = parseModifiers();
      var list = [];
      if (matchKeyword("function")) {
        list.push(parseFunctionDeclaration());
      } else if (matchKeyword("struct") || matchKeyword("union")) {
        list.push.apply(list, parseVariableDeclarationList(undefined, true,
                                                           parseStructType()));
      } else {
        list.push.apply(list, parseVariableDeclarationList(undefined, true,
                                                           parseTypeIdentifier()));
      }
      members.push.apply(members, list.map(function (x) {
        return {
          type: Syntax.MemberDeclarator,
          declarator: x,
          modifiers: modifiers
        };
      }));
      consumeSemicolon();
    }
    expect('}');
    return {
      type: Syntax.StructType,
      id: id,
      members: members,
      isUnion: isUnion
    };
  }

  function parseFunctionDeclaration() {
    var id, param, paramType, params = [], body, token, firstRestricted, message, previousStrict, paramSet, paramTypes, returnType;

    var start = index;
    expectKeyword('function');
    returnType = parseInlineableType();
    token = lookahead();
    id = parseVariableIdentifier();
    if (strict) {
      if (isRestrictedWord(token.value)) {
        throwError(token, Messages.StrictFunctionName);
      }
    } else {
      if (isRestrictedWord(token.value)) {
        firstRestricted = token;
        message = Messages.StrictFunctionName;
      } else if (isStrictModeReservedWord(token.value)) {
        firstRestricted = token;
        message = Messages.StrictReservedWord;
      }
    }

    expect('(');
    paramTypes = [];

    if (!match(')')) {
      paramSet = {};
      while (index < length) {
        paramTypes.push(parseInlineableType());
        token = lookahead();
        param = parseVariableIdentifier();
        if (strict) {
          if (isRestrictedWord(token.value)) {
            throwError(token, Messages.StrictParamName);
          }
          if (Object.prototype.hasOwnProperty.call(paramSet, token.value)) {
            throwError(token, Messages.StrictParamDupe);
          }
        } else if (!firstRestricted) {
          if (isRestrictedWord(token.value)) {
            firstRestricted = token;
            message = Messages.StrictParamName;
          } else if (isStrictModeReservedWord(token.value)) {
            firstRestricted = token;
            message = Messages.StrictReservedWord;
          } else if (Object.prototype.hasOwnProperty.call(paramSet, token.value)) {
            firstRestricted = token;
            message = Messages.StrictParamDupe;
          }
        }
        params.push(param);
        paramSet[param.name] = true;
        if (match(')')) {
          break;
        }
        expect(',');
      }
    }

    expect(')');

    previousStrict = strict;
    body = parseFunctionSourceElements();
    if (strict && firstRestricted) {
      throwError(firstRestricted, message);
    }
    strict = previousStrict;

    for (var i = 0, j = paramTypes.length; i < j; i++) {
      if (!paramTypes[i]) {
        paramTypes[i] = {
          type: Syntax.TypeIdentifier,
          name: "dyn",
          range: params[i].range
        };
      }
    }
    if (!returnType) {
      returnType = {
        type: Syntax.TypeIdentifier,
        name: "dyn",
        range: [start, start]
      }
    }

    return {
      type: Syntax.FunctionDeclaration,
      id: id,
      decltype: {
        type: Syntax.ArrowType,
        params: paramTypes,
        return: returnType,
        range: returnType.range
      },
      params: params,
      body: body
    };
  }

  function parseFunctionExpression() {
    var token, id = null, firstRestricted, message, param, paramType, params = [], body, previousStrict, paramSet, paramTypes, returnType;

    var start = index;
    expectKeyword('function');
    if (!match('(')) {
      returnType = parseInlineableType();
      token = lookahead();
      id = parseVariableIdentifier();
      if (strict) {
        if (isRestrictedWord(token.value)) {
          throwError(token, Messages.StrictFunctionName);
        }
      } else {
        if (isRestrictedWord(token.value)) {
          firstRestricted = token;
          message = Messages.StrictFunctionName;
        } else if (isStrictModeReservedWord(token.value)) {
          firstRestricted = token;
          message = Messages.StrictReservedWord;
        }
      }
    }

    expect('(');
    paramTypes = [];

    if (!match(')')) {
      paramSet = {};
      while (index < length) {
        paramTypes.push(parseInlineableType());
        token = lookahead();
        param = parseVariableIdentifier();
        if (strict) {
          if (isRestrictedWord(token.value)) {
            throwError(token, Messages.StrictParamName);
          }
          if (Object.prototype.hasOwnProperty.call(paramSet, token.value)) {
            throwError(token, Messages.StrictParamDupe);
          }
        } else if (!firstRestricted) {
          if (isRestrictedWord(token.value)) {
            firstRestricted = token;
            message = Messages.StrictParamName;
          } else if (isStrictModeReservedWord(token.value)) {
            firstRestricted = token;
            message = Messages.StrictReservedWord;
          } else if (Object.prototype.hasOwnProperty.call(paramSet, token.value)) {
            firstRestricted = token;
            message = Messages.StrictParamDupe;
          }
        }
        params.push(param);
        paramSet[param.name] = true;
        if (match(')')) {
          break;
        }
        expect(',');
      }
    }

    expect(')');

    previousStrict = strict;
    body = parseFunctionSourceElements();
    if (strict && firstRestricted) {
      throwError(firstRestricted, message);
    }
    strict = previousStrict;

    for (var i = 0, j = paramTypes.length; i < j; i++) {
      if (!paramTypes[i]) {
        paramTypes[i] = {
          type: Syntax.TypeIdentifier,
          name: "dyn",
          range: params[i].range
        };
      }
    }
    if (!returnType) {
      returnType = {
        type: Syntax.TypeIdentifier,
        name: "dyn",
        range: [start, start]
      }
    }

    return {
      type: Syntax.FunctionExpression,
      id: id,
      decltype: {
        type: Syntax.ArrowType,
        params: paramTypes,
        return: returnType,
        range: returnType.range
      },
      params: params,
      body: body
    };
  }

  // 14 Program

  function parseSourceElement(inToplevel) {
    var token = lookahead();
    if (token.type === Token.Keyword) {
      switch (token.value) {
      case 'const':
      case 'let':
      case 'extern':
        return parseConstLetExternDeclaration(token.value);
      case 'function':
        return parseFunctionDeclaration();
      case 'struct':
      case 'union':
        if (inToplevel) {
          return parseStructDeclaration();
        }
        // FALLTHROUGH
      case 'typedef':
        if (inToplevel) {
          return parseTypeDef();
        }
        // FALLTHROUGH
      default:
        return parseStatement();
      }
    }

    if (token.type !== Token.EOF) {
      return parseStatement();
    }
  }

  function parseSourceElements() {
    var sourceElement, sourceElements = [], token, directive, firstRestricted;

    while (index < length) {
      token = lookahead();
      if (token.type !== Token.StringLiteral) {
        break;
      }

      sourceElement = parseSourceElement(true);
      sourceElements.push(sourceElement);
      if (sourceElement.expression.type !== Syntax.Literal) {
        // this is not directive
        break;
      }
      directive = sliceSource(token.range[0] + 1, token.range[1] - 1);
      if (directive === 'use strict') {
        strict = true;
        if (firstRestricted) {
          throwError(firstRestricted, Messages.StrictOctalLiteral);
        }
      } else {
        if (!firstRestricted && token.octal) {
          firstRestricted = token;
        }
      }
    }

    while (index < length) {
      sourceElement = parseSourceElement(true);
      if (typeof sourceElement === 'undefined') {
        break;
      }
      sourceElements.push(sourceElement);
    }
    return sourceElements;
  }

  function parseProgram() {
    var program;
    strict = false;
    program = {
      type: Syntax.Program,
      body: parseSourceElements()
    };
    return program;
  }

  // The following functions are needed only when the option to preserve
  // the comments is active.

  function addComment(start, end, type, value) {
    assert(typeof start === 'number', 'Comment must have valid position');

    // Because the way the actual token is scanned, often the comments
    // (if any) are skipped twice during the lexical analysis.
    // Thus, we need to skip adding a comment if the comment array already
    // handled it.
    if (extra.comments.length > 0) {
      if (extra.comments[extra.comments.length - 1].range[1] > start) {
        return;
      }
    }

    extra.comments.push({
      range: [start, end],
      type: type,
      value: value
    });
  }

  function scanComment() {
    var comment, ch, start, blockComment, lineComment;

    comment = '';
    blockComment = false;
    lineComment = false;

    while (index < length) {
      ch = source[index];

      if (lineComment) {
        ch = nextChar();
        if (index >= length) {
          lineComment = false;
          comment += ch;
          addComment(start, index, 'Line', comment);
        } else if (isLineTerminator(ch)) {
          lineComment = false;
          addComment(start, index, 'Line', comment);
          if (ch === '\r' && source[index] === '\n') {
            ++index;
          }
          ++lineNumber;
          lineStart = index;
          comment = '';
        } else {
          comment += ch;
        }
      } else if (blockComment) {
        if (isLineTerminator(ch)) {
          if (ch === '\r' && source[index + 1] === '\n') {
            ++index;
            comment += '\r\n';
          } else {
            comment += ch;
          }
          ++lineNumber;
          ++index;
          lineStart = index;
          if (index >= length) {
            throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
          }
        } else {
          ch = nextChar();
          if (index >= length) {
            throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
          }
          comment += ch;
          if (ch === '*') {
            ch = source[index];
            if (ch === '/') {
              comment = comment.substr(0, comment.length - 1);
              blockComment = false;
              ++index;
              addComment(start, index, 'Block', comment);
              comment = '';
            }
          }
        }
      } else if (ch === '/') {
        ch = source[index + 1];
        if (ch === '/') {
          start = index;
          index += 2;
          lineComment = true;
        } else if (ch === '*') {
          start = index;
          index += 2;
          blockComment = true;
          if (index >= length) {
            throwError({}, Messages.UnexpectedToken, 'ILLEGAL');
          }
        } else {
          break;
        }
      } else if (isWhiteSpace(ch)) {
        ++index;
      } else if (isLineTerminator(ch)) {
        ++index;
        if (ch ===  '\r' && source[index] === '\n') {
          ++index;
        }
        ++lineNumber;
        lineStart = index;
      } else {
        break;
      }
    }
  }

  function collectToken() {
    var token = extra.advance(),
        range,
        value;

    if (token.type !== Token.EOF) {
      range = [token.range[0], token.range[1]];
      value = sliceSource(token.range[0], token.range[1]);
      extra.tokens.push({
        type: TokenName[token.type],
        value: value,
        range: range
      });
    }

    return token;
  }

  function collectRegex() {
    var pos, regex, token;

    skipComment();

    pos = index;
    regex = extra.scanRegExp();

    // Pop the previous token, which is likely '/' or '/='
    if (extra.tokens.length > 0) {
      token = extra.tokens[extra.tokens.length - 1];
      if (token.range[0] === pos && token.type === 'Punctuator') {
        if (token.value === '/' || token.value === '/=') {
          extra.tokens.pop();
        }
      }
    }

    extra.tokens.push({
      type: 'RegularExpression',
      value: regex.literal,
      range: [pos, index]
    });

    return regex;
  }

  function createLiteral(token) {
    return {
      type: Syntax.Literal,
      value: token.value
    };
  }

  function createRawLiteral(token) {
    return {
      type: Syntax.Literal,
      value: token.value,
      raw: sliceSource(token.range[0], token.range[1])
    };
  }

  function wrapTrackingFunction(range, loc) {

    return function (parseFunction) {

      function isBinary(node) {
        return node.type === Syntax.LogicalExpression ||
          node.type === Syntax.BinaryExpression;
      }

      function visit(node) {
        if (isBinary(node.left)) {
          visit(node.left);
        }
        if (isBinary(node.right)) {
          visit(node.right);
        }

        if (range && typeof node.range === 'undefined') {
          node.range = [node.left.range[0], node.right.range[1]];
        }
        if (loc && typeof node.loc === 'undefined') {
          node.loc = {
            start: node.left.loc.start,
            end: node.right.loc.end
          };
        }
      }

      return function () {
        var node, rangeInfo, locInfo;

        skipComment();
        rangeInfo = [index, 0];
        locInfo = {
          start: {
            line: lineNumber,
            column: index - lineStart
          }
        };

        node = parseFunction.apply(null, arguments);
        if (typeof node !== 'undefined') {

          if (range) {
            rangeInfo[1] = index;
            node.range = rangeInfo;
          }

          if (loc) {
            locInfo.end = {
              line: lineNumber,
              column: index - lineStart
            };
            node.loc = locInfo;
          }

          if (isBinary(node)) {
            visit(node);
          }

          if (node.type === Syntax.MemberExpression) {
            if (typeof node.object.range !== 'undefined') {
              node.range[0] = node.object.range[0];
            }
            if (typeof node.object.loc !== 'undefined') {
              node.loc.start = node.object.loc.start;
            }
          }
          return node;
        }
      };

    };
  }

  function patch() {

    var wrapTracking;

    if (extra.comments) {
      extra.skipComment = skipComment;
      skipComment = scanComment;
    }

    if (extra.raw) {
      extra.createLiteral = createLiteral;
      createLiteral = createRawLiteral;
    }

    if (extra.range || extra.loc) {

      wrapTracking = wrapTrackingFunction(extra.range, extra.loc);

      extra.parseAdditiveExpression = parseAdditiveExpression;
      extra.parseAssignmentExpression = parseAssignmentExpression;
      extra.parseBitwiseANDExpression = parseBitwiseANDExpression;
      extra.parseBitwiseORExpression = parseBitwiseORExpression;
      extra.parseBitwiseXORExpression = parseBitwiseXORExpression;
      extra.parseBlock = parseBlock;
      extra.parseFunctionSourceElements = parseFunctionSourceElements;
      extra.parseCallMember = parseCallMember;
      extra.parseCatchClause = parseCatchClause;
      extra.parseComputedMember = parseComputedMember;
      extra.parseConditionalExpression = parseConditionalExpression;
      extra.parseConstLetExternDeclaration = parseConstLetExternDeclaration;
      extra.parseEqualityExpression = parseEqualityExpression;
      extra.parseExpression = parseExpression;
      extra.parseForVariableDeclaration = parseForVariableDeclaration;
      extra.parseFunctionDeclaration = parseFunctionDeclaration;
      extra.parseFunctionExpression = parseFunctionExpression;
      extra.parseLogicalANDExpression = parseLogicalANDExpression;
      extra.parseLogicalORExpression = parseLogicalORExpression;
      extra.parseMultiplicativeExpression = parseMultiplicativeExpression;
      extra.parseNewExpression = parseNewExpression;
      extra.parseNonComputedMember = parseNonComputedMember;
      extra.parseNonComputedProperty = parseNonComputedProperty;
      extra.parseObjectProperty = parseObjectProperty;
      extra.parseObjectPropertyKey = parseObjectPropertyKey;
      extra.parsePostfixExpression = parsePostfixExpression;
      extra.parsePrimaryExpression = parsePrimaryExpression;
      extra.parseProgram = parseProgram;
      extra.parsePropertyFunction = parsePropertyFunction;
      extra.parseRelationalExpression = parseRelationalExpression;
      extra.parseStatement = parseStatement;
      extra.parseShiftExpression = parseShiftExpression;
      extra.parseSwitchCase = parseSwitchCase;
      extra.parseUnaryExpression = parseUnaryExpression;
      extra.parseVariableDeclaration = parseVariableDeclaration;
      extra.parseVariableIdentifier = parseVariableIdentifier;

      parseAdditiveExpression = wrapTracking(extra.parseAdditiveExpression);
      parseAssignmentExpression = wrapTracking(extra.parseAssignmentExpression);
      parseBitwiseANDExpression = wrapTracking(extra.parseBitwiseANDExpression);
      parseBitwiseORExpression = wrapTracking(extra.parseBitwiseORExpression);
      parseBitwiseXORExpression = wrapTracking(extra.parseBitwiseXORExpression);
      parseBlock = wrapTracking(extra.parseBlock);
      parseFunctionSourceElements = wrapTracking(extra.parseFunctionSourceElements);
      parseCallMember = wrapTracking(extra.parseCallMember);
      parseCatchClause = wrapTracking(extra.parseCatchClause);
      parseComputedMember = wrapTracking(extra.parseComputedMember);
      parseConditionalExpression = wrapTracking(extra.parseConditionalExpression);
      parseConstLetExternDeclaration = wrapTracking(extra.parseConstLetExternDeclaration);
      parseEqualityExpression = wrapTracking(extra.parseEqualityExpression);
      parseExpression = wrapTracking(extra.parseExpression);
      parseForVariableDeclaration = wrapTracking(extra.parseForVariableDeclaration);
      parseFunctionDeclaration = wrapTracking(extra.parseFunctionDeclaration);
      parseFunctionExpression = wrapTracking(extra.parseFunctionExpression);
      parseLogicalANDExpression = wrapTracking(extra.parseLogicalANDExpression);
      parseLogicalORExpression = wrapTracking(extra.parseLogicalORExpression);
      parseMultiplicativeExpression = wrapTracking(extra.parseMultiplicativeExpression);
      parseNewExpression = wrapTracking(extra.parseNewExpression);
      parseNonComputedMember = wrapTracking(extra.parseNonComputedMember);
      parseNonComputedProperty = wrapTracking(extra.parseNonComputedProperty);
      parseObjectProperty = wrapTracking(extra.parseObjectProperty);
      parseObjectPropertyKey = wrapTracking(extra.parseObjectPropertyKey);
      parsePostfixExpression = wrapTracking(extra.parsePostfixExpression);
      parsePrimaryExpression = wrapTracking(extra.parsePrimaryExpression);
      parseProgram = wrapTracking(extra.parseProgram);
      parsePropertyFunction = wrapTracking(extra.parsePropertyFunction);
      parseRelationalExpression = wrapTracking(extra.parseRelationalExpression);
      parseStatement = wrapTracking(extra.parseStatement);
      parseShiftExpression = wrapTracking(extra.parseShiftExpression);
      parseSwitchCase = wrapTracking(extra.parseSwitchCase);
      parseUnaryExpression = wrapTracking(extra.parseUnaryExpression);
      parseVariableDeclaration = wrapTracking(extra.parseVariableDeclaration);
      parseVariableIdentifier = wrapTracking(extra.parseVariableIdentifier);

      extra.parsePointerType = parsePointerType;
      extra.parseStructType = parseStructType;
      extra.parseTypeIdentifier = parseTypeIdentifier;
      extra.parseTypeDef = parseTypeDef;

      parsePointerType = wrapTracking(extra.parsePointerType);
      parseStructType = wrapTracking(extra.parseStructType);
      parseTypeIdentifier = wrapTracking(extra.parseTypeIdentifier);
      parseTypeDef = wrapTracking(parseTypeDef);
    }

    if (typeof extra.tokens !== 'undefined') {
      extra.advance = advance;
      extra.scanRegExp = scanRegExp;

      advance = collectToken;
      scanRegExp = collectRegex;
    }
  }

  function unpatch() {
    if (typeof extra.skipComment === 'function') {
      skipComment = extra.skipComment;
    }

    if (extra.raw) {
      createLiteral = extra.createLiteral;
    }

    if (extra.range || extra.loc) {
      parseAdditiveExpression = extra.parseAdditiveExpression;
      parseAssignmentExpression = extra.parseAssignmentExpression;
      parseBitwiseANDExpression = extra.parseBitwiseANDExpression;
      parseBitwiseORExpression = extra.parseBitwiseORExpression;
      parseBitwiseXORExpression = extra.parseBitwiseXORExpression;
      parseBlock = extra.parseBlock;
      parseFunctionSourceElements = extra.parseFunctionSourceElements;
      parseCallMember = extra.parseCallMember;
      parseCatchClause = extra.parseCatchClause;
      parseComputedMember = extra.parseComputedMember;
      parseConditionalExpression = extra.parseConditionalExpression;
      parseConstLetExternDeclaration = extra.parseConstLetExternDeclaration;
      parseEqualityExpression = extra.parseEqualityExpression;
      parseExpression = extra.parseExpression;
      parseForVariableDeclaration = extra.parseForVariableDeclaration;
      parseFunctionDeclaration = extra.parseFunctionDeclaration;
      parseFunctionExpression = extra.parseFunctionExpression;
      parseLogicalANDExpression = extra.parseLogicalANDExpression;
      parseLogicalORExpression = extra.parseLogicalORExpression;
      parseMultiplicativeExpression = extra.parseMultiplicativeExpression;
      parseNewExpression = extra.parseNewExpression;
      parseNonComputedMember = extra.parseNonComputedMember;
      parseNonComputedProperty = extra.parseNonComputedProperty;
      parseObjectProperty = extra.parseObjectProperty;
      parseObjectPropertyKey = extra.parseObjectPropertyKey;
      parsePrimaryExpression = extra.parsePrimaryExpression;
      parsePostfixExpression = extra.parsePostfixExpression;
      parseProgram = extra.parseProgram;
      parsePropertyFunction = extra.parsePropertyFunction;
      parseRelationalExpression = extra.parseRelationalExpression;
      parseStatement = extra.parseStatement;
      parseShiftExpression = extra.parseShiftExpression;
      parseSwitchCase = extra.parseSwitchCase;
      parseUnaryExpression = extra.parseUnaryExpression;
      parseVariableDeclaration = extra.parseVariableDeclaration;
      parseVariableIdentifier = extra.parseVariableIdentifier;

      parsePointerType = extra.parsePointerType;
      parseStructType = extra.parseStructType;
      parseTypeIdentifier = extra.parseTypeIdentifier;
      parseTypeDef = extra.parseTypeDef;
    }

    if (typeof extra.scanRegExp === 'function') {
      advance = extra.advance;
      scanRegExp = extra.scanRegExp;
    }
  }

  // LLJS
  function parseImportStatement() {
    var imports = [];

    expectKeyword('import');
    expect('{');

    while (index < length) {
      imports.push(parseVariableIdentifier());
      if(!match(',')) {
        break;
      }
      lex();
    }

    expect('}');
    expectKeyword('from');

    var from = lex();
    if(from.type != Token.StringLiteral) {
      throwError(from, Messages.UnexpectedToken, 'ILLEGAL');
    }

    consumeSemicolon();

    return {
      type: Syntax.ImportExpression,
      imports: imports,
      from: from
    };
  }

  function stringToArray(str) {
    var length = str.length,
        result = [],
        i;
    for (i = 0; i < length; ++i) {
      result[i] = str.charAt(i);
    }
    return result;
  }

  function disambiguateCast(call) {
    var callee, args, as, length;

    callee = call.callee;
    if (!(args = call.arguments)) {
      return;
    }
    length = args.length;
    if (!(as = toType(callee))) {
      return;
    }

    call.type = Syntax.CastExpression;
    call.as = as;
    if (length === 0) {
      throwError({}, Messages.CastingNothing);
    } else if (length === 1) {
      call.argument = args[0];
    } else {
      call.argument = {
        type: Syntax.SequenceExpression,
        expressions: args
      };
    }

    delete call.callee;
    delete call.arguments;
  }

  function parse(code, options) {
    var program, toString, i, j;

    Types = {
      i8: true,
      u8: true,
      i16: true,
      u16: true,
      i32: true,
      u32: true,
      int: true,
      uint: true,
      bool: true,
      void: true,
      num: true,
      float: true,
      double: true,
      dyn: true
    };

    toString = String;
    if (typeof code !== 'string' && !(code instanceof String)) {
      code = toString(code);
    }

    source = code;
    index = 0;
    lineNumber = (source.length > 0) ? 1 : 0;
    lineStart = 0;
    length = source.length;
    buffer = null;
    state = {
      allowIn: true,
      labelSet: {},
      lastParenthesized: null,
      inFunctionBody: false,
      inIteration: false,
      inSwitch: false
    };

    extra = {};
    if (typeof options !== 'undefined') {
      extra.range = (typeof options.range === 'boolean') && options.range;
      extra.loc = (typeof options.loc === 'boolean') && options.loc;
      extra.raw = (typeof options.raw === 'boolean') && options.raw;
      if (typeof options.tokens === 'boolean' && options.tokens) {
        extra.tokens = [];
      }
      if (typeof options.comment === 'boolean' && options.comment) {
        extra.comments = [];
      }
      if (typeof options.tolerant === 'boolean' && options.tolerant) {
        extra.errors = [];
      }
    }

    if (length > 0) {
      if (typeof source[0] === 'undefined') {
        // Try first to convert to a string. This is good as fast path
        // for old IE which understands string indexing for string
        // literals only and not for string object.
        if (code instanceof String) {
          source = code.valueOf();
        }

        // Force accessing the characters via an array.
        if (typeof source[0] === 'undefined') {
          source = stringToArray(code);
        }
      }
    }

    patch();
    try {
      program = parseProgram();
      if (typeof extra.comments !== 'undefined') {
        program.comments = extra.comments;
      }
      if (typeof extra.tokens !== 'undefined') {
        program.tokens = extra.tokens;
      }
      if (typeof extra.errors !== 'undefined') {
        program.errors = extra.errors;
      }
    } catch (e) {
      throw e;
    } finally {
      unpatch();
      extra = {};
    }

    for (i = 0, j = maybeCasts.length; i < j; i++) {
      disambiguateCast(maybeCasts[i]);
    }

    return program;
  }

  // Sync with package.json.
  exports.version = '1.0.0-dev';

  exports.parse = parse;

}(typeof exports === 'undefined' ? (esprima = {}) : exports));
/* vim: set sw=4 ts=4 et tw=80 : */
/*
  Copyright (C) 2012 Robert Gust-Bardon <donate@robert.gust-bardon.org>
  Copyright (C) 2012 John Freeman <jfreeman08@gmail.com>
  Copyright (C) 2012 Ariya Hidayat <ariya.hidayat@gmail.com>
  Copyright (C) 2012 Mathias Bynens <mathias@qiwi.be>
  Copyright (C) 2012 Joost-Wim Boekesteijn <joost-wim@boekesteijn.nl>
  Copyright (C) 2012 Kris Kowal <kris.kowal@cixar.com>
  Copyright (C) 2012 Yusuke Suzuki <utatane.tea@gmail.com>
  Copyright (C) 2012 Arpad Borsos <arpad.borsos@googlemail.com>
  Copyright (C) 2011 Ariya Hidayat <ariya.hidayat@gmail.com>

  Redistribution and use in source and binary forms, with or without
  modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
  AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
  IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
  ARE DISCLAIMED. IN NO EVENT SHALL <COPYRIGHT HOLDER> BE LIABLE FOR ANY
  DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
  (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
  LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
  ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/*jslint bitwise:true */
/*global escodegen:true, exports:true, generateStatement: true*/

(function (exports) {
    'use strict';

    var Syntax,
        Precedence,
        BinaryPrecedence,
        Regex,
        VisitorKeys,
        VisitorOption,
        isArray,
        base,
        indent,
        json,
        renumber,
        hexadecimal,
        quotes,
        escapeless,
        newline,
        space,
        parentheses,
        semicolons,
        extra,
        parse;

    Syntax = {
        AssignmentExpression: 'AssignmentExpression',
        ArrayExpression: 'ArrayExpression',
        BlockStatement: 'BlockStatement',
        BinaryExpression: 'BinaryExpression',
        BreakStatement: 'BreakStatement',
        CallExpression: 'CallExpression',
        CatchClause: 'CatchClause',
        ConditionalExpression: 'ConditionalExpression',
        ContinueStatement: 'ContinueStatement',
        DoWhileStatement: 'DoWhileStatement',
        DebuggerStatement: 'DebuggerStatement',
        EmptyStatement: 'EmptyStatement',
        ExpressionStatement: 'ExpressionStatement',
        ForStatement: 'ForStatement',
        ForInStatement: 'ForInStatement',
        FunctionDeclaration: 'FunctionDeclaration',
        FunctionExpression: 'FunctionExpression',
        Identifier: 'Identifier',
        IfStatement: 'IfStatement',
        Literal: 'Literal',
        LabeledStatement: 'LabeledStatement',
        LogicalExpression: 'LogicalExpression',
        MemberExpression: 'MemberExpression',
        NewExpression: 'NewExpression',
        ObjectExpression: 'ObjectExpression',
        Program: 'Program',
        Property: 'Property',
        ReturnStatement: 'ReturnStatement',
        SequenceExpression: 'SequenceExpression',
        SwitchStatement: 'SwitchStatement',
        SwitchCase: 'SwitchCase',
        ThisExpression: 'ThisExpression',
        ThrowStatement: 'ThrowStatement',
        TryStatement: 'TryStatement',
        UnaryExpression: 'UnaryExpression',
        UpdateExpression: 'UpdateExpression',
        VariableDeclaration: 'VariableDeclaration',
        VariableDeclarator: 'VariableDeclarator',
        WhileStatement: 'WhileStatement',
        WithStatement: 'WithStatement'
    };

    Precedence = {
        Sequence: 0,
        Assignment: 1,
        Conditional: 2,
        LogicalOR: 3,
        LogicalAND: 4,
        BitwiseOR: 5,
        BitwiseXOR: 6,
        BitwiseAND: 7,
        Equality: 8,
        Relational: 9,
        BitwiseSHIFT: 10,
        Additive: 11,
        Multiplicative: 12,
        Unary: 13,
        Postfix: 14,
        Call: 15,
        New: 16,
        Member: 17,
        Primary: 18
    };

    BinaryPrecedence = {
        '||': Precedence.LogicalOR,
        '&&': Precedence.LogicalAND,
        '|': Precedence.BitwiseOR,
        '^': Precedence.BitwiseXOR,
        '&': Precedence.BitwiseAND,
        '==': Precedence.Equality,
        '!=': Precedence.Equality,
        '===': Precedence.Equality,
        '!==': Precedence.Equality,
        '<': Precedence.Relational,
        '>': Precedence.Relational,
        '<=': Precedence.Relational,
        '>=': Precedence.Relational,
        'in': Precedence.Relational,
        'instanceof': Precedence.Relational,
        '<<': Precedence.BitwiseSHIFT,
        '>>': Precedence.BitwiseSHIFT,
        '>>>': Precedence.BitwiseSHIFT,
        '+': Precedence.Additive,
        '-': Precedence.Additive,
        '*': Precedence.Multiplicative,
        '%': Precedence.Multiplicative,
        '/': Precedence.Multiplicative
    };

    Regex = {
        NonAsciiIdentifierPart: new RegExp('[\xaa\xb5\xba\xc0-\xd6\xd8-\xf6\xf8-\u02c1\u02c6-\u02d1\u02e0-\u02e4\u02ec\u02ee\u0300-\u0374\u0376\u0377\u037a-\u037d\u0386\u0388-\u038a\u038c\u038e-\u03a1\u03a3-\u03f5\u03f7-\u0481\u0483-\u0487\u048a-\u0527\u0531-\u0556\u0559\u0561-\u0587\u0591-\u05bd\u05bf\u05c1\u05c2\u05c4\u05c5\u05c7\u05d0-\u05ea\u05f0-\u05f2\u0610-\u061a\u0620-\u0669\u066e-\u06d3\u06d5-\u06dc\u06df-\u06e8\u06ea-\u06fc\u06ff\u0710-\u074a\u074d-\u07b1\u07c0-\u07f5\u07fa\u0800-\u082d\u0840-\u085b\u08a0\u08a2-\u08ac\u08e4-\u08fe\u0900-\u0963\u0966-\u096f\u0971-\u0977\u0979-\u097f\u0981-\u0983\u0985-\u098c\u098f\u0990\u0993-\u09a8\u09aa-\u09b0\u09b2\u09b6-\u09b9\u09bc-\u09c4\u09c7\u09c8\u09cb-\u09ce\u09d7\u09dc\u09dd\u09df-\u09e3\u09e6-\u09f1\u0a01-\u0a03\u0a05-\u0a0a\u0a0f\u0a10\u0a13-\u0a28\u0a2a-\u0a30\u0a32\u0a33\u0a35\u0a36\u0a38\u0a39\u0a3c\u0a3e-\u0a42\u0a47\u0a48\u0a4b-\u0a4d\u0a51\u0a59-\u0a5c\u0a5e\u0a66-\u0a75\u0a81-\u0a83\u0a85-\u0a8d\u0a8f-\u0a91\u0a93-\u0aa8\u0aaa-\u0ab0\u0ab2\u0ab3\u0ab5-\u0ab9\u0abc-\u0ac5\u0ac7-\u0ac9\u0acb-\u0acd\u0ad0\u0ae0-\u0ae3\u0ae6-\u0aef\u0b01-\u0b03\u0b05-\u0b0c\u0b0f\u0b10\u0b13-\u0b28\u0b2a-\u0b30\u0b32\u0b33\u0b35-\u0b39\u0b3c-\u0b44\u0b47\u0b48\u0b4b-\u0b4d\u0b56\u0b57\u0b5c\u0b5d\u0b5f-\u0b63\u0b66-\u0b6f\u0b71\u0b82\u0b83\u0b85-\u0b8a\u0b8e-\u0b90\u0b92-\u0b95\u0b99\u0b9a\u0b9c\u0b9e\u0b9f\u0ba3\u0ba4\u0ba8-\u0baa\u0bae-\u0bb9\u0bbe-\u0bc2\u0bc6-\u0bc8\u0bca-\u0bcd\u0bd0\u0bd7\u0be6-\u0bef\u0c01-\u0c03\u0c05-\u0c0c\u0c0e-\u0c10\u0c12-\u0c28\u0c2a-\u0c33\u0c35-\u0c39\u0c3d-\u0c44\u0c46-\u0c48\u0c4a-\u0c4d\u0c55\u0c56\u0c58\u0c59\u0c60-\u0c63\u0c66-\u0c6f\u0c82\u0c83\u0c85-\u0c8c\u0c8e-\u0c90\u0c92-\u0ca8\u0caa-\u0cb3\u0cb5-\u0cb9\u0cbc-\u0cc4\u0cc6-\u0cc8\u0cca-\u0ccd\u0cd5\u0cd6\u0cde\u0ce0-\u0ce3\u0ce6-\u0cef\u0cf1\u0cf2\u0d02\u0d03\u0d05-\u0d0c\u0d0e-\u0d10\u0d12-\u0d3a\u0d3d-\u0d44\u0d46-\u0d48\u0d4a-\u0d4e\u0d57\u0d60-\u0d63\u0d66-\u0d6f\u0d7a-\u0d7f\u0d82\u0d83\u0d85-\u0d96\u0d9a-\u0db1\u0db3-\u0dbb\u0dbd\u0dc0-\u0dc6\u0dca\u0dcf-\u0dd4\u0dd6\u0dd8-\u0ddf\u0df2\u0df3\u0e01-\u0e3a\u0e40-\u0e4e\u0e50-\u0e59\u0e81\u0e82\u0e84\u0e87\u0e88\u0e8a\u0e8d\u0e94-\u0e97\u0e99-\u0e9f\u0ea1-\u0ea3\u0ea5\u0ea7\u0eaa\u0eab\u0ead-\u0eb9\u0ebb-\u0ebd\u0ec0-\u0ec4\u0ec6\u0ec8-\u0ecd\u0ed0-\u0ed9\u0edc-\u0edf\u0f00\u0f18\u0f19\u0f20-\u0f29\u0f35\u0f37\u0f39\u0f3e-\u0f47\u0f49-\u0f6c\u0f71-\u0f84\u0f86-\u0f97\u0f99-\u0fbc\u0fc6\u1000-\u1049\u1050-\u109d\u10a0-\u10c5\u10c7\u10cd\u10d0-\u10fa\u10fc-\u1248\u124a-\u124d\u1250-\u1256\u1258\u125a-\u125d\u1260-\u1288\u128a-\u128d\u1290-\u12b0\u12b2-\u12b5\u12b8-\u12be\u12c0\u12c2-\u12c5\u12c8-\u12d6\u12d8-\u1310\u1312-\u1315\u1318-\u135a\u135d-\u135f\u1380-\u138f\u13a0-\u13f4\u1401-\u166c\u166f-\u167f\u1681-\u169a\u16a0-\u16ea\u16ee-\u16f0\u1700-\u170c\u170e-\u1714\u1720-\u1734\u1740-\u1753\u1760-\u176c\u176e-\u1770\u1772\u1773\u1780-\u17d3\u17d7\u17dc\u17dd\u17e0-\u17e9\u180b-\u180d\u1810-\u1819\u1820-\u1877\u1880-\u18aa\u18b0-\u18f5\u1900-\u191c\u1920-\u192b\u1930-\u193b\u1946-\u196d\u1970-\u1974\u1980-\u19ab\u19b0-\u19c9\u19d0-\u19d9\u1a00-\u1a1b\u1a20-\u1a5e\u1a60-\u1a7c\u1a7f-\u1a89\u1a90-\u1a99\u1aa7\u1b00-\u1b4b\u1b50-\u1b59\u1b6b-\u1b73\u1b80-\u1bf3\u1c00-\u1c37\u1c40-\u1c49\u1c4d-\u1c7d\u1cd0-\u1cd2\u1cd4-\u1cf6\u1d00-\u1de6\u1dfc-\u1f15\u1f18-\u1f1d\u1f20-\u1f45\u1f48-\u1f4d\u1f50-\u1f57\u1f59\u1f5b\u1f5d\u1f5f-\u1f7d\u1f80-\u1fb4\u1fb6-\u1fbc\u1fbe\u1fc2-\u1fc4\u1fc6-\u1fcc\u1fd0-\u1fd3\u1fd6-\u1fdb\u1fe0-\u1fec\u1ff2-\u1ff4\u1ff6-\u1ffc\u200c\u200d\u203f\u2040\u2054\u2071\u207f\u2090-\u209c\u20d0-\u20dc\u20e1\u20e5-\u20f0\u2102\u2107\u210a-\u2113\u2115\u2119-\u211d\u2124\u2126\u2128\u212a-\u212d\u212f-\u2139\u213c-\u213f\u2145-\u2149\u214e\u2160-\u2188\u2c00-\u2c2e\u2c30-\u2c5e\u2c60-\u2ce4\u2ceb-\u2cf3\u2d00-\u2d25\u2d27\u2d2d\u2d30-\u2d67\u2d6f\u2d7f-\u2d96\u2da0-\u2da6\u2da8-\u2dae\u2db0-\u2db6\u2db8-\u2dbe\u2dc0-\u2dc6\u2dc8-\u2dce\u2dd0-\u2dd6\u2dd8-\u2dde\u2de0-\u2dff\u2e2f\u3005-\u3007\u3021-\u302f\u3031-\u3035\u3038-\u303c\u3041-\u3096\u3099\u309a\u309d-\u309f\u30a1-\u30fa\u30fc-\u30ff\u3105-\u312d\u3131-\u318e\u31a0-\u31ba\u31f0-\u31ff\u3400-\u4db5\u4e00-\u9fcc\ua000-\ua48c\ua4d0-\ua4fd\ua500-\ua60c\ua610-\ua62b\ua640-\ua66f\ua674-\ua67d\ua67f-\ua697\ua69f-\ua6f1\ua717-\ua71f\ua722-\ua788\ua78b-\ua78e\ua790-\ua793\ua7a0-\ua7aa\ua7f8-\ua827\ua840-\ua873\ua880-\ua8c4\ua8d0-\ua8d9\ua8e0-\ua8f7\ua8fb\ua900-\ua92d\ua930-\ua953\ua960-\ua97c\ua980-\ua9c0\ua9cf-\ua9d9\uaa00-\uaa36\uaa40-\uaa4d\uaa50-\uaa59\uaa60-\uaa76\uaa7a\uaa7b\uaa80-\uaac2\uaadb-\uaadd\uaae0-\uaaef\uaaf2-\uaaf6\uab01-\uab06\uab09-\uab0e\uab11-\uab16\uab20-\uab26\uab28-\uab2e\uabc0-\uabea\uabec\uabed\uabf0-\uabf9\uac00-\ud7a3\ud7b0-\ud7c6\ud7cb-\ud7fb\uf900-\ufa6d\ufa70-\ufad9\ufb00-\ufb06\ufb13-\ufb17\ufb1d-\ufb28\ufb2a-\ufb36\ufb38-\ufb3c\ufb3e\ufb40\ufb41\ufb43\ufb44\ufb46-\ufbb1\ufbd3-\ufd3d\ufd50-\ufd8f\ufd92-\ufdc7\ufdf0-\ufdfb\ufe00-\ufe0f\ufe20-\ufe26\ufe33\ufe34\ufe4d-\ufe4f\ufe70-\ufe74\ufe76-\ufefc\uff10-\uff19\uff21-\uff3a\uff3f\uff41-\uff5a\uff66-\uffbe\uffc2-\uffc7\uffca-\uffcf\uffd2-\uffd7\uffda-\uffdc]')
    };

    function getDefaultOptions() {
        // default options
        return {
            indent: null,
            base: null,
            parse: null,
            comment: false,
            format: {
                indent: {
                    style: '    ',
                    base: 0,
                    adjustMultilineComment: false
                },
                json: false,
                renumber: false,
                hexadecimal: false,
                quotes: 'single',
                escapeless: false,
                compact: false,
                parentheses: true,
                semicolons: true
            }
        };
    }

    function stringToArray(str) {
        var length = str.length,
            result = [],
            i;
        for (i = 0; i < length; i += 1) {
            result[i] = str.charAt(i);
        }
        return result;
    }

    function stringRepeat(str, num) {
        var result = '';

        for (num |= 0; num > 0; num >>>= 1, str += str) {
            if (num & 1) {
                result += str;
            }
        }

        return result;
    }

    isArray = Array.isArray;
    if (!isArray) {
        isArray = function isArray(array) {
            return Object.prototype.toString.call(array) === '[object Array]';
        };
    }

    function endsWithLineTerminator(str) {
        var len, ch;
        len = str.length;
        ch = str.charAt(len - 1);
        return ch === '\r' || ch === '\n';
    }

    function shallowCopy(obj) {
        var ret = {}, key;
        for (key in obj) {
            if (obj.hasOwnProperty(key)) {
                ret[key] = obj[key];
            }
        }
        return ret;
    }

    function deepCopy(obj) {
        var ret = {}, key, val;
        for (key in obj) {
            if (obj.hasOwnProperty(key)) {
                val = obj[key];
                if (typeof val === 'object' && val !== null) {
                    ret[key] = deepCopy(val);
                } else {
                    ret[key] = val;
                }
            }
        }
        return ret;
    }

    function updateDeeply(target, override) {
        var key, val;

        function isHashObject(target) {
            return typeof target === 'object' && target instanceof Object && !(target instanceof RegExp);
        }

        for (key in override) {
            if (override.hasOwnProperty(key)) {
                val = override[key];
                if (isHashObject(val)) {
                    if (isHashObject(target[key])) {
                        updateDeeply(target[key], val);
                    } else {
                        target[key] = updateDeeply({}, val);
                    }
                } else {
                    target[key] = val;
                }
            }
        }
        return target;
    }

    function generateNumber(value, forceDouble) {
        var result, point, temp, exponent, pos;

        if (value !== value) {
            throw new Error('Numeric literal whose value is NaN');
        }
        if (1 / value < 0) {
            throw new Error('Numeric literal whose value is negative');
        }

        if (value === 1 / 0) {
            return json ? 'null' : renumber ? '1e400' : '1e+400';
        }

        result = '' + value;

        if(forceDouble && result.indexOf('.') === -1) {
            result += '.0';
        }

        if (!renumber || result.length < 3) {
            return result;
        }

        point = result.indexOf('.');
        if (!json && result.charAt(0) === '0' && point === 1) {
            point = 0;
            result = result.slice(1);
        }
        temp = result;
        result = result.replace('e+', 'e');
        exponent = 0;
        if ((pos = temp.indexOf('e')) > 0) {
            exponent = +temp.slice(pos + 1);
            temp = temp.slice(0, pos);
        }
        if (point >= 0) {
            exponent -= temp.length - point - 1;
            temp = +(temp.slice(0, point) + temp.slice(point + 1)) + '';
        }
        pos = 0;
        while (temp.charAt(temp.length + pos - 1) === '0') {
            pos -= 1;
        }
        if (pos !== 0) {
            exponent -= pos;
            temp = temp.slice(0, pos);
        }
        if (exponent !== 0) {
            temp += 'e' + exponent;
        }
        if ((temp.length < result.length ||
                    (hexadecimal && value > 1e12 && Math.floor(value) === value && (temp = '0x' + value.toString(16)).length < result.length)) &&
                +temp === value) {
            result = temp;
        }

        return result;
    }

    function escapeAllowedCharacter(ch, next) {
        var code = ch.charCodeAt(0), hex = code.toString(16), result = '\\';

        switch (ch) {
        case '\b':
            result += 'b';
            break;
        case '\f':
            result += 'f';
            break;
        case '\t':
            result += 't';
            break;
        default:
            if (json || code > 0xff) {
                result += 'u' + '0000'.slice(hex.length) + hex;
            } else if (ch === '\u0000' && '0123456789'.indexOf(next) < 0) {
                result += '0';
            } else if (ch === '\v') {
                result += 'v';
            } else {
                result += 'x' + '00'.slice(hex.length) + hex;
            }
            break;
        }

        return result;
    }

    function escapeDisallowedCharacter(ch) {
        var result = '\\';
        switch (ch) {
        case '\\':
            result += '\\';
            break;
        case '\n':
            result += 'n';
            break;
        case '\r':
            result += 'r';
            break;
        case '\u2028':
            result += 'u2028';
            break;
        case '\u2029':
            result += 'u2029';
            break;
        default:
            throw new Error('Incorrectly classified character');
        }

        return result;
    }

    function escapeString(str) {
        var result = '', i, len, ch, next, singleQuotes = 0, doubleQuotes = 0, single;

        if (typeof str[0] === 'undefined') {
            str = stringToArray(str);
        }

        for (i = 0, len = str.length; i < len; i += 1) {
            ch = str[i];
            if (ch === '\'') {
                singleQuotes += 1;
            } else if (ch === '"') {
                doubleQuotes += 1;
            } else if (ch === '/' && json) {
                result += '\\';
            } else if ('\\\n\r\u2028\u2029'.indexOf(ch) >= 0) {
                result += escapeDisallowedCharacter(ch);
                continue;
            } else if ((json && ch < ' ') || !(json || escapeless || (ch >= ' ' && ch <= '~'))) {
                result += escapeAllowedCharacter(ch, str[i + 1]);
                continue;
            }
            result += ch;
        }

        single = !(quotes === 'double' || (quotes === 'auto' && doubleQuotes < singleQuotes));
        str = result;
        result = single ? '\'' : '"';

        if (typeof str[0] === 'undefined') {
            str = stringToArray(str);
        }

        for (i = 0, len = str.length; i < len; i += 1) {
            ch = str[i];
            if ((ch === '\'' && single) || (ch === '"' && !single)) {
                result += '\\';
            }
            result += ch;
        }

        return result + (single ? '\'' : '"');
    }

    function isWhiteSpace(ch) {
        return '\t\v\f \xa0'.indexOf(ch) >= 0 || (ch.charCodeAt(0) >= 0x1680 && '\u1680\u180e\u2000\u2001\u2002\u2003\u2004\u2005\u2006\u2007\u2008\u2009\u200a\u202f\u205f\u3000\ufeff'.indexOf(ch) >= 0);
    }

    function isLineTerminator(ch) {
        return '\n\r\u2028\u2029'.indexOf(ch) >= 0;
    }

    function isIdentifierPart(ch) {
        return (ch === '$') || (ch === '_') || (ch === '\\') ||
            (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') ||
            ((ch >= '0') && (ch <= '9')) ||
            ((ch.charCodeAt(0) >= 0x80) && Regex.NonAsciiIdentifierPart.test(ch));
    }

    function join(left, right) {
        var leftChar = left.charAt(left.length - 1),
            rightChar = right.charAt(0);

        if (((leftChar === '+' || leftChar === '-') && leftChar === rightChar) || (isIdentifierPart(leftChar) && isIdentifierPart(rightChar))) {
            return left + ' ' + right;
        } else if (isWhiteSpace(leftChar) || isLineTerminator(leftChar) || isWhiteSpace(rightChar) || isLineTerminator(rightChar)) {
            return left + right;
        }
        return left + space + right;
    }

    function addIndent(stmt) {
        return base + stmt;
    }

    function calculateSpaces(str) {
        var i;
        for (i = str.length - 1; i >= 0; i -= 1) {
            if (isLineTerminator(str.charAt(i))) {
                break;
            }
        }
        return (str.length - 1) - i;
    }

    function adjustMultilineComment(value, specialBase) {
        var array, i, len, line, j, ch, spaces, previousBase;

        array = value.split(/\r\n|[\r\n]/);
        spaces = Number.MAX_VALUE;

        // first line doesn't have indentation
        for (i = 1, len = array.length; i < len; i += 1) {
            line = array[i];
            j = 0;
            while (j < line.length && isWhiteSpace(line[j])) {
                j += 1;
            }
            if (spaces > j) {
                spaces = j;
            }
        }

        if (typeof specialBase !== 'undefined') {
            // pattern like
            // {
            //   var t = 20;  /*
            //                 * this is comment
            //                 */
            // }
            previousBase = base;
            if (array[1][spaces] === '*') {
                specialBase += ' ';
            }
            base = specialBase;
        } else {
            if (spaces % 2 === 1) {
                // /*
                //  *
                //  */
                // If spaces are odd number, above pattern is considered.
                // We waste 1 space.
                spaces -= 1;
            }
            previousBase = base;
        }

        for (i = 1, len = array.length; i < len; i += 1) {
            array[i] = addIndent(array[i].slice(spaces));
        }

        base = previousBase;

        return array.join('\n');
    }

    function generateComment(comment, specialBase) {
        if (comment.type === 'Line') {
            if (endsWithLineTerminator(comment.value)) {
                return '//' + comment.value;
            } else {
                // Always use LineTerminator
                return '//' + comment.value + '\n';
            }
        }
        if (extra.format.indent.adjustMultilineComment && /[\n\r]/.test(comment.value)) {
            return adjustMultilineComment('/*' + comment.value + '*/', specialBase);
        }
        return '/*' + comment.value + '*/';
    }

    function addCommentsToStatement(stmt, result) {
        var i, len, comment, save, node, tailingToStatement, specialBase, fragment;

        if (stmt.leadingComments) {
            save = result;

            comment = stmt.leadingComments[0];
            result = generateComment(comment);
            if (!endsWithLineTerminator(result)) {
                result += '\n';
            }

            for (i = 1, len = stmt.leadingComments.length; i < len; i += 1) {
                comment = stmt.leadingComments[i];
                fragment = generateComment(comment);
                if (!endsWithLineTerminator(fragment)) {
                    fragment += '\n';
                }
                result += addIndent(fragment);
            }

            result += addIndent(save);
        }

        if (stmt.trailingComments) {
            tailingToStatement = !endsWithLineTerminator(result);
            specialBase = stringRepeat(' ', calculateSpaces(base + result + indent));
            for (i = 0, len = stmt.trailingComments.length; i < len; i += 1) {
                comment = stmt.trailingComments[i];
                if (tailingToStatement) {
                    // We assume target like following script
                    //
                    // var t = 20;  /**
                    //               * This is comment of t
                    //               */
                    if (i === 0) {
                        // first case
                        result += indent;
                    } else {
                        result += specialBase;
                    }
                    result += generateComment(comment, specialBase);
                } else {
                    result += addIndent(generateComment(comment));
                }
                if (i !== len - 1 && !endsWithLineTerminator(result)) {
                    result += '\n';
                }
            }
        }

        return result;
    }

    function parenthesize(text, current, should) {
        if (current < should) {
            return '(' + text + ')';
        }
        return text;
    }

    function maybeBlock(stmt, semicolonOptional) {
        var previousBase, result, noLeadingComment;

        noLeadingComment = !extra.comment || !stmt.leadingComments;

        if (stmt.type === Syntax.BlockStatement && noLeadingComment) {
            return space + generateStatement(stmt);
        }

        if (stmt.type === Syntax.EmptyStatement && noLeadingComment) {
            return ';';
        }

        previousBase = base;
        base += indent;
        result = newline + addIndent(generateStatement(stmt, { semicolonOptional: semicolonOptional }));
        base = previousBase;

        return result;
    }

    function maybeBlockSuffix(stmt, result) {
        if (stmt.type === Syntax.BlockStatement && (!extra.comment || !stmt.leadingComments) && !endsWithLineTerminator(result)) {
            return space;
        }
        if (endsWithLineTerminator(result)) {
            return addIndent('');
        }
        return (newline === '' ? ' ' : newline) + addIndent('');
    }

    function generateFunctionBody(node) {
        var result, i, len;
        result = '(';
        for (i = 0, len = node.params.length; i < len; i += 1) {
            result += node.params[i].name;
            if (i + 1 < len) {
                result += ',' + space;
            }
        }
        return result + ')' + maybeBlock(node.body);
    }

    function generateExpression(expr, option) {
        var result, precedence, currentPrecedence, previousBase, i, len, raw, fragment, allowIn, allowCall, allowUnparenthesizedNew;

        precedence = option.precedence;
        allowIn = option.allowIn;
        allowCall = option.allowCall;

        switch (expr.type) {
        case Syntax.SequenceExpression:
            result = '';
            allowIn |= (Precedence.Sequence < precedence);
            for (i = 0, len = expr.expressions.length; i < len; i += 1) {
                result += generateExpression(expr.expressions[i], {
                    precedence: Precedence.Assignment,
                    allowIn: allowIn,
                    allowCall: true
                });
                if (i + 1 < len) {
                    result += ',' + space;
                }
            }
            result = parenthesize(result, Precedence.Sequence, precedence);
            break;

        case Syntax.AssignmentExpression:
            allowIn |= (Precedence.Assignment < precedence);
            result = parenthesize(
                generateExpression(expr.left, {
                    precedence: Precedence.Call,
                    allowIn: allowIn,
                    allowCall: true
                }) + space + expr.operator + space +
                    generateExpression(expr.right, {
                        precedence: Precedence.Assignment,
                        allowIn: allowIn,
                        allowCall: true
                    }),
                Precedence.Assignment,
                precedence
            );
            break;

        case Syntax.ConditionalExpression:
            allowIn |= (Precedence.Conditional < precedence);
            result = parenthesize(
                generateExpression(expr.test, {
                    precedence: Precedence.LogicalOR,
                    allowIn: allowIn,
                    allowCall: true
                }) + space + '?' + space +
                    generateExpression(expr.consequent, {
                        precedence: Precedence.Assignment,
                        allowIn: allowIn,
                        allowCall: true
                    }) + space + ':' + space +
                    generateExpression(expr.alternate, {
                        precedence: Precedence.Assignment,
                        allowIn: allowIn,
                        allowCall: true
                    }),
                Precedence.Conditional,
                precedence
            );
            break;

        case Syntax.LogicalExpression:
        case Syntax.BinaryExpression:
            currentPrecedence = BinaryPrecedence[expr.operator];

            allowIn |= (currentPrecedence < precedence);

            result = join(
                generateExpression(expr.left, {
                    precedence: currentPrecedence,
                    allowIn: allowIn,
                    allowCall: true
                }),
                expr.operator
            );

            fragment = generateExpression(expr.right, {
                precedence: currentPrecedence + 1,
                allowIn: allowIn,
                allowCall: true
            });

            if (expr.operator === '/' && result.charAt(result.length - 1) === '/') {
                // If '/' concats with '/', it is interpreted as comment start
                result += ' ' + fragment;
            } else {
                result = join(result, fragment);
            }

            if (expr.operator === 'in' && !allowIn) {
                result = '(' + result + ')';
            } else {
                result = parenthesize(result, currentPrecedence, precedence);
            }

            break;

        case Syntax.CallExpression:
            result = generateExpression(expr.callee, {
                precedence: Precedence.Call,
                allowIn: true,
                allowCall: true,
                allowUnparenthesizedNew: false
            });

            result += '(';
            for (i = 0, len = expr['arguments'].length; i < len; i += 1) {
                result += generateExpression(expr['arguments'][i], {
                    precedence: Precedence.Assignment,
                    allowIn: true,
                    allowCall: true
                });
                if (i + 1 < len) {
                    result += ',' + space;
                }
            }
            result += ')';

            if (!allowCall) {
                result = '(' + result + ')';
            } else {
                result = parenthesize(result, Precedence.Call, precedence);
            }
            break;

        case Syntax.NewExpression:
            len = expr['arguments'].length;
            allowUnparenthesizedNew = option.allowUnparenthesizedNew === undefined || option.allowUnparenthesizedNew;

            result = join(
                'new',
                generateExpression(expr.callee, {
                    precedence: Precedence.New,
                    allowIn: true,
                    allowCall: false,
                    allowUnparenthesizedNew: allowUnparenthesizedNew && !parentheses && len === 0
                })
            );

            if (!allowUnparenthesizedNew || parentheses || len > 0) {
                result += '(';
                for (i = 0; i < len; i += 1) {
                    result += generateExpression(expr['arguments'][i], {
                        precedence: Precedence.Assignment,
                        allowIn: true,
                        allowCall: true
                    });
                    if (i + 1 < len) {
                        result += ',' + space;
                    }
                }
                result += ')';
            }

            result = parenthesize(result, Precedence.New, precedence);
            break;

        case Syntax.MemberExpression:
            result = generateExpression(expr.object, {
                precedence: Precedence.Call,
                allowIn: true,
                allowCall: allowCall,
                allowUnparenthesizedNew: false
            });

            if (expr.computed) {
                result += '[' + generateExpression(expr.property, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: allowCall
                }) + ']';
            } else {
                if (expr.object.type === Syntax.Literal && typeof expr.object.value === 'number') {
                    if (result.indexOf('.') < 0) {
                        if (!/[eExX]/.test(result) && !(result.length >= 2 && result[0] === '0')) {
                            result += '.';
                        }
                    }
                }
                result += '.' + expr.property.name;
            }

            result = parenthesize(result, Precedence.Member, precedence);
            break;

        case Syntax.UnaryExpression:
            fragment = generateExpression(expr.argument, {
                precedence: Precedence.Unary + (
                    expr.argument.type === Syntax.UnaryExpression &&
                        expr.operator.length < 3 &&
                        expr.argument.operator === expr.operator ? 1 : 0
                ),
                allowIn: true,
                allowCall: true
            });

            if (space === '') {
                result = join(expr.operator, fragment);
            } else {
                result = expr.operator;
                if (result.length > 2) {
                    result += ' ';
                }
                result += fragment;
            }
            result = parenthesize(result, Precedence.Unary, precedence);
            break;

        case Syntax.UpdateExpression:
            if (expr.prefix) {
                result = parenthesize(
                    expr.operator +
                        generateExpression(expr.argument, {
                            precedence: Precedence.Unary,
                            allowIn: true,
                            allowCall: true
                        }),
                    Precedence.Unary,
                    precedence
                );
            } else {
                result = parenthesize(
                    generateExpression(expr.argument, {
                        precedence: Precedence.Postfix,
                        allowIn: true,
                        allowCall: true
                    }) + expr.operator,
                    Precedence.Postfix,
                    precedence
                );
            }
            break;

        case Syntax.FunctionExpression:
            result = 'function';
            if (expr.id) {
                result += ' ' + expr.id.name;
            } else {
                result += space;
            }
            result += generateFunctionBody(expr);
            break;

        case Syntax.ArrayExpression:
            if (!expr.elements.length) {
                result = '[]';
                break;
            }
            result = '[' + newline;
            previousBase = base;
            base += indent;
            for (i = 0, len = expr.elements.length; i < len; i += 1) {
                if (!expr.elements[i]) {
                    result += addIndent('');
                    if (i + 1 === len) {
                        result += ',';
                    }
                } else {
                    result += addIndent(generateExpression(expr.elements[i], {
                        precedence: Precedence.Assignment,
                        allowIn: true,
                        allowCall: true
                    }));
                }
                if (i + 1 < len) {
                    result += ',' + newline;
                }
            }
            base = previousBase;
            if (!endsWithLineTerminator(result)) {
                result += newline;
            }
            result += addIndent(']');
            break;

        case Syntax.Property:
            if (expr.kind === 'get' || expr.kind === 'set') {
                result = expr.kind + ' ' + generateExpression(expr.key, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }) + generateFunctionBody(expr.value);
            } else {
                result =
                    generateExpression(expr.key, {
                        precedence: Precedence.Sequence,
                        allowIn: true,
                        allowCall: true
                    }) + ':' + space + generateExpression(expr.value, {
                        precedence: Precedence.Assignment,
                        allowIn: true,
                        allowCall: true
                    });
            }
            break;

        case Syntax.ObjectExpression:
            if (!expr.properties.length) {
                result = '{}';
                break;
            }
            result = '{' + newline;
            previousBase = base;
            base += indent;
            for (i = 0, len = expr.properties.length; i < len; i += 1) {
                result += addIndent(generateExpression(expr.properties[i], {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }));
                if (i + 1 < len) {
                    result += ',' + newline;
                }
            }
            base = previousBase;
            if (!endsWithLineTerminator(result)) {
                result += newline;
            }
            result += addIndent('}');
            break;

        case Syntax.ThisExpression:
            result = 'this';
            break;

        case Syntax.Identifier:
            result = expr.name;
            break;

        case Syntax.Literal:
            if (expr.hasOwnProperty('raw') && parse) {
                try {
                    raw = parse(expr.raw).body[0].expression;
                    if (raw.type === Syntax.Literal) {
                        if (raw.value === expr.value) {
                            result = expr.raw;
                            break;
                        }
                    }
                } catch (e) {
                    // not use raw property
                }
            }

            if (expr.value === null) {
                result = 'null';
                break;
            }

            if (typeof expr.value === 'string') {
                result = escapeString(expr.value);
                break;
            }

            if (typeof expr.value === 'number') {
                result = generateNumber(expr.value, expr.forceDouble);
                break;
            }

            result = expr.value.toString();
            break;

        default:
            break;
        }

        if (result === undefined) {
            throw new Error('Unknown expression type: ' + expr.type);
        }
        return result;
    }

    function generateStatement(stmt, option) {
        var i, len, result, previousBase, node, allowIn, fragment, semicolon;

        allowIn = true;
        semicolon = ';';
        if (option) {
            allowIn = option.allowIn === undefined || option.allowIn;
            if (!semicolons && option.semicolonOptional === true) {
                semicolon = '';
            }
        }

        switch (stmt.type) {
        case Syntax.BlockStatement:
            result = '';
            if(!stmt.inline) {
              result = '{' + newline;
            }

            previousBase = base;
            base += indent;
            for (i = 0, len = stmt.body.length; i < len; i += 1) {
                fragment = addIndent(generateStatement(stmt.body[i], {semicolonOptional: i === len - 1}));
                result += fragment;
                if (!endsWithLineTerminator(fragment)) {
                    result += newline;
                }
            }
            base = previousBase;

            if(!stmt.inline) {
              result += addIndent('}');
            }
            break;

        case Syntax.BreakStatement:
            if (stmt.label) {
                result = 'break ' + stmt.label.name + semicolon;
            } else {
                result = 'break' + semicolon;
            }
            break;

        case Syntax.ContinueStatement:
            if (stmt.label) {
                result = 'continue ' + stmt.label.name + semicolon;
            } else {
                result = 'continue' + semicolon;
            }
            break;

        case Syntax.DoWhileStatement:
            result = join('do', maybeBlock(stmt.body));
            result += maybeBlockSuffix(stmt.body, result);
            result += 'while' + space + '(' + generateExpression(stmt.test, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }) + ')' + semicolon;
            break;

        case Syntax.CatchClause:
            previousBase = base;
            base += indent;
            result = 'catch' + space + '(' + generateExpression(stmt.param, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }) + ')';
            base = previousBase;
            result += maybeBlock(stmt.body);
            break;

        case Syntax.DebuggerStatement:
            result = 'debugger' + semicolon;
            break;

        case Syntax.EmptyStatement:
            result = ';';
            break;

        case Syntax.ExpressionStatement:
            result = generateExpression(stmt.expression, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            });
            // 12.4 '{', 'function' is not allowed in this position.
            // wrap expression with parentheses
            if (result.charAt(0) === '{' || (result.slice(0, 8) === 'function' && " (".indexOf(result.charAt(8)) >= 0)) {
                result = '(' + result + ')' + semicolon;
            } else {
                result += semicolon;
            }
            break;

        case Syntax.VariableDeclarator:
            if (stmt.init) {
                result = stmt.id.name + space + '=' + space + generateExpression(stmt.init, {
                    precedence: Precedence.Assignment,
                    allowIn: allowIn,
                    allowCall: true
                });
            } else {
                result = stmt.id.name;
            }
            break;

        case Syntax.VariableDeclaration:
            result = stmt.kind;
            // special path for
            // var x = function () {
            // };
            if (stmt.declarations.length === 1 && stmt.declarations[0].init &&
                    stmt.declarations[0].init.type === Syntax.FunctionExpression) {
                result += ' ' + generateStatement(stmt.declarations[0], {
                    allowIn: allowIn
                });
            } else {
                // VariableDeclarator is typed as Statement,
                // but joined with comma (not LineTerminator).
                // So if comment is attached to target node, we should specialize.
                previousBase = base;
                base += indent;

                node = stmt.declarations[0];
                if (extra.comment && node.leadingComments) {
                    result += '\n' + addIndent(generateStatement(node, {
                        allowIn: allowIn
                    }));
                } else {
                    result += ' ' + generateStatement(node, {
                        allowIn: allowIn
                    });
                }

                for (i = 1, len = stmt.declarations.length; i < len; i += 1) {
                    node = stmt.declarations[i];
                    if (extra.comment && node.leadingComments) {
                        result += ',' + newline + addIndent(generateStatement(node, {
                            allowIn: allowIn
                        }));
                    } else {
                        result += ',' + space + generateStatement(node, {
                            allowIn: allowIn
                        });
                    }
                }
                base = previousBase;
            }
            result += semicolon;
            break;

        case Syntax.ThrowStatement:
            result = join(
                'throw',
                generateExpression(stmt.argument, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                })
            ) + semicolon;
            break;

        case Syntax.TryStatement:
            result = 'try' + maybeBlock(stmt.block);
            result += maybeBlockSuffix(stmt.block, result);
            for (i = 0, len = stmt.handlers.length; i < len; i += 1) {
                result += generateStatement(stmt.handlers[i]);
                if (stmt.finalizer || i + 1 !== len) {
                    result += maybeBlockSuffix(stmt.handlers[i].body, result);
                }
            }
            if (stmt.finalizer) {
                result += 'finally' + maybeBlock(stmt.finalizer);
            }
            break;

        case Syntax.SwitchStatement:
            previousBase = base;
            base += indent;
            result = 'switch' + space + '(' + generateExpression(stmt.discriminant, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }) + ')' + space + '{' + newline;
            base = previousBase;
            if (stmt.cases) {
                for (i = 0, len = stmt.cases.length; i < len; i += 1) {
                    fragment = addIndent(generateStatement(stmt.cases[i], {semicolonOptional: i === len - 1}));
                    result += fragment;
                    if (!endsWithLineTerminator(fragment)) {
                        result += newline;
                    }
                }
            }
            result += addIndent('}');
            break;

        case Syntax.SwitchCase:
            previousBase = base;
            base += indent;
            if (stmt.test) {
                result = join(
                    'case',
                    generateExpression(stmt.test, {
                        precedence: Precedence.Sequence,
                        allowIn: true,
                        allowCall: true
                    })
                ) + ':';
            } else {
                result = 'default:';
            }

            i = 0;
            len = stmt.consequent.length;
            if (len && stmt.consequent[0].type === Syntax.BlockStatement) {
                fragment = maybeBlock(stmt.consequent[0]);
                result += fragment;
                i = 1;
            }

            if (i !== len && !endsWithLineTerminator(result)) {
                result += newline;
            }

            for (; i < len; i += 1) {
                fragment = addIndent(generateStatement(stmt.consequent[i], {semicolonOptional: i === len - 1 && semicolon === ''}));
                result += fragment;
                if (i + 1 !== len && !endsWithLineTerminator(fragment)) {
                    result += newline;
                }
            }

            base = previousBase;
            break;

        case Syntax.IfStatement:
            previousBase = base;
            base += indent;
            if (stmt.alternate) {
                if (stmt.alternate.type === Syntax.IfStatement) {
                    result = 'if' + space + '(' +  generateExpression(stmt.test, {
                        precedence: Precedence.Sequence,
                        allowIn: true,
                        allowCall: true
                    }) + ')';
                    base = previousBase;
                    result += maybeBlock(stmt.consequent);
                    result += maybeBlockSuffix(stmt.consequent, result);
                    result += 'else ' + generateStatement(stmt.alternate);
                } else {
                    result = 'if' + space + '(' + generateExpression(stmt.test, {
                        precedence: Precedence.Sequence,
                        allowIn: true,
                        allowCall: true
                    }) + ')';
                    base = previousBase;
                    result += maybeBlock(stmt.consequent);
                    result += maybeBlockSuffix(stmt.consequent, result);
                    result += 'else';
                    result = join(result, maybeBlock(stmt.alternate, semicolon === ''));
                }
            } else {
                result = 'if' + space + '(' + generateExpression(stmt.test, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }) + ')';
                base = previousBase;
                result += maybeBlock(stmt.consequent, semicolon === '');
            }
            break;

        case Syntax.ForStatement:
            previousBase = base;
            base += indent;
            result = 'for' + space + '(';
            if (stmt.init) {
                if (stmt.init.type === Syntax.VariableDeclaration) {
                    result += generateStatement(stmt.init, {
                        allowIn: false
                    });
                } else {
                    result += generateExpression(stmt.init, {
                        precedence: Precedence.Sequence,
                        allowIn: false,
                        allowCall: true
                    }) + ';';
                }
            } else {
                result += ';';
            }

            if (stmt.test) {
                result += space + generateExpression(stmt.test, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }) + ';';
            } else {
                result += ';';
            }

            if (stmt.update) {
                result += space + generateExpression(stmt.update, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                }) + ')';
            } else {
                result += ')';
            }
            base = previousBase;

            result += maybeBlock(stmt.body, semicolon === '');
            break;

        case Syntax.ForInStatement:
            result = 'for' + space + '(';
            if (stmt.left.type === Syntax.VariableDeclaration) {
                previousBase = base;
                base += indent + indent;
                result += stmt.left.kind + ' ' + generateStatement(stmt.left.declarations[0], {
                    allowIn: false
                });
                base = previousBase;
            } else {
                previousBase = base;
                base += indent;
                result += generateExpression(stmt.left, {
                    precedence: Precedence.Call,
                    allowIn: true,
                    allowCall: true
                });
                base = previousBase;
            }

            previousBase = base;
            base += indent;
            result = join(result, 'in');
            result = join(
                result,
                generateExpression(stmt.right, {
                    precedence: Precedence.Sequence,
                    allowIn: true,
                    allowCall: true
                })
            ) + ')';
            base = previousBase;
            result += maybeBlock(stmt.body, semicolon === '');
            break;

        case Syntax.LabeledStatement:
            result = stmt.label.name + ':' + maybeBlock(stmt.body, semicolon === '');
            break;

        case Syntax.Program:
            result = '';
            for (i = 0, len = stmt.body.length; i < len; i += 1) {
                fragment = addIndent(generateStatement(stmt.body[i], {semicolonOptional: i === len - 1}));
                result += fragment;
                if (i + 1 < len && !endsWithLineTerminator(fragment)) {
                    result += newline;
                }
            }
            break;

        case Syntax.FunctionDeclaration:
            result = 'function' + space;
            if (stmt.id) {
                result += (space === '' ? ' ' : '') + stmt.id.name;
            }
            result += generateFunctionBody(stmt);
            break;

        case Syntax.ReturnStatement:
            if (stmt.argument) {
                result = join(
                    'return',
                    generateExpression(stmt.argument, {
                        precedence: Precedence.Sequence,
                        allowIn: true,
                        allowCall: true
                    })
                ) + semicolon;
            } else {
                result = 'return' + semicolon;
            }
            break;

        case Syntax.WhileStatement:
            previousBase = base;
            base += indent;
            result = 'while' + space + '(' + generateExpression(stmt.test, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }) + ')';
            base = previousBase;
            result += maybeBlock(stmt.body, semicolon === '');
            break;

        case Syntax.WithStatement:
            previousBase = base;
            base += indent;
            result = 'with' + space + '(' + generateExpression(stmt.object, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            }) + ')';
            base = previousBase;
            result += maybeBlock(stmt.body, semicolon === '');
            break;

        default:
            break;
        }

        if (result === undefined) {
            throw new Error('Unknown statement type: ' + stmt.type);
        }

        // Attach comments

        if (extra.comment) {
            return addCommentsToStatement(stmt, result);
        }

        return result;
    }

    function generate(node, options) {
        var defaultOptions = getDefaultOptions();

        if (typeof options !== 'undefined') {
            // Obsolete options
            //
            //   `options.indent`
            //   `options.base`
            //
            // Instead of them, we can use `option.format.indent`.
            if (typeof options.indent === 'string') {
                defaultOptions.format.indent.style = options.indent;
            }
            if (typeof options.base === 'number') {
                defaultOptions.format.indent.base = options.base;
            }
            options = updateDeeply(defaultOptions, options);
            indent = options.format.indent.style;
            if (typeof options.base === 'string') {
                base = options.base;
            } else {
                base = stringRepeat(indent, options.format.indent.base);
            }
        } else {
            options = defaultOptions;
            indent = options.format.indent.style;
            base = stringRepeat(indent, options.format.indent.base);
        }
        json = options.format.json;
        renumber = options.format.renumber;
        hexadecimal = json ? false : options.format.hexadecimal;
        quotes = json ? 'double' : options.format.quotes;
        escapeless = options.format.escapeless;
        if (options.format.compact) {
            newline = space = indent = base = '';
        } else {
            newline = '\n';
            space = ' ';
        }
        parentheses = options.format.parentheses;
        semicolons = options.format.semicolons;
        parse = json ? null : options.parse;
        extra = options;

        switch (node.type) {
        case Syntax.BlockStatement:
        case Syntax.BreakStatement:
        case Syntax.CatchClause:
        case Syntax.ContinueStatement:
        case Syntax.DoWhileStatement:
        case Syntax.DebuggerStatement:
        case Syntax.EmptyStatement:
        case Syntax.ExpressionStatement:
        case Syntax.ForStatement:
        case Syntax.ForInStatement:
        case Syntax.FunctionDeclaration:
        case Syntax.IfStatement:
        case Syntax.LabeledStatement:
        case Syntax.Program:
        case Syntax.ReturnStatement:
        case Syntax.SwitchStatement:
        case Syntax.SwitchCase:
        case Syntax.ThrowStatement:
        case Syntax.TryStatement:
        case Syntax.VariableDeclaration:
        case Syntax.VariableDeclarator:
        case Syntax.WhileStatement:
        case Syntax.WithStatement:
            return generateStatement(node);

        case Syntax.AssignmentExpression:
        case Syntax.ArrayExpression:
        case Syntax.BinaryExpression:
        case Syntax.CallExpression:
        case Syntax.ConditionalExpression:
        case Syntax.FunctionExpression:
        case Syntax.Identifier:
        case Syntax.Literal:
        case Syntax.LogicalExpression:
        case Syntax.MemberExpression:
        case Syntax.NewExpression:
        case Syntax.ObjectExpression:
        case Syntax.Property:
        case Syntax.SequenceExpression:
        case Syntax.ThisExpression:
        case Syntax.UnaryExpression:
        case Syntax.UpdateExpression:
            return generateExpression(node, {
                precedence: Precedence.Sequence,
                allowIn: true,
                allowCall: true
            });

        default:
            break;
        }
        throw new Error('Unknown node type: ' + node.type);
    }

    // simple visitor implementation

    VisitorKeys = {
        AssignmentExpression: ['left', 'right'],
        ArrayExpression: ['elements'],
        BlockStatement: ['body'],
        BinaryExpression: ['left', 'right'],
        BreakStatement: ['label'],
        CallExpression: ['callee', 'arguments'],
        CatchClause: ['param', 'body'],
        ConditionalExpression: ['test', 'consequent', 'alternate'],
        ContinueStatement: ['label'],
        DoWhileStatement: ['body', 'test'],
        DebuggerStatement: [],
        EmptyStatement: [],
        ExpressionStatement: ['expression'],
        ForStatement: ['init', 'test', 'update', 'body'],
        ForInStatement: ['left', 'right', 'body'],
        FunctionDeclaration: ['id', 'params', 'body'],
        FunctionExpression: ['id', 'params', 'body'],
        Identifier: [],
        IfStatement: ['test', 'consequent', 'alternate'],
        Literal: [],
        LabeledStatement: ['label', 'body'],
        LogicalExpression: ['left', 'right'],
        MemberExpression: ['object', 'property'],
        NewExpression: ['callee', 'arguments'],
        ObjectExpression: ['properties'],
        Program: ['body'],
        Property: ['key', 'value'],
        ReturnStatement: ['argument'],
        SequenceExpression: ['expressions'],
        SwitchStatement: ['descriminant', 'cases'],
        SwitchCase: ['test', 'consequent'],
        ThisExpression: [],
        ThrowStatement: ['argument'],
        TryStatement: ['block', 'handlers', 'finalizer'],
        UnaryExpression: ['argument'],
        UpdateExpression: ['argument'],
        VariableDeclaration: ['declarations'],
        VariableDeclarator: ['id', 'init'],
        WhileStatement: ['test', 'body'],
        WithStatement: ['object', 'body'],
        PointerType: ['base'],
        StructType: ['id', 'fields'],
        FieldDeclarator: ['id', 'decltype'],
        ArrowType: ['params', 'return'],
        TypeIdentifier: [],
        TypeAliasDirective: ['original', 'alias'],
        CastExpression: ['as', 'argument']
    };

    VisitorOption = {
        Break: 1,
        Skip: 2
    };

    function traverse(top, visitor) {
        var worklist, leavelist, node, ret, current, current2, candidates, candidate;

        worklist = [ top ];
        leavelist = [];

        while (worklist.length) {
            node = worklist.pop();

            if (node) {
                if (visitor.enter) {
                    ret = visitor.enter(node);
                } else {
                    ret = undefined;
                }

                if (ret === VisitorOption.Break) {
                    return;
                }

                worklist.push(null);
                leavelist.push(node);

                if (ret !== VisitorOption.Skip) {
                    candidates = VisitorKeys[node.type];
                    current = candidates.length;
                    while ((current -= 1) >= 0) {
                        candidate = node[candidates[current]];
                        if (candidate) {
                            if (isArray(candidate)) {
                                current2 = candidate.length;
                                while ((current2 -= 1) >= 0) {
                                    if (candidate[current2]) {
                                        worklist.push(candidate[current2]);
                                    }
                                }
                            } else {
                                worklist.push(candidate);
                            }
                        }
                    }
                }
            } else {
                node = leavelist.pop();
                if (visitor.leave) {
                    ret = visitor.leave(node);
                } else {
                    ret = undefined;
                }
                if (ret === VisitorOption.Break) {
                    return;
                }
            }
        }
    }


    // based on LLVM libc++ upper_bound / lower_bound
    // MIT License

    function upperBound(array, func) {
        var diff, len, i, current;

        len = array.length;
        i = 0;

        while (len) {
            diff = len >>> 1;
            current = i + diff;
            if (func(array[current])) {
                len = diff;
            } else {
                i = current + 1;
                len -= diff + 1;
            }
        }
        return i;
    }

    function lowerBound(array, func) {
        var diff, len, i, current;

        len = array.length;
        i = 0;

        while (len) {
            diff = len >>> 1;
            current = i + diff;
            if (func(array[current])) {
                i = current + 1;
                len -= diff + 1;
            } else {
                len = diff;
            }
        }
        return i;
    }

    function extendCommentRange(comment, tokens) {
        var target, token;

        target = upperBound(tokens, function search(token) {
            return token.range[0] > comment.range[0];
        });

        comment.extendedRange = [comment.range[0], comment.range[1]];

        if (target !== tokens.length) {
            comment.extendedRange[1] = tokens[target].range[0];
        }

        target -= 1;
        if (target >= 0) {
            if (target < tokens.length) {
                comment.extendedRange[0] = tokens[target].range[1];
            } else if (token.length) {
                comment.extendedRange[1] = tokens[tokens.length - 1].range[0];
            }
        }

        return comment;
    }

    function attachComments(tree, providedComments, tokens) {
        // At first, we should calculate extended comment ranges.
        var comments = [], comment, len, i;

        if (!tree.range) {
            throw new Error('attachComments needs range information');
        }

        // tokens array is empty, we attach comments to tree as 'leadingComments'
        if (!tokens.length) {
            if (providedComments.length) {
                for (i = 0, len = providedComments.length; i < len; i += 1) {
                    comment = deepCopy(providedComments[i]);
                    comment.extendedRange = [0, tree.range[0]];
                    comments.push(comment);
                }
                tree.leadingComments = comments;
            }
            return tree;
        }

        for (i = 0, len = providedComments.length; i < len; i += 1) {
            comments.push(extendCommentRange(deepCopy(providedComments[i]), tokens));
        }

        // This is based on John Freeman's implementation.
        traverse(tree, {
            cursor: 0,
            enter: function (node) {
                var comment;

                while (this.cursor < comments.length) {
                    comment = comments[this.cursor];
                    if (comment.extendedRange[1] > node.range[0]) {
                        break;
                    }

                    if (comment.extendedRange[1] === node.range[0]) {
                        if (!node.leadingComments) {
                            node.leadingComments = [];
                        }
                        node.leadingComments.push(comment);
                        comments.splice(this.cursor, 1);
                    } else {
                        this.cursor += 1;
                    }
                }

                // already out of owned node
                if (this.cursor === comments.length) {
                    return VisitorOption.Break;
                }

                if (comments[this.cursor].extendedRange[0] > node.range[1]) {
                    return VisitorOption.Skip;
                }
            }
        });

        traverse(tree, {
            cursor: 0,
            leave: function (node) {
                var comment;

                while (this.cursor < comments.length) {
                    comment = comments[this.cursor];
                    if (node.range[1] < comment.extendedRange[0]) {
                        break;
                    }

                    if (node.range[1] === comment.extendedRange[0]) {
                        if (!node.trailingComments) {
                            node.trailingComments = [];
                        }
                        node.trailingComments.push(comment);
                        comments.splice(this.cursor, 1);
                    } else {
                        this.cursor += 1;
                    }
                }

                // already out of owned node
                if (this.cursor === comments.length) {
                    return VisitorOption.Break;
                }

                if (comments[this.cursor].extendedRange[0] > node.range[1]) {
                    return VisitorOption.Skip;
                }
            }
        });

        return tree;
    }

    // Sync with package.json.
    exports.version = '0.0.6-dev';

    exports.generate = generate;
    exports.traverse = traverse;
    exports.attachComments = attachComments;

}(typeof exports === 'undefined' ? (escodegen = {}) : exports));
/* vim: set sw=4 ts=4 et tw=80 : */
(function (exports) {

  const NODE_JS = 1;
  const JS_SHELL = 2;
  const BROWSER = 3;

  var mode;
  if (typeof process !== "undefined") {
    mode = NODE_JS;
    // Install compiler as an extension for '.ljs' files that are loaded using the
    // |require| function. This is how mocha tests are executed.
    var fs = require('fs');
    require.extensions['.ljs'] = function(module, filename) {
      var source = fs.readFileSync(filename, 'utf8');
      return module._compile(compile(source, {filename: filename, memcheck: false}), filename);
    };
  } else if (typeof snarf !== "undefined") {
    mode = JS_SHELL;
  } else {
    mode = BROWSER;
  }

  var util, esprima, escodegen, estransform, compiler;
  var argv;

  if (mode === NODE_JS) {
    util = require("./util.js");
    esprima = require("./esprima.js");
    escodegen = require("./escodegen.js");
    estransform = require("./estransform.js");
    compiler = require("./compiler.js");
    templates = require('./template/templates');

    snarf = require('fs').readFileSync;
    argv = process.argv.slice(2);
    print = console.log;
    quit = process.exit;
  } else if (mode === JS_SHELL) {
    load("./estransform.js");
    load("./util.js");
    load("./esprima.js");
    load("./escodegen.js");
    load("./compiler.js");
    load("./template/templates.js");

    argv = this.arguments;
  }

  if (mode !== NODE_JS) {
    util = this.util;
    esprima = this.esprima;
    escodegen = this.escodegen;
    estransform = this.estransform;
    compiler = this.compiler;
  }

  const assert = util.assert;
  const lang = estransform.lang;
  const allFields = estransform.allFields;

  function pretty(node, indent) {
    if (typeof indent === "undefined") {
      indent = "";
    }

    var s = "";

    if (node instanceof Array) {
      for (var i = 0, j = node.length; i < j; i++) {
        s += pretty(node[i], indent);
      }
      return s;
    }

    s += indent + node.type;

    var spec = lang[node.type];
    if (!spec) {
      s += " ???\n";
      return s;
    }

    var fields = allFields(spec);
    var children = [];
    var values = [];
    // We do loc manually.
    fields.pop();
    for (var i = 0, j = fields.length; i < j; i++) {
      var fname = fields[i];
      if (fname.charAt(0) === "@") {
        fname = fname.substr(1);
        if (node[fname]) {
          children.push(pretty(node[fname], indent + "  "));
        }
      } else {
        if (typeof node[fname] !== "undefined") {
          values.push(node[fname]);
        }
      }
    }

    if (values.length) {
      s += " '" + values.join("' '") + "'";
    }

    var loc = node.loc;
    if (loc) {
      s += (" (" + loc.start.line + ":" + loc.start.column + "-" +
            loc.end.line + ":" + loc.end.column + ")");
    }

    s += "\n" + children.join("");
    return s;
  }

  function cli() {
    var optparser = new util.OptParser([
      ["a",           "asmjs",        "", "Compile as asm.js module"],
      ["m",           "module-name",  "", "Export asm module as this name"],
      ["e",           "exported-funcs", "main", "Functions to export from the asm module (comma-delimited)"],
      ["E",           "only-parse",   false, "Only parse"],
      ["A",           "emit-ast",     false, "Do not generate JS, emit AST"],
      ["P",           "pretty-print", false, "Pretty-print AST instead of emitting JSON (with -A)"],
      ["b",           "bare",         false, "Do not wrap in a module"],
      // ["l",           "load-instead", false, "Emit load('memory') instead of require('memory')"],
      ["W",           "warn",         true,  "Print warnings (enabled by default)"],
      ["Wconversion",  null,          false, "Print intra-integer and pointer conversion warnings"],
      ["0",           "simple-log",   false, "Log simple messages. No colors and snippets."],
      ["t",           "trace",        false, "Trace compiler execution"],
      ["o",           "output",       "",    "Output file name"],
      // ["m",           "memcheck",     false, "Compile with memcheck instrumentation"],
      ["h",           "help",         false, "Print this message"],
      ["w",           "nowarn",       false, "Inhibit all warning messages"]
    ]);

    var p = optparser.parse(argv);
    if (!p) {
      quit(1);
    }

    var options = p.options;
    var files = p.rest;

    if (!files.length || options.help) {
      print("ljc: [option(s)] file");
      print(optparser.usage());
      quit();
    }

    var filename = files[0];
    var path = filename.split("/");
    var basename = path.pop();
    var dir = path.join("/");
    basename = basename.substr(0, basename.lastIndexOf(".")) || basename;

    var source = snarf(filename);
    options.filename = filename;
    options.basename = basename;
    var code = compile(source, options);

    if (options["pretty-print"]) {
      print(pretty(code));
    } else {
      // SpiderMonkey has no way to write to a file, but if we're on node we can
      // emit .js.
      if (options["output"] && mode === NODE_JS && !options["only-parse"]) {
        // var outname = (dir ? dir + "/" : "") + basename;
        // Don't overwrite the source file by mistake.
        if (options["output"] !== filename) {
          if (options["emit-ast"]) {
            require('fs').writeFileSync(options["output"], JSON.stringify(code, null, 2));
          } else {
            // Escodegen doesn't emit a final newline for some reason, so add one.
            require('fs').writeFileSync(options["output"], code + "\n");
          }
        }
      } else {
        print(code);
      }
    }
  }

  function compile(source, options) {
    // -W anything infers -W.
    for (var p in options) {
      if (p.charAt(0) === "W") {
        options.warn = true;
        break;
      }
    }

    if(options.nowarn) {
      options.warn = false;
    }

    var logger = new util.Logger("ljc", options.filename, source, options);
    var code;

    try {
      var node = esprima.parse(source, { loc: true, comment: true, range: true, tokens: true });
      //node = escodegen.attachComments(node, node.comments, node.tokens);
      var types = null;

      for(var i=0; i<node.body.length; i++) {
          var expr = node.body[i];

          if(expr.type == 'ImportExpression') {
            var importSource = snarf('./' + expr.from.value + '.ljs');
            var importNode = esprima.parse(importSource, { tokens: true });

            types = compiler.getTypes(expr.imports.map(function(x) { return x.name; }),
                                      importNode, types, logger);
          }
      }

      if (options["only-parse"]) {
        code = node;
      } else {
        var data = compiler.compile(node, options.filename, logger, options, types);
        var externs = data.externs;
        var exports = options['exported-funcs'].split(',');
        node = data.node;

        if (options["emit-ast"]) {
          code = node;
        } else {
          code = '';

          if(options.asmjs) {
            code += templates.header.replace(
              '{% imports %}',
              externs.map(function(e) {
                return 'var ' + e + ' = env.' + e + ';';
              }).join('\n')
            );
          }

          code += escodegen.generate(node, { base: "", indent: "  ", comment: true });

          if(options.asmjs) {
            code += templates.footer.replace(
              '{% externs %}',
              externs.map(function(e) {
                return e + ': ' + e + ',';
              }).join('\n')
            ).replace(
              '{% exports %}',
              exports.map(function(e) {
                return e + ': ' + e;
              }).join(',\n')
            ).replace(
              '{% finalize %}',
              (options['module-name'] ?
               'window.' + options['module-name'] + ' = asm;' :
               'asm.main();')
            );
          }
        }
      }
    } catch (e) {
      if (mode === BROWSER) {
        throw e;
      }

      if (e.index) {
        // Esprima error, make a loc out of it.
        var lc = { line: e.lineNumber, column: e.column - 1 };
        e.loc = { start: lc, end: lc };
        logger.error(e.message, { start: lc, end: lc });
        logger.flush();
        quit(1);
      }

      if (e.logged && mode !== BROWSER) {
        // Compiler error that has already been logged, so just flush and
        // quit.
        logger.flush();
        quit(1);
      }

      throw e;
    }

    logger.flush();
    return code;
  }

  exports.cli = cli;
  exports.compile = compile;

  if (mode === JS_SHELL) {
    cli();
  }

}).call(this, typeof exports === "undefined" ? (LJC = {}) : exports);
(function(exports) {exports.header = "(function() {\n\nif(!Math.imul) {\n    Math.imul = function(x, y) { return x * y; };\n}\n\nvar MB = 1024 * 1024;\nvar SIZE = 256 * MB;\nvar STACK_SIZE = 2 * MB;\nvar HEAP_SIZE = SIZE - STACK_SIZE;\nvar buffer = new ArrayBuffer(SIZE);\n\nif(typeof window !== 'undefined') {\n    window.U1 = new Uint8Array(buffer);\n    window.I1 = new Int8Array(buffer);\n    window.U2 = new Uint16Array(buffer);\n    window.I2 = new Int16Array(buffer);\n    window.U4 = new Uint32Array(buffer);\n    window.I4 = new Int32Array(buffer);\n    window.F4 = new Float32Array(buffer);\n    window.F8 = new Float64Array(buffer);\n\n    window.asmBuffer = buffer;\n}\n\nvar asm = (function (global, env, buffer) {\n    \"use asm\";\n\n    var stackSize = env.STACK_SIZE|0;\n    var heapSize = env.HEAP_SIZE|0;\n    var totalSize = env.TOTAL_SIZE|0;\n\n    {% imports %}\n\n    var U1 = new global.Uint8Array(buffer);\n    var I1 = new global.Int8Array(buffer);\n    var U2 = new global.Uint16Array(buffer);\n    var I2 = new global.Int16Array(buffer);\n    var U4 = new global.Uint32Array(buffer);\n    var I4 = new global.Int32Array(buffer);\n    var F4 = new global.Float32Array(buffer);\n    var F8 = new global.Float64Array(buffer);\n\n    var acos = global.Math.acos;\n    var asin = global.Math.asin;\n    var atan = global.Math.atan;\n    var cos = global.Math.cos;\n    var sin = global.Math.sin;\n    var tan = global.Math.tan;\n    var ceil = global.Math.ceil;\n    var floor = global.Math.floor;\n    var exp = global.Math.exp;\n    var log = global.Math.log;\n    var sqrt = global.Math.sqrt;\n    var abs = global.Math.abs;\n    var atan2 = global.Math.atan2;\n    var pow = global.Math.pow;\n    var imul = global.Math.imul;\n\n";
exports.footer = "\n    function memcpy(dest, src, num) {\n        dest = dest|0; src = src|0; num = num|0;\n        var ret = 0;\n        ret = dest|0;\n        if ((dest&3) == (src&3)) {\n            while (dest & 3) {\n                if ((num|0) == 0) return ret|0;\n                U1[(dest)]=U1[(src)];\n                dest = (dest+1)|0;\n                src = (src+1)|0;\n                num = (num-1)|0;\n            }\n            while ((num|0) >= 4) {\n                U4[((dest)>>2)]=U4[((src)>>2)];\n                dest = (dest+4)|0;\n                src = (src+4)|0;\n                num = (num-4)|0;\n            }\n        }\n        while ((num|0) > 0) {\n            U1[(dest)]=U1[(src)];\n            dest = (dest+1)|0;\n            src = (src+1)|0;\n            num = (num-1)|0;\n        }\n        return ret|0;\n    }\n\n    function memset(ptr, value, num) {\n        ptr = ptr|0; value = value|0; num = num|0;\n        var stop = 0, value4 = 0, stop4 = 0, unaligned = 0;\n        stop = (ptr + num)|0;\n        if ((num|0) >= 20) {\n            // This is unaligned, but quite large, so work hard to get to aligned settings\n            value = value & 0xff;\n            unaligned = ptr & 3;\n            value4 = value | (value << 8) | (value << 16) | (value << 24);\n            stop4 = stop & ~3;\n            if (unaligned) {\n                unaligned = (ptr + 4 - unaligned)|0;\n                while ((ptr|0) < (unaligned|0)) { // no need to check for stop, since we have large num\n                    U1[(ptr)]=value;\n                    ptr = (ptr+1)|0;\n                }\n            }\n            while ((ptr|0) < (stop4|0)) {\n                U4[((ptr)>>2)]=value4;\n                ptr = (ptr+4)|0;\n            }\n        }\n        while ((ptr|0) < (stop|0)) {\n            U1[(ptr)]=value;\n            ptr = (ptr+1)|0;\n        }\n    }\n\n    return { {% exports %} };\n\n})({ Uint8Array: Uint8Array,\n     Int8Array: Int8Array,\n     Uint16Array: Uint16Array,\n     Int16Array: Int16Array,\n     Uint32Array: Uint32Array,\n     Int32Array: Int32Array,\n     Float32Array: Float32Array,\n     Float64Array: Float64Array,\n     Math: Math },\n   { {% externs %}\n     HEAP_SIZE: HEAP_SIZE,\n     STACK_SIZE: STACK_SIZE,\n     TOTAL_SIZE: SIZE },\n   buffer);\n\nfunction assertEqual(val1, val2) {\n  var err = true;\n  var msg;\n  if(val1 | 0 !== val1) {\n    if(Math.abs(val1 - val2) < .00000001) {\n      err = false;\n    }\n    else {\n      msg = 'eps';\n    }\n  }\n  else if(val1 === val2) {\n    err = false;\n  }\n\n  if(err) {\n    throw new Error(val1 + ' does not equal ' + val2);\n  }\n}\n\nfunction _print(/* arg1, arg2, ..., argN */) {\n    var func = ((typeof console !== 'undefined' && console.log) || print);\n    func.apply(null, arguments);\n}\n\nvar _time;\nfunction start() {\n  _time = Date.now();\n}\n\nfunction end() {\n  return Date.now() - _time;\n}\n\n{% finalize %}\n})();\n";
}).call(this, typeof exports === "undefined" ? (templates = {}) : exports);
