(() => {
  // output/Data.Argonaut.Core/foreign.js
  function id(x) {
    return x;
  }
  function stringify(j) {
    return JSON.stringify(j);
  }
  function stringifyWithIndent(i2) {
    return function(j) {
      return JSON.stringify(j, null, i2);
    };
  }
  function isArray(a2) {
    return Object.prototype.toString.call(a2) === "[object Array]";
  }
  function _caseJson(isNull3, isBool, isNum, isStr, isArr, isObj, j) {
    if (j == null)
      return isNull3();
    else if (typeof j === "boolean")
      return isBool(j);
    else if (typeof j === "number")
      return isNum(j);
    else if (typeof j === "string")
      return isStr(j);
    else if (Object.prototype.toString.call(j) === "[object Array]")
      return isArr(j);
    else
      return isObj(j);
  }
  function _compare(EQ2, GT2, LT2, a2, b2) {
    if (a2 == null) {
      if (b2 == null)
        return EQ2;
      else
        return LT2;
    } else if (typeof a2 === "boolean") {
      if (typeof b2 === "boolean") {
        if (a2 === b2)
          return EQ2;
        else if (a2 === false)
          return LT2;
        else
          return GT2;
      } else if (b2 == null)
        return GT2;
      else
        return LT2;
    } else if (typeof a2 === "number") {
      if (typeof b2 === "number") {
        if (a2 === b2)
          return EQ2;
        else if (a2 < b2)
          return LT2;
        else
          return GT2;
      } else if (b2 == null)
        return GT2;
      else if (typeof b2 === "boolean")
        return GT2;
      else
        return LT2;
    } else if (typeof a2 === "string") {
      if (typeof b2 === "string") {
        if (a2 === b2)
          return EQ2;
        else if (a2 < b2)
          return LT2;
        else
          return GT2;
      } else if (b2 == null)
        return GT2;
      else if (typeof b2 === "boolean")
        return GT2;
      else if (typeof b2 === "number")
        return GT2;
      else
        return LT2;
    } else if (isArray(a2)) {
      if (isArray(b2)) {
        for (var i2 = 0; i2 < Math.min(a2.length, b2.length); i2++) {
          var ca = _compare(EQ2, GT2, LT2, a2[i2], b2[i2]);
          if (ca !== EQ2)
            return ca;
        }
        if (a2.length === b2.length)
          return EQ2;
        else if (a2.length < b2.length)
          return LT2;
        else
          return GT2;
      } else if (b2 == null)
        return GT2;
      else if (typeof b2 === "boolean")
        return GT2;
      else if (typeof b2 === "number")
        return GT2;
      else if (typeof b2 === "string")
        return GT2;
      else
        return LT2;
    } else {
      if (b2 == null)
        return GT2;
      else if (typeof b2 === "boolean")
        return GT2;
      else if (typeof b2 === "number")
        return GT2;
      else if (typeof b2 === "string")
        return GT2;
      else if (isArray(b2))
        return GT2;
      else {
        var akeys = Object.keys(a2);
        var bkeys = Object.keys(b2);
        if (akeys.length < bkeys.length)
          return LT2;
        else if (akeys.length > bkeys.length)
          return GT2;
        var keys4 = akeys.concat(bkeys).sort();
        for (var j = 0; j < keys4.length; j++) {
          var k = keys4[j];
          if (a2[k] === void 0)
            return LT2;
          else if (b2[k] === void 0)
            return GT2;
          var ck = _compare(EQ2, GT2, LT2, a2[k], b2[k]);
          if (ck !== EQ2)
            return ck;
        }
        return EQ2;
      }
    }
  }

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqCharImpl = refEq;
  var eqStringImpl = refEq;

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label5) {
    return function(rec) {
      return rec[label5];
    };
  };

  // output/Data.Eq/index.js
  var eqUnit = {
    eq: function(v) {
      return function(v1) {
        return true;
      };
    }
  };
  var eqString = {
    eq: eqStringImpl
  };
  var eqNumber = {
    eq: eqNumberImpl
  };
  var eqInt = {
    eq: eqIntImpl
  };
  var eqChar = {
    eq: eqCharImpl
  };
  var eq1 = function(dict) {
    return dict.eq1;
  };
  var eq = function(dict) {
    return dict.eq;
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Function/index.js
  var flip = function(f) {
    return function(b2) {
      return function(a2) {
        return f(a2)(b2);
      };
    };
  };
  var $$const = function(a2) {
    return function(v) {
      return a2;
    };
  };

  // output/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(arr[i2]);
      }
      return result;
    };
  };

  // output/Data.Unit/foreign.js
  var unit = void 0;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map113 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map113($$const(x))(f);
      };
    };
  };
  var functorArray = {
    map: arrayMap
  };

  // output/Data.Semigroup/foreign.js
  var concatString = function(s1) {
    return function(s2) {
      return s1 + s2;
    };
  };
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupString = {
    append: concatString
  };
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Alt/index.js
  var altArray = {
    alt: /* @__PURE__ */ append(semigroupArray),
    Functor0: function() {
      return functorArray;
    }
  };
  var alt = function(dict) {
    return dict.alt;
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map36 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map36($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure13 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure13(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure13 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure13(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure13 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply2(pure13(f))(a2);
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq6) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq6 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();
  var eqOrdering = {
    eq: function(v) {
      return function(v1) {
        if (v instanceof LT && v1 instanceof LT) {
          return true;
        }
        ;
        if (v instanceof GT && v1 instanceof GT) {
          return true;
        }
        ;
        if (v instanceof EQ && v1 instanceof EQ) {
          return true;
        }
        ;
        return false;
      };
    }
  };

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };
  var add = function(dict) {
    return dict.add;
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordNumber = /* @__PURE__ */ function() {
    return {
      compare: ordNumberImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqNumber;
      }
    };
  }();
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();
  var ordChar = /* @__PURE__ */ function() {
    return {
      compare: ordCharImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqChar;
      }
    };
  }();
  var compare1 = function(dict) {
    return dict.compare1;
  };
  var compare = function(dict) {
    return dict.compare;
  };

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedNumber = {
    top: topNumber,
    bottom: bottomNumber,
    Ord0: function() {
      return ordNumber;
    }
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var boundedChar = {
    top: topChar,
    bottom: bottomChar,
    Ord0: function() {
      return ordChar;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i2) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i2 + 1;
        var empty8 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty8;
      }
    ) + '"';
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };

  // output/Data.Maybe/index.js
  var identity3 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
  var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
  var functorMaybe = {
    map: function(v) {
      return function(v1) {
        if (v1 instanceof Just) {
          return new Just(v(v1.value0));
        }
        ;
        return Nothing.value;
      };
    }
  };
  var map2 = /* @__PURE__ */ map(functorMaybe);
  var fromMaybe = function(a2) {
    return maybe(a2)(identity3);
  };
  var fromJust = function() {
    return function(v) {
      if (v instanceof Just) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
    };
  };
  var applyMaybe = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return map2(v.value0)(v1);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorMaybe;
    }
  };
  var bindMaybe = {
    bind: function(v) {
      return function(v1) {
        if (v instanceof Just) {
          return v1(v.value0);
        }
        ;
        if (v instanceof Nothing) {
          return Nothing.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Apply0: function() {
      return applyMaybe;
    }
  };

  // output/Foreign.Object/foreign.js
  function _copyST(m) {
    return function() {
      var r = {};
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r[k] = m[k];
        }
      }
      return r;
    };
  }
  var empty = {};
  function runST(f) {
    return f();
  }
  function _lookup(no, yes, k, m) {
    return k in m ? yes(m[k]) : no;
  }
  function toArrayWithKey(f) {
    return function(m) {
      var r = [];
      for (var k in m) {
        if (hasOwnProperty.call(m, k)) {
          r.push(f(k)(m[k]));
        }
      }
      return r;
    };
  }
  var keys = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Control.Bind/index.js
  var discard = function(dict) {
    return dict.discard;
  };
  var bind = function(dict) {
    return dict.bind;
  };
  var bindFlipped = function(dictBind) {
    return flip(bind(dictBind));
  };
  var composeKleisliFlipped = function(dictBind) {
    var bindFlipped12 = bindFlipped(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bindFlipped12(f)(g(a2));
        };
      };
    };
  };
  var composeKleisli = function(dictBind) {
    var bind15 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind15(f(a2))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
  };

  // output/Control.Monad.ST.Internal/foreign.js
  var map_ = function(f) {
    return function(a2) {
      return function() {
        return f(a2());
      };
    };
  };
  var pure_ = function(a2) {
    return function() {
      return a2;
    };
  };
  var bind_ = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };
  var foreach = function(as) {
    return function(f) {
      return function() {
        for (var i2 = 0, l = as.length; i2 < l; i2++) {
          f(as[i2])();
        }
      };
    };
  };

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind7 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind7(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind7 = bind(dictMonad.Bind1());
    var pure10 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind7(f)(function(f$prime) {
          return bind7(a2)(function(a$prime) {
            return pure10(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Either/index.js
  var Left = /* @__PURE__ */ function() {
    function Left2(value0) {
      this.value0 = value0;
    }
    ;
    Left2.create = function(value0) {
      return new Left2(value0);
    };
    return Left2;
  }();
  var Right = /* @__PURE__ */ function() {
    function Right2(value0) {
      this.value0 = value0;
    }
    ;
    Right2.create = function(value0) {
      return new Right2(value0);
    };
    return Right2;
  }();
  var note = function(a2) {
    return maybe(new Left(a2))(Right.create);
  };
  var functorEither = {
    map: function(f) {
      return function(m) {
        if (m instanceof Left) {
          return new Left(m.value0);
        }
        ;
        if (m instanceof Right) {
          return new Right(f(m.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
      };
    }
  };
  var map3 = /* @__PURE__ */ map(functorEither);
  var either = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return v(v2.value0);
        }
        ;
        if (v2 instanceof Right) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var applyEither = {
    apply: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return new Left(v.value0);
        }
        ;
        if (v instanceof Right) {
          return map3(v.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };
  var bindEither = {
    bind: /* @__PURE__ */ either(function(e) {
      return function(v) {
        return new Left(e);
      };
    })(function(a2) {
      return function(f) {
        return f(a2);
      };
    }),
    Apply0: function() {
      return applyEither;
    }
  };
  var applicativeEither = /* @__PURE__ */ function() {
    return {
      pure: Right.create,
      Apply0: function() {
        return applyEither;
      }
    };
  }();
  var altEither = {
    alt: function(v) {
      return function(v1) {
        if (v instanceof Left) {
          return v1;
        }
        ;
        return v;
      };
    },
    Functor0: function() {
      return functorEither;
    }
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };
  var div = function(dict) {
    return dict.div;
  };

  // output/Data.Monoid/index.js
  var monoidString = {
    mempty: "",
    Semigroup0: function() {
      return semigroupString;
    }
  };
  var mempty = function(dict) {
    return dict.mempty;
  };

  // output/Effect/foreign.js
  var pureE = function(a2) {
    return function() {
      return a2;
    };
  };
  var bindE = function(a2) {
    return function(f) {
      return function() {
        return f(a2())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Effect.Ref/foreign.js
  var _new = function(val) {
    return function() {
      return { value: val };
    };
  };
  var read = function(ref2) {
    return function() {
      return ref2.value;
    };
  };
  var modifyImpl = function(f) {
    return function(ref2) {
      return function() {
        var t = f(ref2.value);
        ref2.value = t.state;
        return t.value;
      };
    };
  };
  var write = function(val) {
    return function(ref2) {
      return function() {
        ref2.value = val;
      };
    };
  };

  // output/Effect.Ref/index.js
  var $$void2 = /* @__PURE__ */ $$void(functorEffect);
  var $$new = _new;
  var modify$prime = modifyImpl;
  var modify = function(f) {
    return modify$prime(function(s) {
      var s$prime = f(s);
      return {
        state: s$prime,
        value: s$prime
      };
    });
  };
  var modify_ = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map4 = /* @__PURE__ */ map(functorEffect);
  var Loop = /* @__PURE__ */ function() {
    function Loop2(value0) {
      this.value0 = value0;
    }
    ;
    Loop2.create = function(value0) {
      return new Loop2(value0);
    };
    return Loop2;
  }();
  var Done = /* @__PURE__ */ function() {
    function Done2(value0) {
      this.value0 = value0;
    }
    ;
    Done2.create = function(value0) {
      return new Done2(value0);
    };
    return Done2;
  }();
  var tailRecM = function(dict) {
    return dict.tailRecM;
  };
  var monadRecEffect = {
    tailRecM: function(f) {
      return function(a2) {
        var fromDone = function(v) {
          if (v instanceof Done) {
            return v.value0;
          }
          ;
          throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
        };
        return function __do2() {
          var r = bindFlipped2($$new)(f(a2))();
          (function() {
            while (!function __do3() {
              var v = read(r)();
              if (v instanceof Loop) {
                var e = f(v.value0)();
                write(e)(r)();
                return false;
              }
              ;
              if (v instanceof Done) {
                return true;
              }
              ;
              throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
            }()) {
            }
            ;
            return {};
          })();
          return map4(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
    }
  };

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy2 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var functorST = {
    map: map_
  };
  var monadST = {
    Applicative0: function() {
      return applicativeST;
    },
    Bind1: function() {
      return bindST;
    }
  };
  var bindST = {
    bind: bind_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var applicativeST = {
    pure: pure_,
    Apply0: function() {
      return $lazy_applyST(0);
    }
  };
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy2("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value14) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value14);
  };
  var replicatePolyfill = function(count, value14) {
    var result = [];
    var n = 0;
    for (var i2 = 0; i2 < count; i2++) {
      result[n++] = value14;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons2(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head3) {
      return function(tail2) {
        return new Cons2(head3, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr6, xs) {
      return listToArray(foldr6(curryCons)(emptyList)(xs));
    };
  }();
  var length = function(xs) {
    return xs.length;
  };
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i2 = 0, l = xs.length; i2 < l; i2++) {
      if (f(xs[i2]))
        return just(i2);
    }
    return nothing;
  };
  var _deleteAt = function(just, nothing, i2, l) {
    if (i2 < 0 || i2 >= l.length)
      return nothing;
    var l1 = l.slice();
    l1.splice(i2, 1);
    return just(l1);
  };
  var reverse = function(l) {
    return l.slice().reverse();
  };
  var sortByImpl = function() {
    function mergeFromTo(compare6, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, mid, to);
      i2 = from3;
      j = mid;
      k = from3;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare6(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare6, fromOrdering, xs) {
      var out;
      if (xs.length < 2)
        return xs;
      out = xs.slice(0);
      mergeFromTo(compare6, fromOrdering, out, xs.slice(0), 0, xs.length);
      return out;
    };
  }();

  // output/Control.Lazy/index.js
  var lazyFn = {
    defer: function(f) {
      return function(x) {
        return f(unit)(x);
      };
    }
  };
  var defer = function(dict) {
    return dict.defer;
  };

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare6, fromOrdering, xs1, xs2, from3, to) {
      var mid;
      var i2;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from3 + (to - from3 >> 1);
      if (mid - from3 > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, from3, mid);
      if (to - mid > 1)
        mergeFromTo(compare6, fromOrdering, xs2, xs1, mid, to);
      i2 = from3;
      j = mid;
      k = from3;
      while (i2 < mid && j < to) {
        x = xs2[i2];
        y = xs2[j];
        c = fromOrdering(compare6(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i2;
        }
      }
      while (i2 < mid) {
        xs1[k++] = xs2[i2++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare6, fromOrdering, xs) {
      if (xs.length < 2)
        return xs;
      mergeFromTo(compare6, fromOrdering, xs, xs.slice(0), 0, xs.length);
      return xs;
    };
  }();

  // output/Data.HeytingAlgebra/foreign.js
  var boolConj = function(b1) {
    return function(b2) {
      return b1 && b2;
    };
  };
  var boolDisj = function(b1) {
    return function(b2) {
      return b1 || b2;
    };
  };
  var boolNot = function(b2) {
    return !b2;
  };

  // output/Data.HeytingAlgebra/index.js
  var tt = function(dict) {
    return dict.tt;
  };
  var not = function(dict) {
    return dict.not;
  };
  var implies = function(dict) {
    return dict.implies;
  };
  var ff = function(dict) {
    return dict.ff;
  };
  var disj = function(dict) {
    return dict.disj;
  };
  var heytingAlgebraBoolean = {
    ff: false,
    tt: true,
    implies: function(a2) {
      return function(b2) {
        return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
      };
    },
    conj: boolConj,
    disj: boolDisj,
    not: boolNot
  };
  var conj = function(dict) {
    return dict.conj;
  };
  var heytingAlgebraFunction = function(dictHeytingAlgebra) {
    var ff1 = ff(dictHeytingAlgebra);
    var tt1 = tt(dictHeytingAlgebra);
    var implies1 = implies(dictHeytingAlgebra);
    var conj1 = conj(dictHeytingAlgebra);
    var disj1 = disj(dictHeytingAlgebra);
    var not1 = not(dictHeytingAlgebra);
    return {
      ff: function(v) {
        return ff1;
      },
      tt: function(v) {
        return tt1;
      },
      implies: function(f) {
        return function(g) {
          return function(a2) {
            return implies1(f(a2))(g(a2));
          };
        };
      },
      conj: function(f) {
        return function(g) {
          return function(a2) {
            return conj1(f(a2))(g(a2));
          };
        };
      },
      disj: function(f) {
        return function(g) {
          return function(a2) {
            return disj1(f(a2))(g(a2));
          };
        };
      },
      not: function(f) {
        return function(a2) {
          return not1(f(a2));
        };
      }
    };
  };

  // output/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = len - 1; i2 >= 0; i2--) {
          acc = f(xs[i2])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init3) {
      return function(xs) {
        var acc = init3;
        var len = xs.length;
        for (var i2 = 0; i2 < len; i2++) {
          acc = f(acc)(xs[i2]);
        }
        return acc;
      };
    };
  };

  // output/Control.Plus/index.js
  var plusArray = {
    empty: [],
    Alt0: function() {
      return altArray;
    }
  };
  var empty2 = function(dict) {
    return dict.empty;
  };

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var uncurry = function(f) {
    return function(v) {
      return f(v.value0)(v.value1);
    };
  };
  var snd = function(v) {
    return v.value1;
  };
  var functorTuple = {
    map: function(f) {
      return function(m) {
        return new Tuple(m.value0, f(m.value1));
      };
    }
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Data.Bifunctor/index.js
  var bimap = function(dict) {
    return dict.bimap;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var wrap = function() {
    return coerce2;
  };
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Foldable/index.js
  var foldr = function(dict) {
    return dict.foldr;
  };
  var traverse_ = function(dictApplicative) {
    var applySecond2 = applySecond(dictApplicative.Apply0());
    var pure10 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure10(unit));
      };
    };
  };
  var for_ = function(dictApplicative) {
    var traverse_14 = traverse_(dictApplicative);
    return function(dictFoldable) {
      return flip(traverse_14(dictFoldable));
    };
  };
  var foldl = function(dict) {
    return dict.foldl;
  };
  var intercalate = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictMonoid) {
      var append11 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(sep) {
        return function(xs) {
          var go2 = function(v) {
            return function(v1) {
              if (v.init) {
                return {
                  init: false,
                  acc: v1
                };
              }
              ;
              return {
                init: false,
                acc: append11(v.acc)(append11(sep)(v1))
              };
            };
          };
          return foldl22(go2)({
            init: true,
            acc: mempty2
          })(xs).acc;
        };
      };
    };
  };
  var foldableMaybe = {
    foldr: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v2.value0)(v1);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldl: function(v) {
      return function(v1) {
        return function(v2) {
          if (v2 instanceof Nothing) {
            return v1;
          }
          ;
          if (v2 instanceof Just) {
            return v(v1)(v2.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      return function(v) {
        return function(v1) {
          if (v1 instanceof Nothing) {
            return mempty2;
          }
          ;
          if (v1 instanceof Just) {
            return v(v1.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
        };
      };
    }
  };
  var foldMapDefaultR = function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(dictMonoid) {
      var append11 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append11(f(x))(acc);
          };
        })(mempty2);
      };
    };
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: function(dictMonoid) {
      return foldMapDefaultR(foldableArray)(dictMonoid);
    }
  };
  var foldMap = function(dict) {
    return dict.foldMap;
  };

  // output/Data.Function.Uncurried/foreign.js
  var runFn2 = function(fn) {
    return function(a2) {
      return function(b2) {
        return fn(a2, b2);
      };
    };
  };
  var runFn4 = function(fn) {
    return function(a2) {
      return function(b2) {
        return function(c) {
          return function(d) {
            return fn(a2, b2, c, d);
          };
        };
      };
    };
  };

  // output/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i2 = 0; i2 < l; i2++) {
        result[i2] = f(i2)(xs[i2]);
      }
      return result;
    };
  };

  // output/Data.FunctorWithIndex/index.js
  var mapWithIndex = function(dict) {
    return dict.mapWithIndex;
  };
  var functorWithIndexArray = {
    mapWithIndex: mapWithIndexArray,
    Functor0: function() {
      return functorArray;
    }
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a2) {
      return [a2];
    }
    function array2(a2) {
      return function(b2) {
        return [a2, b2];
      };
    }
    function array3(a2) {
      return function(b2) {
        return function(c) {
          return [a2, b2, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map36) {
        return function(pure10) {
          return function(f) {
            return function(array) {
              function go2(bot, top5) {
                switch (top5 - bot) {
                  case 0:
                    return pure10([]);
                  case 1:
                    return map36(array1)(f(array[bot]));
                  case 2:
                    return apply2(map36(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map36(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top5 - bot) / 4) * 2;
                    return apply2(map36(concat2)(go2(bot, pivot)))(go2(pivot, top5));
                }
              }
              return go2(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Traversable/index.js
  var identity4 = /* @__PURE__ */ identity(categoryFn);
  var traverse = function(dict) {
    return dict.traverse;
  };
  var sequenceDefault = function(dictTraversable) {
    var traverse22 = traverse(dictTraversable);
    return function(dictApplicative) {
      return traverse22(dictApplicative)(identity4);
    };
  };
  var traversableArray = {
    traverse: function(dictApplicative) {
      var Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
    },
    sequence: function(dictApplicative) {
      return sequenceDefault(traversableArray)(dictApplicative);
    },
    Functor0: function() {
      return functorArray;
    },
    Foldable1: function() {
      return foldableArray;
    }
  };

  // output/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var maybe2 = f(value14);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust5(maybe2);
                result.push(fst2(tuple));
                value14 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust5) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b2) {
              var result = [];
              var value14 = b2;
              while (true) {
                var tuple = f(value14);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value14 = fromJust5(maybe2);
              }
            };
          };
        };
      };
    };
  };

  // output/Data.Semigroup.Foldable/index.js
  var foldr1 = function(dict) {
    return dict.foldr1;
  };
  var foldl1 = function(dict) {
    return dict.foldl1;
  };
  var foldMap1DefaultL = function(dictFoldable1) {
    var foldl11 = foldl1(dictFoldable1);
    return function(dictFunctor) {
      var map36 = map(dictFunctor);
      return function(dictSemigroup) {
        var append11 = append(dictSemigroup);
        return function(f) {
          var $162 = foldl11(append11);
          var $163 = map36(f);
          return function($164) {
            return $162($163($164));
          };
        };
      };
    };
  };
  var foldMap1 = function(dict) {
    return dict.foldMap1;
  };

  // output/Data.Unfoldable1/index.js
  var fromJust2 = /* @__PURE__ */ fromJust();
  var unfoldr1 = function(dict) {
    return dict.unfoldr1;
  };
  var unfoldable1Array = {
    unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust2)(fst)(snd)
  };

  // output/Data.Unfoldable/index.js
  var fromJust3 = /* @__PURE__ */ fromJust();
  var unfoldr = function(dict) {
    return dict.unfoldr;
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust3)(fst)(snd),
    Unfoldable10: function() {
      return unfoldable1Array;
    }
  };

  // output/Data.Array/index.js
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var singleton2 = function(a2) {
    return [a2];
  };
  var replicate = /* @__PURE__ */ runFn2(replicateImpl);
  var fromFoldable = function(dictFoldable) {
    return runFn2(fromFoldableImpl)(foldr(dictFoldable));
  };
  var findIndex = /* @__PURE__ */ function() {
    return runFn4(findIndexImpl)(Just.create)(Nothing.value);
  }();
  var deleteAt = /* @__PURE__ */ function() {
    return runFn4(_deleteAt)(Just.create)(Nothing.value);
  }();
  var deleteBy = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2.length === 0) {
          return [];
        }
        ;
        return maybe(v2)(function(i2) {
          return fromJust4(deleteAt(i2)(v2));
        })(findIndex(v(v1))(v2));
      };
    };
  };
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };

  // output/Data.FoldableWithIndex/index.js
  var foldr8 = /* @__PURE__ */ foldr(foldableArray);
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var foldl8 = /* @__PURE__ */ foldl(foldableArray);
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
    var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
    return function(dictMonoid) {
      var append11 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldrWithIndex1(function(i2) {
          return function(x) {
            return function(acc) {
              return append11(f(i2)(x))(acc);
            };
          };
        })(mempty2);
      };
    };
  };
  var foldableWithIndexArray = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $291 = foldr8(function(v) {
          return function(y) {
            return f(v.value0)(v.value1)(y);
          };
        })(z);
        var $292 = mapWithIndex2(Tuple.create);
        return function($293) {
          return $291($292($293));
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $294 = foldl8(function(y) {
          return function(v) {
            return f(v.value0)(y)(v.value1);
          };
        })(z);
        var $295 = mapWithIndex2(Tuple.create);
        return function($296) {
          return $294($295($296));
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
    },
    Foldable0: function() {
      return foldableArray;
    }
  };
  var foldMapWithIndex = function(dict) {
    return dict.foldMapWithIndex;
  };

  // output/Foreign.Object.ST/foreign.js
  var newImpl = function() {
    return {};
  };
  function poke2(k) {
    return function(v) {
      return function(m) {
        return function() {
          m[k] = v;
          return m;
        };
      };
    };
  }

  // output/Foreign.Object/index.js
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindST);
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var thawST = _copyST;
  var singleton3 = function(k) {
    return function(v) {
      return runST(bindFlipped3(poke2(k)(v))(newImpl));
    };
  };
  var mutate = function(f) {
    return function(m) {
      return runST(function __do2() {
        var s = thawST(m)();
        f(s)();
        return s;
      });
    };
  };
  var lookup = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();
  var insert = function(k) {
    return function(v) {
      return mutate(poke2(k)(v));
    };
  };
  var fromFoldable2 = function(dictFoldable) {
    var fromFoldable15 = fromFoldable(dictFoldable);
    return function(l) {
      return runST(function __do2() {
        var s = newImpl();
        foreach(fromFoldable15(l))(function(v) {
          return $$void3(poke2(v.value0)(v.value1)(s));
        })();
        return s;
      });
    };
  };

  // output/Data.Argonaut.Core/index.js
  var eq2 = /* @__PURE__ */ eq(eqOrdering);
  var verbJsonType = function(def) {
    return function(f) {
      return function(g) {
        return g(def)(f);
      };
    };
  };
  var toJsonType = /* @__PURE__ */ function() {
    return verbJsonType(Nothing.value)(Just.create);
  }();
  var jsonSingletonObject = function(key) {
    return function(val) {
      return id(singleton3(key)(val));
    };
  };
  var jsonEmptyObject = /* @__PURE__ */ id(empty);
  var isJsonType = /* @__PURE__ */ verbJsonType(false)(/* @__PURE__ */ $$const(true));
  var ordJson = {
    compare: function(a2) {
      return function(b2) {
        return _compare(EQ.value, GT.value, LT.value, a2, b2);
      };
    },
    Eq0: function() {
      return eqJson;
    }
  };
  var eqJson = {
    eq: function(j1) {
      return function(j2) {
        return eq2(compare(ordJson)(j1)(j2))(EQ.value);
      };
    }
  };
  var caseJsonString = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), f, $$const(d), $$const(d), j);
      };
    };
  };
  var toString = /* @__PURE__ */ toJsonType(caseJsonString);
  var caseJsonObject = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), $$const(d), f, j);
      };
    };
  };
  var toObject = /* @__PURE__ */ toJsonType(caseJsonObject);
  var caseJsonNumber = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), f, $$const(d), $$const(d), $$const(d), j);
      };
    };
  };
  var toNumber = /* @__PURE__ */ toJsonType(caseJsonNumber);
  var caseJsonNull = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson(f, $$const(d), $$const(d), $$const(d), $$const(d), $$const(d), j);
      };
    };
  };
  var isNull = /* @__PURE__ */ isJsonType(caseJsonNull);
  var caseJsonBoolean = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), f, $$const(d), $$const(d), $$const(d), $$const(d), j);
      };
    };
  };
  var toBoolean = /* @__PURE__ */ toJsonType(caseJsonBoolean);
  var caseJsonArray = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), f, $$const(d), j);
      };
    };
  };
  var toArray = /* @__PURE__ */ toJsonType(caseJsonArray);
  var caseJson = function(a2) {
    return function(b2) {
      return function(c) {
        return function(d) {
          return function(e) {
            return function(f) {
              return function(json) {
                return _caseJson(a2, b2, c, d, e, f, json);
              };
            };
          };
        };
      };
    };
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var foldr1Impl = function(f, xs) {
    var acc = xs[xs.length - 1];
    for (var i2 = xs.length - 2; i2 >= 0; i2--) {
      acc = f(xs[i2])(acc);
    }
    return acc;
  };
  var foldl1Impl = function(f, xs) {
    var acc = xs[0];
    var len = xs.length;
    for (var i2 = 1; i2 < len; i2++) {
      acc = f(acc)(xs[i2]);
    }
    return acc;
  };
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    };
    function finalCell(head3) {
      return new ConsCell(head3, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply2, map36, f) {
      var buildFrom = function(x, ys) {
        return apply2(map36(consList)(f(x)))(ys);
      };
      var go2 = function(acc, currentLen, xs) {
        if (currentLen === 0) {
          return acc;
        } else {
          var last3 = xs[currentLen - 1];
          return new Cont(function() {
            var built = go2(buildFrom(last3, acc), currentLen - 1, xs);
            return built;
          });
        }
      };
      return function(array) {
        var acc = map36(finalCell)(f(array[array.length - 1]));
        var result = go2(acc, array.length - 1, array);
        while (result instanceof Cont) {
          result = result.fn();
        }
        return map36(listToArray)(result);
      };
    };
  }();

  // output/Data.Array.NonEmpty.Internal/index.js
  var NonEmptyArray = function(x) {
    return x;
  };
  var semigroupNonEmptyArray = semigroupArray;
  var functorNonEmptyArray = functorArray;
  var foldableNonEmptyArray = foldableArray;
  var foldable1NonEmptyArray = {
    foldMap1: function(dictSemigroup) {
      return foldMap1DefaultL(foldable1NonEmptyArray)(functorNonEmptyArray)(dictSemigroup);
    },
    foldr1: /* @__PURE__ */ runFn2(foldr1Impl),
    foldl1: /* @__PURE__ */ runFn2(foldl1Impl),
    Foldable0: function() {
      return foldableNonEmptyArray;
    }
  };

  // output/Data.NonEmpty/index.js
  var map5 = /* @__PURE__ */ map(functorTuple);
  var map1 = /* @__PURE__ */ map(functorMaybe);
  var NonEmpty = /* @__PURE__ */ function() {
    function NonEmpty2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    NonEmpty2.create = function(value0) {
      return function(value1) {
        return new NonEmpty2(value0, value1);
      };
    };
    return NonEmpty2;
  }();
  var unfoldable1NonEmpty = function(dictUnfoldable) {
    var unfoldr3 = unfoldr(dictUnfoldable);
    return {
      unfoldr1: function(f) {
        return function(b2) {
          return uncurry(NonEmpty.create)(map5(unfoldr3(map1(f)))(f(b2)));
        };
      }
    };
  };
  var singleton4 = function(dictPlus) {
    var empty8 = empty2(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty8);
    };
  };
  var functorNonEmpty = function(dictFunctor) {
    var map211 = map(dictFunctor);
    return {
      map: function(f) {
        return function(m) {
          return new NonEmpty(f(m.value0), map211(f)(m.value1));
        };
      }
    };
  };
  var foldableNonEmpty = function(dictFoldable) {
    var foldMap4 = foldMap(dictFoldable);
    var foldl6 = foldl(dictFoldable);
    var foldr6 = foldr(dictFoldable);
    return {
      foldMap: function(dictMonoid) {
        var append16 = append(dictMonoid.Semigroup0());
        var foldMap15 = foldMap4(dictMonoid);
        return function(f) {
          return function(v) {
            return append16(f(v.value0))(foldMap15(f)(v.value1));
          };
        };
      },
      foldl: function(f) {
        return function(b2) {
          return function(v) {
            return foldl6(f)(f(b2)(v.value0))(v.value1);
          };
        };
      },
      foldr: function(f) {
        return function(b2) {
          return function(v) {
            return f(v.value0)(foldr6(f)(b2)(v.value1));
          };
        };
      }
    };
  };
  var foldable1NonEmpty = function(dictFoldable) {
    var foldl6 = foldl(dictFoldable);
    var foldr6 = foldr(dictFoldable);
    var foldableNonEmpty1 = foldableNonEmpty(dictFoldable);
    return {
      foldMap1: function(dictSemigroup) {
        var append16 = append(dictSemigroup);
        return function(f) {
          return function(v) {
            return foldl6(function(s) {
              return function(a1) {
                return append16(s)(f(a1));
              };
            })(f(v.value0))(v.value1);
          };
        };
      },
      foldr1: function(f) {
        return function(v) {
          return maybe(v.value0)(f(v.value0))(foldr6(function(a1) {
            var $250 = maybe(a1)(f(a1));
            return function($251) {
              return Just.create($250($251));
            };
          })(Nothing.value)(v.value1));
        };
      },
      foldl1: function(f) {
        return function(v) {
          return foldl6(f)(v.value0)(v.value1);
        };
      },
      Foldable0: function() {
        return foldableNonEmpty1;
      }
    };
  };

  // output/Data.Array.NonEmpty/index.js
  var unsafeFromArray = NonEmptyArray;
  var toArray2 = function(v) {
    return v;
  };
  var singleton5 = function($110) {
    return unsafeFromArray(singleton2($110));
  };
  var fromFoldable1 = function(dictFoldable1) {
    var $117 = fromFoldable(dictFoldable1.Foldable0());
    return function($118) {
      return unsafeFromArray($117($118));
    };
  };
  var fromArray = function(xs) {
    if (length(xs) > 0) {
      return new Just(unsafeFromArray(xs));
    }
    ;
    if (otherwise) {
      return Nothing.value;
    }
    ;
    throw new Error("Failed pattern match at Data.Array.NonEmpty (line 161, column 1 - line 161, column 58): " + [xs.constructor.name]);
  };
  var fromFoldable3 = function(dictFoldable) {
    var $119 = fromFoldable(dictFoldable);
    return function($120) {
      return fromArray($119($120));
    };
  };
  var cons$prime = function(x) {
    return function(xs) {
      return unsafeFromArray(cons(x)(xs));
    };
  };
  var fromNonEmpty = function(v) {
    return cons$prime(v.value0)(v.value1);
  };
  var adaptAny = function(f) {
    return function($128) {
      return f(toArray2($128));
    };
  };
  var unsafeAdapt = function(f) {
    var $129 = adaptAny(f);
    return function($130) {
      return unsafeFromArray($129($130));
    };
  };
  var reverse2 = /* @__PURE__ */ unsafeAdapt(reverse);

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber2 = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var trunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
  };

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber2(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber2(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var trunc2 = function($38) {
    return unsafeClamp(trunc($38));
  };

  // output/Data.List.Types/index.js
  var Nil = /* @__PURE__ */ function() {
    function Nil2() {
    }
    ;
    Nil2.value = new Nil2();
    return Nil2;
  }();
  var Cons = /* @__PURE__ */ function() {
    function Cons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Cons2.create = function(value0) {
      return function(value1) {
        return new Cons2(value0, value1);
      };
    };
    return Cons2;
  }();
  var NonEmptyList = function(x) {
    return x;
  };
  var toList = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var listMap = function(f) {
    var chunkedRevMap = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
            $tco_var_v = new Cons(v1, v);
            $copy_v1 = v1.value1.value1.value1;
            return;
          }
          ;
          var unrolledMap = function(v2) {
            if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
              return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
            }
            ;
            if (v2 instanceof Cons && v2.value1 instanceof Nil) {
              return new Cons(f(v2.value0), Nil.value);
            }
            ;
            return Nil.value;
          };
          var reverseUnrolledMap = function($copy_v2) {
            return function($copy_v3) {
              var $tco_var_v2 = $copy_v2;
              var $tco_done1 = false;
              var $tco_result2;
              function $tco_loop2(v2, v3) {
                if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                  $tco_var_v2 = v2.value1;
                  $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                  return;
                }
                ;
                $tco_done1 = true;
                return v3;
              }
              ;
              while (!$tco_done1) {
                $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
              }
              ;
              return $tco_result2;
            };
          };
          $tco_done = true;
          return reverseUnrolledMap(v)(unrolledMap(v1));
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return chunkedRevMap(Nil.value);
  };
  var functorList = {
    map: listMap
  };
  var functorNonEmptyList = /* @__PURE__ */ functorNonEmpty(functorList);
  var foldableList = {
    foldr: function(f) {
      return function(b2) {
        var rev3 = function() {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return v;
                }
                ;
                if (v1 instanceof Cons) {
                  $tco_var_v = new Cons(v1.value0, v);
                  $copy_v1 = v1.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(Nil.value);
        }();
        var $284 = foldl(foldableList)(flip(f))(b2);
        return function($285) {
          return $284(rev3($285));
        };
      };
    },
    foldl: function(f) {
      var go2 = function($copy_b) {
        return function($copy_v) {
          var $tco_var_b = $copy_b;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(b2, v) {
            if (v instanceof Nil) {
              $tco_done1 = true;
              return b2;
            }
            ;
            if (v instanceof Cons) {
              $tco_var_b = f(b2)(v.value0);
              $copy_v = v.value1;
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_b, $copy_v);
          }
          ;
          return $tco_result;
        };
      };
      return go2;
    },
    foldMap: function(dictMonoid) {
      var append23 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append23(acc);
          return function($287) {
            return $286(f($287));
          };
        })(mempty2);
      };
    }
  };
  var foldl2 = /* @__PURE__ */ foldl(foldableList);
  var foldr2 = /* @__PURE__ */ foldr(foldableList);
  var foldableNonEmptyList = /* @__PURE__ */ foldableNonEmpty(foldableList);
  var semigroupList = {
    append: function(xs) {
      return function(ys) {
        return foldr2(Cons.create)(ys)(xs);
      };
    }
  };
  var append1 = /* @__PURE__ */ append(semigroupList);
  var semigroupNonEmptyList = {
    append: function(v) {
      return function(as$prime) {
        return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
      };
    }
  };
  var unfoldable1List = {
    unfoldr1: function(f) {
      return function(b2) {
        var go2 = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source2, memo) {
              var v = f(source2);
              if (v.value1 instanceof Just) {
                $tco_var_source = v.value1.value0;
                $copy_memo = new Cons(v.value0, memo);
                return;
              }
              ;
              if (v.value1 instanceof Nothing) {
                $tco_done = true;
                return foldl2(flip(Cons.create))(Nil.value)(new Cons(v.value0, memo));
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 135, column 22 - line 137, column 61): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go2(b2)(Nil.value);
      };
    }
  };
  var unfoldableList = {
    unfoldr: function(f) {
      return function(b2) {
        var go2 = function($copy_source) {
          return function($copy_memo) {
            var $tco_var_source = $copy_source;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(source2, memo) {
              var v = f(source2);
              if (v instanceof Nothing) {
                $tco_done = true;
                return foldl2(flip(Cons.create))(Nil.value)(memo);
              }
              ;
              if (v instanceof Just) {
                $tco_var_source = v.value0.value1;
                $copy_memo = new Cons(v.value0.value0, memo);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 142, column 22 - line 144, column 52): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_source, $copy_memo);
            }
            ;
            return $tco_result;
          };
        };
        return go2(b2)(Nil.value);
      };
    },
    Unfoldable10: function() {
      return unfoldable1List;
    }
  };
  var unfoldable1NonEmptyList = /* @__PURE__ */ unfoldable1NonEmpty(unfoldableList);
  var foldable1NonEmptyList = /* @__PURE__ */ foldable1NonEmpty(foldableList);
  var eq1List = {
    eq1: function(dictEq) {
      var eq6 = eq(dictEq);
      return function(xs) {
        return function(ys) {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              return function($copy_v2) {
                var $tco_var_v = $copy_v;
                var $tco_var_v1 = $copy_v1;
                var $tco_done = false;
                var $tco_result;
                function $tco_loop(v, v1, v2) {
                  if (!v2) {
                    $tco_done = true;
                    return false;
                  }
                  ;
                  if (v instanceof Nil && v1 instanceof Nil) {
                    $tco_done = true;
                    return v2;
                  }
                  ;
                  if (v instanceof Cons && v1 instanceof Cons) {
                    $tco_var_v = v.value1;
                    $tco_var_v1 = v1.value1;
                    $copy_v2 = v2 && eq6(v1.value0)(v.value0);
                    return;
                  }
                  ;
                  $tco_done = true;
                  return false;
                }
                ;
                while (!$tco_done) {
                  $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
                }
                ;
                return $tco_result;
              };
            };
          };
          return go2(xs)(ys)(true);
        };
      };
    }
  };
  var eq12 = /* @__PURE__ */ eq1(eq1List);
  var eqList = function(dictEq) {
    return {
      eq: eq12(dictEq)
    };
  };
  var ord1List = {
    compare1: function(dictOrd) {
      var compare6 = compare(dictOrd);
      return function(xs) {
        return function(ys) {
          var go2 = function($copy_v) {
            return function($copy_v1) {
              var $tco_var_v = $copy_v;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1) {
                if (v instanceof Nil && v1 instanceof Nil) {
                  $tco_done = true;
                  return EQ.value;
                }
                ;
                if (v instanceof Nil) {
                  $tco_done = true;
                  return LT.value;
                }
                ;
                if (v1 instanceof Nil) {
                  $tco_done = true;
                  return GT.value;
                }
                ;
                if (v instanceof Cons && v1 instanceof Cons) {
                  var v2 = compare6(v.value0)(v1.value0);
                  if (v2 instanceof EQ) {
                    $tco_var_v = v.value1;
                    $copy_v1 = v1.value1;
                    return;
                  }
                  ;
                  $tco_done = true;
                  return v2;
                }
                ;
                throw new Error("Failed pattern match at Data.List.Types (line 60, column 5 - line 60, column 20): " + [v.constructor.name, v1.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $copy_v1);
              }
              ;
              return $tco_result;
            };
          };
          return go2(xs)(ys);
        };
      };
    },
    Eq10: function() {
      return eq1List;
    }
  };
  var compare12 = /* @__PURE__ */ compare1(ord1List);
  var ordList = function(dictOrd) {
    var eqList1 = eqList(dictOrd.Eq0());
    return {
      compare: compare12(dictOrd),
      Eq0: function() {
        return eqList1;
      }
    };
  };
  var altList = {
    alt: append1,
    Functor0: function() {
      return functorList;
    }
  };
  var plusList = /* @__PURE__ */ function() {
    return {
      empty: Nil.value,
      Alt0: function() {
        return altList;
      }
    };
  }();

  // output/Data.List/index.js
  var map6 = /* @__PURE__ */ map(functorMaybe);
  var foldr3 = /* @__PURE__ */ foldr(foldableList);
  var uncons2 = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just({
        head: v.value0,
        tail: v.value1
      });
    }
    ;
    throw new Error("Failed pattern match at Data.List (line 259, column 1 - line 259, column 66): " + [v.constructor.name]);
  };
  var toUnfoldable2 = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(xs) {
      return map6(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons2(xs));
    });
  };
  var snoc2 = function(xs) {
    return function(x) {
      return foldr3(Cons.create)(new Cons(x, Nil.value))(xs);
    };
  };
  var reverse3 = /* @__PURE__ */ function() {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v1 instanceof Nil) {
            $tco_done = true;
            return v;
          }
          ;
          if (v1 instanceof Cons) {
            $tco_var_v = new Cons(v1.value0, v);
            $copy_v1 = v1.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return go2(Nil.value);
  }();
  var unsnoc2 = function(lst) {
    var go2 = function($copy_v) {
      return function($copy_v1) {
        var $tco_var_v = $copy_v;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v, v1) {
          if (v instanceof Nil) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Cons && v.value1 instanceof Nil) {
            $tco_done = true;
            return new Just({
              revInit: v1,
              last: v.value0
            });
          }
          ;
          if (v instanceof Cons) {
            $tco_var_v = v.value1;
            $copy_v1 = new Cons(v.value0, v1);
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List (line 270, column 3 - line 270, column 21): " + [v.constructor.name, v1.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_v, $copy_v1);
        }
        ;
        return $tco_result;
      };
    };
    return map6(function(h) {
      return {
        init: reverse3(h.revInit),
        last: h.last
      };
    })(go2(lst)(Nil.value));
  };
  var $$null = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var fromFoldable4 = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };
  var dropWhile2 = function(p2) {
    var go2 = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Cons && p2(v.value0)) {
          $copy_v = v.value1;
          return;
        }
        ;
        $tco_done = true;
        return v;
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return go2;
  };

  // output/Partial.Unsafe/foreign.js
  var _unsafePartial = function(f) {
    return f();
  };

  // output/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output/Partial/index.js
  var crashWith = function() {
    return _crashWith;
  };

  // output/Partial.Unsafe/index.js
  var crashWith2 = /* @__PURE__ */ crashWith();
  var unsafePartial = _unsafePartial;
  var unsafeCrashWith = function(msg) {
    return unsafePartial(function() {
      return crashWith2(msg);
    });
  };

  // output/Data.List.NonEmpty/index.js
  var append12 = /* @__PURE__ */ append(semigroupList);
  var wrappedOperation = function(name15) {
    return function(f) {
      return function(v) {
        var v1 = f(new Cons(v.value0, v.value1));
        if (v1 instanceof Cons) {
          return new NonEmpty(v1.value0, v1.value1);
        }
        ;
        if (v1 instanceof Nil) {
          return unsafeCrashWith("Impossible: empty list in NonEmptyList " + name15);
        }
        ;
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 92, column 3 - line 94, column 81): " + [v1.constructor.name]);
      };
    };
  };
  var unsnoc3 = function(v) {
    var v1 = unsnoc2(v.value1);
    if (v1 instanceof Nothing) {
      return {
        init: Nil.value,
        last: v.value0
      };
    }
    ;
    if (v1 instanceof Just) {
      return {
        init: new Cons(v.value0, v1.value0.init),
        last: v1.value0.last
      };
    }
    ;
    throw new Error("Failed pattern match at Data.List.NonEmpty (line 160, column 35 - line 162, column 50): " + [v1.constructor.name]);
  };
  var uncons3 = function(v) {
    return {
      head: v.value0,
      tail: v.value1
    };
  };
  var toList2 = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var singleton6 = /* @__PURE__ */ function() {
    var $200 = singleton4(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var snoc$prime = function(v) {
    return function(v1) {
      if (v instanceof Cons) {
        return new NonEmpty(v.value0, snoc2(v.value1)(v1));
      }
      ;
      if (v instanceof Nil) {
        return singleton6(v1);
      }
      ;
      throw new Error("Failed pattern match at Data.List.NonEmpty (line 140, column 1 - line 140, column 51): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var reverse4 = /* @__PURE__ */ wrappedOperation("reverse")(reverse3);
  var fromList = function(v) {
    if (v instanceof Nil) {
      return Nothing.value;
    }
    ;
    if (v instanceof Cons) {
      return new Just(new NonEmpty(v.value0, v.value1));
    }
    ;
    throw new Error("Failed pattern match at Data.List.NonEmpty (line 121, column 1 - line 121, column 57): " + [v.constructor.name]);
  };
  var cons$prime2 = function(x) {
    return function(xs) {
      return new NonEmpty(x, xs);
    };
  };
  var cons2 = function(y) {
    return function(v) {
      return new NonEmpty(y, new Cons(v.value0, v.value1));
    };
  };
  var appendFoldable = function(dictFoldable) {
    var fromFoldable15 = fromFoldable4(dictFoldable);
    return function(v) {
      return function(ys) {
        return new NonEmpty(v.value0, append12(v.value1)(fromFoldable15(ys)));
      };
    };
  };

  // output/Data.Map.Internal/index.js
  var $runtime_lazy3 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Leaf = /* @__PURE__ */ function() {
    function Leaf2() {
    }
    ;
    Leaf2.value = new Leaf2();
    return Leaf2;
  }();
  var Node = /* @__PURE__ */ function() {
    function Node2(value0, value1, value22, value32, value42, value52) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
      this.value4 = value42;
      this.value5 = value52;
    }
    ;
    Node2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return function(value42) {
              return function(value52) {
                return new Node2(value0, value1, value22, value32, value42, value52);
              };
            };
          };
        };
      };
    };
    return Node2;
  }();
  var IterLeaf = /* @__PURE__ */ function() {
    function IterLeaf2() {
    }
    ;
    IterLeaf2.value = new IterLeaf2();
    return IterLeaf2;
  }();
  var IterEmit = /* @__PURE__ */ function() {
    function IterEmit2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    IterEmit2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new IterEmit2(value0, value1, value22);
        };
      };
    };
    return IterEmit2;
  }();
  var IterNode = /* @__PURE__ */ function() {
    function IterNode2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    IterNode2.create = function(value0) {
      return function(value1) {
        return new IterNode2(value0, value1);
      };
    };
    return IterNode2;
  }();
  var IterDone = /* @__PURE__ */ function() {
    function IterDone2() {
    }
    ;
    IterDone2.value = new IterDone2();
    return IterDone2;
  }();
  var IterNext = /* @__PURE__ */ function() {
    function IterNext2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    IterNext2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new IterNext2(value0, value1, value22);
        };
      };
    };
    return IterNext2;
  }();
  var Split = /* @__PURE__ */ function() {
    function Split2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Split2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Split2(value0, value1, value22);
        };
      };
    };
    return Split2;
  }();
  var SplitLast = /* @__PURE__ */ function() {
    function SplitLast2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    SplitLast2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new SplitLast2(value0, value1, value22);
        };
      };
    };
    return SplitLast2;
  }();
  var unsafeNode = function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return new Node(1, 1, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 680, column 5 - line 684, column 39): " + [r.constructor.name]);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Leaf) {
        return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
      }
      ;
      if (r instanceof Node) {
        return new Node(1 + function() {
          var $277 = l.value0 > r.value0;
          if ($277) {
            return l.value0;
          }
          ;
          return r.value0;
        }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 686, column 5 - line 690, column 68): " + [r.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 678, column 32 - line 690, column 68): " + [l.constructor.name]);
  };
  var toMapIter = /* @__PURE__ */ function() {
    return flip(IterNode.create)(IterLeaf.value);
  }();
  var stepWith = function(f) {
    return function(next) {
      return function(done) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v instanceof IterLeaf) {
              $tco_done = true;
              return done(unit);
            }
            ;
            if (v instanceof IterEmit) {
              $tco_done = true;
              return next(v.value0, v.value1, v.value2);
            }
            ;
            if (v instanceof IterNode) {
              $copy_v = f(v.value1)(v.value0);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 918, column 8 - line 924, column 20): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2;
      };
    };
  };
  var singleton7 = function(k) {
    return function(v) {
      return new Node(1, 1, k, v, Leaf.value, Leaf.value);
    };
  };
  var unsafeBalancedNode = /* @__PURE__ */ function() {
    var height8 = function(v) {
      if (v instanceof Leaf) {
        return 0;
      }
      ;
      if (v instanceof Node) {
        return v.value0;
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 735, column 12 - line 737, column 26): " + [v.constructor.name]);
    };
    var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
      if (rl instanceof Node && rl.value0 > height8(rr)) {
        return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
      }
      ;
      return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
    };
    var rotateRight = function(k, v, lk, lv, ll, lr, r) {
      if (lr instanceof Node && height8(ll) <= lr.value0) {
        return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
      }
      ;
      return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
    };
    return function(k, v, l, r) {
      if (l instanceof Leaf) {
        if (r instanceof Leaf) {
          return singleton7(k)(v);
        }
        ;
        if (r instanceof Node && r.value0 > 1) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      if (l instanceof Node) {
        if (r instanceof Node) {
          if (r.value0 > (l.value0 + 1 | 0)) {
            return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
          }
          ;
          if (l.value0 > (r.value0 + 1 | 0)) {
            return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
          }
          ;
        }
        ;
        if (r instanceof Leaf && l.value0 > 1) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
        return unsafeNode(k, v, l, r);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 695, column 40 - line 716, column 34): " + [l.constructor.name]);
    };
  }();
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy3("unsafeSplit", "Data.Map.Internal", function() {
    return function(comp, k, m) {
      if (m instanceof Leaf) {
        return new Split(Nothing.value, Leaf.value, Leaf.value);
      }
      ;
      if (m instanceof Node) {
        var v = comp(k)(m.value2);
        if (v instanceof LT) {
          var v1 = $lazy_unsafeSplit(771)(comp, k, m.value4);
          return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
        }
        ;
        if (v instanceof GT) {
          var v1 = $lazy_unsafeSplit(774)(comp, k, m.value5);
          return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
        }
        ;
        if (v instanceof EQ) {
          return new Split(new Just(m.value3), m.value4, m.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 769, column 5 - line 777, column 30): " + [v.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 765, column 34 - line 777, column 30): " + [m.constructor.name]);
    };
  });
  var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(764);
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy3("unsafeSplitLast", "Data.Map.Internal", function() {
    return function(k, v, l, r) {
      if (r instanceof Leaf) {
        return new SplitLast(k, v, l);
      }
      ;
      if (r instanceof Node) {
        var v1 = $lazy_unsafeSplitLast(757)(r.value2, r.value3, r.value4, r.value5);
        return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 754, column 37 - line 758, column 57): " + [r.constructor.name]);
    };
  });
  var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(753);
  var unsafeJoinNodes = function(v, v1) {
    if (v instanceof Leaf) {
      return v1;
    }
    ;
    if (v instanceof Node) {
      var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
      return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 742, column 25 - line 746, column 38): " + [v.constructor.name, v1.constructor.name]);
  };
  var $lazy_unsafeUnionWith = /* @__PURE__ */ $runtime_lazy3("unsafeUnionWith", "Data.Map.Internal", function() {
    return function(comp, app, l, r) {
      if (l instanceof Leaf) {
        return r;
      }
      ;
      if (r instanceof Leaf) {
        return l;
      }
      ;
      if (r instanceof Node) {
        var v = unsafeSplit(comp, r.value2, l);
        var l$prime = $lazy_unsafeUnionWith(787)(comp, app, v.value1, r.value4);
        var r$prime = $lazy_unsafeUnionWith(788)(comp, app, v.value2, r.value5);
        if (v.value0 instanceof Just) {
          return unsafeBalancedNode(r.value2, app(v.value0.value0)(r.value3), l$prime, r$prime);
        }
        ;
        if (v.value0 instanceof Nothing) {
          return unsafeBalancedNode(r.value2, r.value3, l$prime, r$prime);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 789, column 5 - line 793, column 46): " + [v.value0.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 782, column 42 - line 793, column 46): " + [l.constructor.name, r.constructor.name]);
    };
  });
  var unsafeUnionWith = /* @__PURE__ */ $lazy_unsafeUnionWith(781);
  var unionWith = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(app) {
      return function(m1) {
        return function(m2) {
          return unsafeUnionWith(compare6, app, m1, m2);
        };
      };
    };
  };
  var union2 = function(dictOrd) {
    return unionWith(dictOrd)($$const);
  };
  var member = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return false;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare6(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return true;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 457, column 7 - line 460, column 19): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 454, column 8 - line 460, column 19): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var lookup2 = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return Nothing.value;
          }
          ;
          if (v instanceof Node) {
            var v1 = compare6(k)(v.value2);
            if (v1 instanceof LT) {
              $copy_v = v.value4;
              return;
            }
            ;
            if (v1 instanceof GT) {
              $copy_v = v.value5;
              return;
            }
            ;
            if (v1 instanceof EQ) {
              $tco_done = true;
              return new Just(v.value3);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 281, column 7 - line 284, column 22): " + [v1.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 278, column 8 - line 284, column 22): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2;
    };
  };
  var iterMapL = /* @__PURE__ */ function() {
    var go2 = function($copy_iter) {
      return function($copy_v) {
        var $tco_var_iter = $copy_iter;
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(iter, v) {
          if (v instanceof Leaf) {
            $tco_done = true;
            return iter;
          }
          ;
          if (v instanceof Node) {
            if (v.value5 instanceof Leaf) {
              $tco_var_iter = new IterEmit(v.value2, v.value3, iter);
              $copy_v = v.value4;
              return;
            }
            ;
            $tco_var_iter = new IterEmit(v.value2, v.value3, new IterNode(v.value5, iter));
            $copy_v = v.value4;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 929, column 13 - line 936, column 48): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($tco_var_iter, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2;
  }();
  var stepAscCps = /* @__PURE__ */ stepWith(iterMapL);
  var stepAsc = /* @__PURE__ */ function() {
    return stepAscCps(function(k, v, next) {
      return new IterNext(k, v, next);
    })($$const(IterDone.value));
  }();
  var eqMapIter = function(dictEq) {
    var eq15 = eq(dictEq);
    return function(dictEq1) {
      var eq23 = eq(dictEq1);
      return {
        eq: function() {
          var go2 = function($copy_a) {
            return function($copy_b) {
              var $tco_var_a = $copy_a;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(a2, b2) {
                var v = stepAsc(a2);
                if (v instanceof IterNext) {
                  var v2 = stepAsc(b2);
                  if (v2 instanceof IterNext && (eq15(v.value0)(v2.value0) && eq23(v.value1)(v2.value1))) {
                    $tco_var_a = v.value2;
                    $copy_b = v2.value2;
                    return;
                  }
                  ;
                  $tco_done = true;
                  return false;
                }
                ;
                if (v instanceof IterDone) {
                  $tco_done = true;
                  return true;
                }
                ;
                throw new Error("Failed pattern match at Data.Map.Internal (line 837, column 14 - line 846, column 13): " + [v.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_a, $copy_b);
              }
              ;
              return $tco_result;
            };
          };
          return go2;
        }()
      };
    };
  };
  var isEmpty = function(v) {
    if (v instanceof Leaf) {
      return true;
    }
    ;
    return false;
  };
  var insertWith = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(app) {
      return function(k) {
        return function(v) {
          var go2 = function(v1) {
            if (v1 instanceof Leaf) {
              return singleton7(k)(v);
            }
            ;
            if (v1 instanceof Node) {
              var v2 = compare6(k)(v1.value2);
              if (v2 instanceof LT) {
                return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
              }
              ;
              if (v2 instanceof GT) {
                return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
              }
              ;
              if (v2 instanceof EQ) {
                return new Node(v1.value0, v1.value1, k, app(v1.value3)(v), v1.value4, v1.value5);
              }
              ;
              throw new Error("Failed pattern match at Data.Map.Internal (line 484, column 7 - line 487, column 44): " + [v2.constructor.name]);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 481, column 8 - line 487, column 44): " + [v1.constructor.name]);
          };
          return go2;
        };
      };
    };
  };
  var insert3 = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton7(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare6(k)(v1.value2);
            if (v2 instanceof LT) {
              return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
            }
            ;
            if (v2 instanceof GT) {
              return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
            }
            ;
            if (v2 instanceof EQ) {
              return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 469, column 7 - line 472, column 35): " + [v2.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 466, column 8 - line 472, column 35): " + [v1.constructor.name]);
        };
        return go2;
      };
    };
  };
  var functorMap = {
    map: function(f) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          return new Node(v.value0, v.value1, v.value2, f(v.value3), go2(v.value4), go2(v.value5));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 145, column 10 - line 148, column 39): " + [v.constructor.name]);
      };
      return go2;
    }
  };
  var foldableMap = {
    foldr: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(170)(m$prime.value4, f(m$prime.value3)($lazy_go(170)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 167, column 26 - line 170, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(167);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldl: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(176)(f($lazy_go(176)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 173, column 26 - line 176, column 43): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(173);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMap: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append16 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append16(go2(v.value4))(append16(f(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 179, column 10 - line 182, column 28): " + [v.constructor.name]);
        };
        return go2;
      };
    }
  };
  var foldableWithIndexMap = {
    foldrWithIndex: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
          return function(m$prime, z$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(190)(m$prime.value4, f(m$prime.value2)(m$prime.value3)($lazy_go(190)(m$prime.value5, z$prime)));
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 187, column 26 - line 190, column 45): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(187);
        return function(m) {
          return go2(m, z);
        };
      };
    },
    foldlWithIndex: function(f) {
      return function(z) {
        var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
          return function(z$prime, m$prime) {
            if (m$prime instanceof Leaf) {
              return z$prime;
            }
            ;
            if (m$prime instanceof Node) {
              return $lazy_go(196)(f(m$prime.value2)($lazy_go(196)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 193, column 26 - line 196, column 45): " + [m$prime.constructor.name]);
          };
        });
        var go2 = $lazy_go(193);
        return function(m) {
          return go2(z, m);
        };
      };
    },
    foldMapWithIndex: function(dictMonoid) {
      var mempty2 = mempty(dictMonoid);
      var append16 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append16(go2(v.value4))(append16(f(v.value2)(v.value3))(go2(v.value5)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 199, column 10 - line 202, column 30): " + [v.constructor.name]);
        };
        return go2;
      };
    },
    Foldable0: function() {
      return foldableMap;
    }
  };
  var keys2 = /* @__PURE__ */ function() {
    return foldrWithIndex(foldableWithIndexMap)(function(k) {
      return function(v) {
        return function(acc) {
          return new Cons(k, acc);
        };
      };
    })(Nil.value);
  }();
  var filterWithKey = function(dictOrd) {
    return function(f) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          if (f(v.value2)(v.value3)) {
            return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), go2(v.value5));
          }
          ;
          if (otherwise) {
            return unsafeJoinNodes(go2(v.value4), go2(v.value5));
          }
          ;
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 625, column 8 - line 631, column 47): " + [v.constructor.name]);
      };
      return go2;
    };
  };
  var filter3 = function(dictOrd) {
    var $769 = filterWithKey(dictOrd);
    return function($770) {
      return $769($$const($770));
    };
  };
  var eqMap = function(dictEq) {
    var eqMapIter1 = eqMapIter(dictEq);
    return function(dictEq1) {
      var eq15 = eq(eqMapIter1(dictEq1));
      return {
        eq: function(xs) {
          return function(ys) {
            if (xs instanceof Leaf) {
              if (ys instanceof Leaf) {
                return true;
              }
              ;
              return false;
            }
            ;
            if (xs instanceof Node) {
              if (ys instanceof Node && xs.value1 === ys.value1) {
                return eq15(toMapIter(xs))(toMapIter(ys));
              }
              ;
              return false;
            }
            ;
            throw new Error("Failed pattern match at Data.Map.Internal (line 92, column 14 - line 103, column 16): " + [xs.constructor.name]);
          };
        }
      };
    };
  };
  var empty3 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var $$delete2 = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(k) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare6(k)(v.value2);
          if (v1 instanceof LT) {
            return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
          }
          ;
          if (v1 instanceof GT) {
            return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
          }
          ;
          if (v1 instanceof EQ) {
            return unsafeJoinNodes(v.value4, v.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 496, column 7 - line 499, column 43): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 493, column 8 - line 499, column 43): " + [v.constructor.name]);
      };
      return go2;
    };
  };
  var alter = function(dictOrd) {
    var compare6 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare6, k, m);
          var v2 = f(v.value0);
          if (v2 instanceof Nothing) {
            return unsafeJoinNodes(v.value1, v.value2);
          }
          ;
          if (v2 instanceof Just) {
            return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 512, column 3 - line 516, column 41): " + [v2.constructor.name]);
        };
      };
    };
  };

  // output/Data.Set/index.js
  var coerce3 = /* @__PURE__ */ coerce();
  var foldMap2 = /* @__PURE__ */ foldMap(foldableList);
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var foldr4 = /* @__PURE__ */ foldr(foldableList);
  var $$Set = function(x) {
    return x;
  };
  var union3 = function(dictOrd) {
    return coerce3(union2(dictOrd));
  };
  var toMap = function(v) {
    return v;
  };
  var toList3 = function(v) {
    return keys2(v);
  };
  var toUnfoldable3 = function(dictUnfoldable) {
    var $96 = toUnfoldable2(dictUnfoldable);
    return function($97) {
      return $96(toList3($97));
    };
  };
  var singleton8 = function(a2) {
    return singleton7(a2)(unit);
  };
  var semigroupSet = function(dictOrd) {
    return {
      append: union3(dictOrd)
    };
  };
  var member2 = function(dictOrd) {
    return coerce3(member(dictOrd));
  };
  var isEmpty2 = /* @__PURE__ */ coerce3(isEmpty);
  var insert4 = function(dictOrd) {
    var insert12 = insert3(dictOrd);
    return function(a2) {
      return function(v) {
        return insert12(a2)(unit)(v);
      };
    };
  };
  var fromMap = $$Set;
  var foldableSet = {
    foldMap: function(dictMonoid) {
      var foldMap15 = foldMap2(dictMonoid);
      return function(f) {
        var $98 = foldMap15(f);
        return function($99) {
          return $98(toList3($99));
        };
      };
    },
    foldl: function(f) {
      return function(x) {
        var $100 = foldl3(f)(x);
        return function($101) {
          return $100(toList3($101));
        };
      };
    },
    foldr: function(f) {
      return function(x) {
        var $102 = foldr4(f)(x);
        return function($103) {
          return $102(toList3($103));
        };
      };
    }
  };
  var eqSet = function(dictEq) {
    var eq6 = eq(eqMap(dictEq)(eqUnit));
    return {
      eq: function(v) {
        return function(v1) {
          return eq6(v)(v1);
        };
      }
    };
  };
  var ordSet = function(dictOrd) {
    var compare6 = compare(ordList(dictOrd));
    var eqSet1 = eqSet(dictOrd.Eq0());
    return {
      compare: function(s1) {
        return function(s2) {
          return compare6(toList3(s1))(toList3(s2));
        };
      },
      Eq0: function() {
        return eqSet1;
      }
    };
  };
  var empty4 = empty3;
  var fromFoldable5 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      var insert12 = insert4(dictOrd);
      return foldl22(function(m) {
        return function(a2) {
          return insert12(a2)(m);
        };
      })(empty4);
    };
  };
  var monoidSet = function(dictOrd) {
    var semigroupSet1 = semigroupSet(dictOrd);
    return {
      mempty: empty4,
      Semigroup0: function() {
        return semigroupSet1;
      }
    };
  };

  // output/Data.String.CodePoints/foreign.js
  var hasArrayFrom = typeof Array.from === "function";
  var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
  var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
  var hasCodePointAt = typeof String.prototype.codePointAt === "function";
  var _unsafeCodePointAt0 = function(fallback) {
    return hasCodePointAt ? function(str) {
      return str.codePointAt(0);
    } : fallback;
  };
  var _singleton = function(fallback) {
    return hasFromCodePoint ? String.fromCodePoint : fallback;
  };
  var _take = function(fallback) {
    return function(n) {
      if (hasStringIterator) {
        return function(str) {
          var accum = "";
          var iter = str[Symbol.iterator]();
          for (var i2 = 0; i2 < n; ++i2) {
            var o = iter.next();
            if (o.done)
              return accum;
            accum += o.value;
          }
          return accum;
        };
      }
      return fallback(n);
    };
  };
  var _toCodePointArray = function(fallback) {
    return function(unsafeCodePointAt02) {
      if (hasArrayFrom) {
        return function(str) {
          return Array.from(str, unsafeCodePointAt02);
        };
      }
      return fallback;
    };
  };

  // output/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output/Data.Enum/index.js
  var bottom1 = /* @__PURE__ */ bottom(boundedChar);
  var top1 = /* @__PURE__ */ top(boundedChar);
  var toEnum = function(dict) {
    return dict.toEnum;
  };
  var fromEnum = function(dict) {
    return dict.fromEnum;
  };
  var toEnumWithDefaults = function(dictBoundedEnum) {
    var toEnum1 = toEnum(dictBoundedEnum);
    var fromEnum1 = fromEnum(dictBoundedEnum);
    var bottom22 = bottom(dictBoundedEnum.Bounded0());
    return function(low2) {
      return function(high2) {
        return function(x) {
          var v = toEnum1(x);
          if (v instanceof Just) {
            return v.value0;
          }
          ;
          if (v instanceof Nothing) {
            var $140 = x < fromEnum1(bottom22);
            if ($140) {
              return low2;
            }
            ;
            return high2;
          }
          ;
          throw new Error("Failed pattern match at Data.Enum (line 158, column 33 - line 160, column 62): " + [v.constructor.name]);
        };
      };
    };
  };
  var defaultSucc = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) + 1 | 0);
      };
    };
  };
  var defaultPred = function(toEnum$prime) {
    return function(fromEnum$prime) {
      return function(a2) {
        return toEnum$prime(fromEnum$prime(a2) - 1 | 0);
      };
    };
  };
  var charToEnum = function(v) {
    if (v >= toCharCode(bottom1) && v <= toCharCode(top1)) {
      return new Just(fromCharCode(v));
    }
    ;
    return Nothing.value;
  };
  var enumChar = {
    succ: /* @__PURE__ */ defaultSucc(charToEnum)(toCharCode),
    pred: /* @__PURE__ */ defaultPred(charToEnum)(toCharCode),
    Ord0: function() {
      return ordChar;
    }
  };
  var boundedEnumChar = /* @__PURE__ */ function() {
    return {
      cardinality: toCharCode(top1) - toCharCode(bottom1) | 0,
      toEnum: charToEnum,
      fromEnum: toCharCode,
      Bounded0: function() {
        return boundedChar;
      },
      Enum1: function() {
        return enumChar;
      }
    };
  }();

  // output/Data.String.CodeUnits/foreign.js
  var singleton9 = function(c) {
    return c;
  };
  var length3 = function(s) {
    return s.length;
  };
  var _lastIndexOfStartingAt = function(just) {
    return function(nothing) {
      return function(x) {
        return function(startAt) {
          return function(s) {
            var i2 = s.lastIndexOf(x, startAt);
            return i2 === -1 ? nothing : just(i2);
          };
        };
      };
    };
  };
  var take3 = function(n) {
    return function(s) {
      return s.substr(0, n);
    };
  };
  var drop3 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt2 = function(i2) {
    return function(s) {
      return { before: s.substring(0, i2), after: s.substring(i2) };
    };
  };

  // output/Data.String.Unsafe/foreign.js
  var charAt = function(i2) {
    return function(s) {
      if (i2 >= 0 && i2 < s.length)
        return s.charAt(i2);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output/Data.String.CodeUnits/index.js
  var stripPrefix = function(v) {
    return function(str) {
      var v1 = splitAt2(length3(v))(str);
      var $20 = v1.before === v;
      if ($20) {
        return new Just(v1.after);
      }
      ;
      return Nothing.value;
    };
  };
  var lastIndexOf$prime = /* @__PURE__ */ function() {
    return _lastIndexOfStartingAt(Just.create)(Nothing.value);
  }();

  // output/Data.String.Common/foreign.js
  var replaceAll = function(s1) {
    return function(s2) {
      return function(s3) {
        return s3.replace(new RegExp(s1.replace(/[-\/\\^$*+?.()|[\]{}]/g, "\\$&"), "g"), s2);
      };
    };
  };
  var joinWith = function(s) {
    return function(xs) {
      return xs.join(s);
    };
  };

  // output/Data.String.Common/index.js
  var $$null2 = function(s) {
    return s === "";
  };

  // output/Data.String.CodePoints/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map7 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div2 = /* @__PURE__ */ div(euclideanRingInt);
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var unsurrogate = function(lead) {
    return function(trail) {
      return (((lead - 55296 | 0) * 1024 | 0) + (trail - 56320 | 0) | 0) + 65536 | 0;
    };
  };
  var isTrail = function(cu) {
    return 56320 <= cu && cu <= 57343;
  };
  var isLead = function(cu) {
    return 55296 <= cu && cu <= 56319;
  };
  var uncons4 = function(s) {
    var v = length3(s);
    if (v === 0) {
      return Nothing.value;
    }
    ;
    if (v === 1) {
      return new Just({
        head: fromEnum2(charAt(0)(s)),
        tail: ""
      });
    }
    ;
    var cu1 = fromEnum2(charAt(1)(s));
    var cu0 = fromEnum2(charAt(0)(s));
    var $43 = isLead(cu0) && isTrail(cu1);
    if ($43) {
      return new Just({
        head: unsurrogate(cu0)(cu1),
        tail: drop3(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop3(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map7(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons4(s));
  };
  var toCodePointArrayFallback = function(s) {
    return unfoldr2(unconsButWithTuple)(s);
  };
  var unsafeCodePointAt0Fallback = function(s) {
    var cu0 = fromEnum2(charAt(0)(s));
    var $47 = isLead(cu0) && length3(s) > 1;
    if ($47) {
      var cu1 = fromEnum2(charAt(1)(s));
      var $48 = isTrail(cu1);
      if ($48) {
        return unsurrogate(cu0)(cu1);
      }
      ;
      return cu0;
    }
    ;
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var length4 = function($74) {
    return length(toCodePointArray($74));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton9($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div2(v - 65536 | 0)(1024) + 55296 | 0;
    var trail = mod2(v - 65536 | 0)(1024) + 56320 | 0;
    return fromCharCode2(lead) + fromCharCode2(trail);
  };
  var singleton10 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = function(v) {
    return function(v1) {
      if (v < 1) {
        return "";
      }
      ;
      var v2 = uncons4(v1);
      if (v2 instanceof Just) {
        return singleton10(v2.value0.head) + takeFallback(v - 1 | 0)(v2.value0.tail);
      }
      ;
      return v1;
    };
  };
  var take4 = /* @__PURE__ */ _take(takeFallback);
  var lastIndexOf$prime2 = function(p2) {
    return function(i2) {
      return function(s) {
        var i$prime = length3(take4(i2)(s));
        return map7(function(k) {
          return length4(take3(k)(s));
        })(lastIndexOf$prime(p2)(i$prime)(s));
      };
    };
  };
  var splitAt3 = function(i2) {
    return function(s) {
      var before = take4(i2)(s);
      return {
        before,
        after: drop3(length3(before))(s)
      };
    };
  };

  // output/Data.String.NonEmpty.Internal/index.js
  var show2 = /* @__PURE__ */ show(showString);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindMaybe);
  var NonEmptyString = function(x) {
    return x;
  };
  var toString2 = function(v) {
    return v;
  };
  var showNonEmptyString = {
    show: function(v) {
      return "(NonEmptyString.unsafeFromString " + (show2(v) + ")");
    }
  };
  var semigroupNonEmptyString = semigroupString;
  var prependString = function(s1) {
    return function(v) {
      return s1 + v;
    };
  };
  var ordNonEmptyString = ordString;
  var nonEmptyNonEmpty = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return {
      nes: function(p2) {
        return reflectSymbol2(p2);
      }
    };
  };
  var nes = function(dict) {
    return dict.nes;
  };
  var liftS = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var joinWith2 = function(dictFoldable) {
    var intercalate4 = intercalate(dictFoldable)(monoidString);
    return function(splice) {
      var $61 = intercalate4(splice);
      return function($62) {
        return $61($62);
      };
    };
  };
  var join1With = function(dictFoldable1) {
    var joinWith22 = joinWith2(dictFoldable1.Foldable0());
    return function(splice) {
      var $63 = joinWith22(splice);
      return function($64) {
        return NonEmptyString($63($64));
      };
    };
  };
  var fromString = function(v) {
    if (v === "") {
      return Nothing.value;
    }
    ;
    return new Just(v);
  };
  var stripPrefix2 = function(pat) {
    return composeKleisliFlipped2(fromString)(liftS(stripPrefix(pat)));
  };
  var eqNonEmptyString = eqString;
  var appendString = function(v) {
    return function(s2) {
      return v + s2;
    };
  };

  // output/Data.Argonaut.Encode.Encoders/index.js
  var map8 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var toUnfoldable22 = /* @__PURE__ */ toUnfoldable3(unfoldableList);
  var extend2 = function(encoder) {
    return function(v) {
      var $40 = caseJsonObject(jsonSingletonObject(v.value0)(v.value1))(function() {
        var $42 = insert(v.value0)(v.value1);
        return function($43) {
          return id($42($43));
        };
      }());
      return function($41) {
        return $40(encoder($41));
      };
    };
  };
  var encodeList = function(encoder) {
    var $45 = map8(encoder);
    return function($46) {
      return id($45(toUnfoldable5($46)));
    };
  };
  var encodeSet = function(dictOrd) {
    return function(encoder) {
      var $51 = encodeList(encoder);
      return function($52) {
        return $51(toUnfoldable22($52));
      };
    };
  };
  var assoc = function(encoder) {
    return function(k) {
      var $64 = Tuple.create(k);
      return function($65) {
        return $64(encoder($65));
      };
    };
  };

  // output/Record/index.js
  var get = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function() {
      return function(l) {
        return function(r) {
          return unsafeGet(reflectSymbol2(l))(r);
        };
      };
    };
  };

  // output/Data.Argonaut.Encode.Class/index.js
  var gEncodeJsonNil = {
    gEncodeJson: function(v) {
      return function(v1) {
        return empty;
      };
    }
  };
  var gEncodeJson = function(dict) {
    return dict.gEncodeJson;
  };
  var encodeRecord = function(dictGEncodeJson) {
    var gEncodeJson1 = gEncodeJson(dictGEncodeJson);
    return function() {
      return {
        encodeJson: function(rec) {
          return id(gEncodeJson1(rec)($$Proxy.value));
        }
      };
    };
  };
  var encodeJsonJson = {
    encodeJson: /* @__PURE__ */ identity(categoryFn)
  };
  var encodeJson = function(dict) {
    return dict.encodeJson;
  };
  var encodeJsonList = function(dictEncodeJson) {
    return {
      encodeJson: encodeList(encodeJson(dictEncodeJson))
    };
  };
  var encodeSet2 = function(dictOrd) {
    var encodeSet1 = encodeSet(dictOrd);
    return function(dictEncodeJson) {
      return {
        encodeJson: encodeSet1(encodeJson(dictEncodeJson))
      };
    };
  };
  var gEncodeJsonCons = function(dictEncodeJson) {
    var encodeJson12 = encodeJson(dictEncodeJson);
    return function(dictGEncodeJson) {
      var gEncodeJson1 = gEncodeJson(dictGEncodeJson);
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        var get3 = get(dictIsSymbol)();
        return function() {
          return {
            gEncodeJson: function(row) {
              return function(v) {
                return insert(reflectSymbol2($$Proxy.value))(encodeJson12(get3($$Proxy.value)(row)))(gEncodeJson1(row)($$Proxy.value));
              };
            }
          };
        };
      };
    };
  };

  // output/Data.String.NonEmpty.CodePoints/index.js
  var liftS2 = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var lastIndexOf$prime3 = function(pat) {
    var $24 = lastIndexOf$prime2(pat);
    return function($25) {
      return liftS2($24($25));
    };
  };
  var fromNonEmptyString = function(v) {
    return v;
  };
  var length5 = function($30) {
    return length4(fromNonEmptyString($30));
  };
  var splitAt4 = function(i2) {
    return function(nes16) {
      var v = splitAt3(i2)(fromNonEmptyString(nes16));
      return {
        before: fromString(v.before),
        after: fromString(v.after)
      };
    };
  };

  // output/Data.Markdown/index.js
  var map9 = /* @__PURE__ */ map(functorNonEmptyArray);
  var append3 = /* @__PURE__ */ append(semigroupNonEmptyList);
  var append13 = /* @__PURE__ */ append(semigroupString);
  var fromFoldable6 = /* @__PURE__ */ fromFoldable4(foldableArray);
  var appendFoldable2 = /* @__PURE__ */ appendFoldable(foldableList);
  var unfoldr12 = /* @__PURE__ */ unfoldr1(unfoldable1NonEmptyList);
  var map12 = /* @__PURE__ */ map(functorNonEmptyList);
  var foldl4 = /* @__PURE__ */ foldl(foldableNonEmptyArray);
  var defer2 = /* @__PURE__ */ defer(lazyFn);
  var map22 = /* @__PURE__ */ map(functorArray);
  var fromFoldable12 = /* @__PURE__ */ fromFoldable(foldableNonEmptyList);
  var foldMap12 = /* @__PURE__ */ foldMap1(foldable1NonEmptyArray)(semigroupNonEmptyList);
  var map32 = /* @__PURE__ */ map(functorList);
  var top3 = /* @__PURE__ */ top(boundedInt);
  var append22 = /* @__PURE__ */ append(semigroupList);
  var fromFoldable22 = /* @__PURE__ */ fromFoldable(foldableList);
  var foldl12 = /* @__PURE__ */ foldl(foldableList);
  var L1 = /* @__PURE__ */ function() {
    function L12() {
    }
    ;
    L12.value = new L12();
    return L12;
  }();
  var L2 = /* @__PURE__ */ function() {
    function L22() {
    }
    ;
    L22.value = new L22();
    return L22;
  }();
  var L3 = /* @__PURE__ */ function() {
    function L32() {
    }
    ;
    L32.value = new L32();
    return L32;
  }();
  var L4 = /* @__PURE__ */ function() {
    function L42() {
    }
    ;
    L42.value = new L42();
    return L42;
  }();
  var L5 = /* @__PURE__ */ function() {
    function L52() {
    }
    ;
    L52.value = new L52();
    return L52;
  }();
  var L6 = /* @__PURE__ */ function() {
    function L62() {
    }
    ;
    L62.value = new L62();
    return L62;
  }();
  var ExternalUrl = /* @__PURE__ */ function() {
    function ExternalUrl2(value0) {
      this.value0 = value0;
    }
    ;
    ExternalUrl2.create = function(value0) {
      return new ExternalUrl2(value0);
    };
    return ExternalUrl2;
  }();
  var InternalUrl = /* @__PURE__ */ function() {
    function InternalUrl2(value0) {
      this.value0 = value0;
    }
    ;
    InternalUrl2.create = function(value0) {
      return new InternalUrl2(value0);
    };
    return InternalUrl2;
  }();
  var Emphasis = /* @__PURE__ */ function() {
    function Emphasis2(value0) {
      this.value0 = value0;
    }
    ;
    Emphasis2.create = function(value0) {
      return new Emphasis2(value0);
    };
    return Emphasis2;
  }();
  var LineBreak = /* @__PURE__ */ function() {
    function LineBreak2() {
    }
    ;
    LineBreak2.value = new LineBreak2();
    return LineBreak2;
  }();
  var Link = /* @__PURE__ */ function() {
    function Link2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Link2.create = function(value0) {
      return function(value1) {
        return new Link2(value0, value1);
      };
    };
    return Link2;
  }();
  var InlineCode = /* @__PURE__ */ function() {
    function InlineCode2(value0) {
      this.value0 = value0;
    }
    ;
    InlineCode2.create = function(value0) {
      return new InlineCode2(value0);
    };
    return InlineCode2;
  }();
  var Text = /* @__PURE__ */ function() {
    function Text3(value0) {
      this.value0 = value0;
    }
    ;
    Text3.create = function(value0) {
      return new Text3(value0);
    };
    return Text3;
  }();
  var Json = /* @__PURE__ */ function() {
    function Json3() {
    }
    ;
    Json3.value = new Json3();
    return Json3;
  }();
  var Mermaid = /* @__PURE__ */ function() {
    function Mermaid2() {
    }
    ;
    Mermaid2.value = new Mermaid2();
    return Mermaid2;
  }();
  var PlainText = /* @__PURE__ */ function() {
    function PlainText2() {
    }
    ;
    PlainText2.value = new PlainText2();
    return PlainText2;
  }();
  var Blockquote = /* @__PURE__ */ function() {
    function Blockquote2(value0) {
      this.value0 = value0;
    }
    ;
    Blockquote2.create = function(value0) {
      return new Blockquote2(value0);
    };
    return Blockquote2;
  }();
  var CodeBlock = /* @__PURE__ */ function() {
    function CodeBlock2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CodeBlock2.create = function(value0) {
      return function(value1) {
        return new CodeBlock2(value0, value1);
      };
    };
    return CodeBlock2;
  }();
  var Heading = /* @__PURE__ */ function() {
    function Heading2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Heading2.create = function(value0) {
      return function(value1) {
        return new Heading2(value0, value1);
      };
    };
    return Heading2;
  }();
  var List = /* @__PURE__ */ function() {
    function List2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    List2.create = function(value0) {
      return function(value1) {
        return new List2(value0, value1);
      };
    };
    return List2;
  }();
  var Paragraph = /* @__PURE__ */ function() {
    function Paragraph2(value0) {
      this.value0 = value0;
    }
    ;
    Paragraph2.create = function(value0) {
      return new Paragraph2(value0);
    };
    return Paragraph2;
  }();
  var Rule = /* @__PURE__ */ function() {
    function Rule2() {
    }
    ;
    Rule2.value = new Rule2();
    return Rule2;
  }();
  var FlowContent = /* @__PURE__ */ function() {
    function FlowContent2(value0) {
      this.value0 = value0;
    }
    ;
    FlowContent2.create = function(value0) {
      return new FlowContent2(value0);
    };
    return FlowContent2;
  }();
  var PhrasingContent = /* @__PURE__ */ function() {
    function PhrasingContent2(value0) {
      this.value0 = value0;
    }
    ;
    PhrasingContent2.create = function(value0) {
      return new PhrasingContent2(value0);
    };
    return PhrasingContent2;
  }();
  var unorderedList = function(dictFoldable1) {
    var fromFoldable11 = fromFoldable1(dictFoldable1);
    var $674 = List.create(false);
    var $675 = map9(fromFoldable11);
    return function($676) {
      return $674($675(fromFoldable11($676)));
    };
  };
  var text = /* @__PURE__ */ function() {
    return Text.create;
  }();
  var renderRule = /* @__PURE__ */ append3(/* @__PURE__ */ singleton6(""))(/* @__PURE__ */ singleton6("---"));
  var renderLink = function(name15) {
    return function(url) {
      var urlString = function() {
        if (url instanceof ExternalUrl) {
          return url.value0;
        }
        ;
        if (url instanceof InternalUrl) {
          return "#" + url.value0;
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 370, column 15 - line 374, column 15): " + [url.constructor.name]);
      }();
      return "[" + (toString2(name15) + ("](" + (urlString + ")")));
    };
  };
  var renderInlineCode = function(code2) {
    return "`" + (toString2(code2) + "`");
  };
  var renderCodeLanguage = function(v) {
    if (v instanceof Json) {
      return "json";
    }
    ;
    if (v instanceof Mermaid) {
      return "mermaid";
    }
    ;
    if (v instanceof PlainText) {
      return "text";
    }
    ;
    throw new Error("Failed pattern match at Data.Markdown (line 438, column 22 - line 444, column 11): " + [v.constructor.name]);
  };
  var renderCodeBlock = function(codeLanguage) {
    return function(code2) {
      var top12 = singleton6("```" + renderCodeLanguage(codeLanguage));
      var codeLines = reverse3(fromFoldable6(code2));
      var bottom4 = append3(singleton6(""))(singleton6("```"));
      return append3(appendFoldable2(bottom4)(codeLines))(top12);
    };
  };
  var paragraph = function(dictFoldable1) {
    var $677 = fromFoldable1(dictFoldable1);
    return function($678) {
      return Paragraph.create($677($678));
    };
  };
  var orderedList = function(dictFoldable1) {
    var fromFoldable11 = fromFoldable1(dictFoldable1);
    var $679 = List.create(true);
    var $680 = map9(fromFoldable11);
    return function($681) {
      return $679($680(fromFoldable11($681)));
    };
  };
  var mapLast = function(f) {
    return function($683) {
      return function(v) {
        var v1 = fromList(v.tail);
        if (v1 instanceof Just) {
          return append3(singleton6(v.head))(mapLast(f)(v1.value0));
        }
        ;
        if (v1 instanceof Nothing) {
          return singleton6(f(v.head));
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 81, column 39 - line 85, column 32): " + [v1.constructor.name]);
      }(uncons3($683));
    };
  };
  var mapLast$prime = function(f) {
    return function($684) {
      return function(v) {
        if (v instanceof Just) {
          return toList2(mapLast(f)(v.value0));
        }
        ;
        if (v instanceof Nothing) {
          return Nil.value;
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 88, column 34 - line 92, column 8): " + [v.constructor.name]);
      }(fromList($684));
    };
  };
  var mapFirst = function(f) {
    return function($685) {
      return function(v) {
        return cons$prime2(f(v.head))(v.tail);
      }(uncons3($685));
    };
  };
  var lineBreak = /* @__PURE__ */ function() {
    return LineBreak.value;
  }();
  var inlineCode = /* @__PURE__ */ function() {
    return InlineCode.create;
  }();
  var formatText = function(maxLineLength) {
    var splitAtFurthestSpace = function(s) {
      var $623 = length5(s) <= maxLineLength;
      if ($623) {
        return new Tuple(s, Nothing.value);
      }
      ;
      var v = lastIndexOf$prime3(" ")(maxLineLength)(s);
      if (v instanceof Nothing) {
        return new Tuple(s, Nothing.value);
      }
      ;
      if (v instanceof Just) {
        var v1 = splitAt4(v.value0)(s);
        if (v1.before instanceof Just && v1.after instanceof Just) {
          return new Tuple(v1.before.value0, stripPrefix2(" ")(v1.after.value0));
        }
        ;
        return new Tuple(s, Nothing.value);
      }
      ;
      throw new Error("Failed pattern match at Data.Markdown (line 274, column 10 - line 285, column 27): " + [v.constructor.name]);
    };
    return unfoldr12(splitAtFurthestSpace);
  };
  var renderPhrasingContentNodes = function(options2) {
    var f = function(acc) {
      return function(node) {
        if (node instanceof Emphasis) {
          return function(v2) {
            return appendFoldable2(mapFirst(function(v12) {
              return v12 + v2.head;
            })(acc))(v2.tail);
          }(uncons3(renderEmphasis(options2)(node.value0)));
        }
        ;
        if (node instanceof Link) {
          return mapFirst(function(v2) {
            return v2 + renderLink(node.value0)(node.value1);
          })(acc);
        }
        ;
        if (node instanceof InlineCode) {
          return mapFirst(function(v2) {
            return v2 + renderInlineCode(node.value0);
          })(acc);
        }
        ;
        if (node instanceof LineBreak) {
          return append3(singleton6(""))(mapFirst(function(v2) {
            return v2 + "\\";
          })(acc));
        }
        ;
        if (node instanceof Text) {
          var v = uncons3(formatText(options2.maxLineLength)(node.value0));
          var v1 = fromList(v.tail);
          if (v1 instanceof Nothing) {
            return mapFirst(function(v2) {
              return v2 + toString2(v.head);
            })(acc);
          }
          ;
          if (v1 instanceof Just) {
            return append3(map12(toString2)(reverse4(v1.value0)))(mapFirst(function(v2) {
              return v2 + toString2(v.head);
            })(acc));
          }
          ;
          throw new Error("Failed pattern match at Data.Markdown (line 259, column 11 - line 264, column 62): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 243, column 5 - line 264, column 62): " + [node.constructor.name]);
      };
    };
    return foldl4(f)(singleton6(""));
  };
  var renderEmphasis = function(options2) {
    return defer2(function(v) {
      var $704 = mapLast(function(v1) {
        return v1 + "_";
      });
      var $705 = mapFirst(function(v1) {
        return "_" + v1;
      });
      var $706 = renderPhrasingContentNodes(options2);
      return function($707) {
        return $704($705($706($707)));
      };
    });
  };
  var renderHeading = function(options2) {
    return function(headingLevel) {
      return function(children2) {
        var prefix = function(v) {
          return v + " ";
        }(function() {
          if (headingLevel instanceof L1) {
            return "#";
          }
          ;
          if (headingLevel instanceof L2) {
            return "##";
          }
          ;
          if (headingLevel instanceof L3) {
            return "###";
          }
          ;
          if (headingLevel instanceof L4) {
            return "####";
          }
          ;
          if (headingLevel instanceof L5) {
            return "#####";
          }
          ;
          if (headingLevel instanceof L6) {
            return "######";
          }
          ;
          throw new Error("Failed pattern match at Data.Markdown (line 345, column 25 - line 357, column 15): " + [headingLevel.constructor.name]);
        }());
        var renderedLines = renderPhrasingContentNodes(options2)(children2);
        var formattedLine = joinWith("")(map22(replaceAll("\\")("<br/>"))(fromFoldable12(reverse4(renderedLines))));
        return append3(singleton6(""))(singleton6(prefix + formattedLine));
      };
    };
  };
  var renderParagraph = function(options2) {
    return function(nodes) {
      var renderedLines = renderPhrasingContentNodes(options2)(nodes);
      return append3(singleton6(""))(renderedLines);
    };
  };
  var renderList = function(options2) {
    return function(isOrdered) {
      return function(children2) {
        var itemPrefix = function() {
          if (isOrdered) {
            return "1.";
          }
          ;
          return "-";
        }() + " ";
        var indent = joinWith("")(replicate(length4(itemPrefix))(" "));
        var renderListItem = function(nodes) {
          var renderedLines = foldMap12(renderFlowContentNode(options2))(reverse2(nodes));
          var v = unsnoc3(renderedLines);
          var formattedOtherLines = map32(function(s) {
            var $650 = $$null2(s);
            if ($650) {
              return s;
            }
            ;
            return indent + s;
          })(dropWhile2($$null2)(v.init));
          var formattedFirstLine = itemPrefix + v.last;
          return snoc$prime(new Cons("", formattedOtherLines))(formattedFirstLine);
        };
        return foldMap12(renderListItem)(reverse2(children2));
      };
    };
  };
  var renderFlowContentNode = function(options2) {
    return defer2(function(v) {
      return function(v1) {
        if (v1 instanceof Blockquote) {
          return renderBlockquote(options2)(v1.value0);
        }
        ;
        if (v1 instanceof CodeBlock) {
          return renderCodeBlock(v1.value0)(v1.value1);
        }
        ;
        if (v1 instanceof Heading) {
          return renderHeading({
            maxLineLength: top3
          })(v1.value0)(v1.value1);
        }
        ;
        if (v1 instanceof List) {
          return renderList(options2)(v1.value0)(v1.value1);
        }
        ;
        if (v1 instanceof Paragraph) {
          return renderParagraph(options2)(v1.value0);
        }
        ;
        if (v1 instanceof Rule) {
          return renderRule;
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 217, column 3 - line 232, column 17): " + [v1.constructor.name]);
      };
    });
  };
  var renderBlockquote = function(options2) {
    return function(children2) {
      var renderedLines = foldMap12(renderFlowContentNode(options2))(reverse2(children2));
      var formatLine = function(s) {
        var $662 = $$null2(s);
        if ($662) {
          return ">";
        }
        ;
        return "> " + s;
      };
      var v = unsnoc3(renderedLines);
      var formattedOtherLines = map32(formatLine)(dropWhile2($$null2)(v.init));
      var formattedFirstLine = formatLine(v.last);
      return snoc$prime(new Cons("", formattedOtherLines))(formattedFirstLine);
    };
  };
  var document = function(dictFoldable) {
    var $718 = map22(FlowContent.create);
    var $719 = fromFoldable(dictFoldable);
    return function($720) {
      return $718($719($720));
    };
  };
  var appendWith$prime = function(f) {
    return function(left) {
      return function($728) {
        return function(v) {
          if (v instanceof Just) {
            return append22(mapLast$prime(function(x) {
              return f(x)(v.value0.head);
            })(left))(v.value0.tail);
          }
          ;
          if (v instanceof Nothing) {
            return left;
          }
          ;
          throw new Error("Failed pattern match at Data.Markdown (line 96, column 38 - line 100, column 9): " + [v.constructor.name]);
        }(uncons2($728));
      };
    };
  };
  var render2 = function(options2) {
    var f = function(acc) {
      return function(v) {
        if (v instanceof FlowContent) {
          var renderedLines = toList2(renderFlowContentNode(options2)(v.value0));
          var mergedLines = append22(renderedLines)(acc);
          return mergedLines;
        }
        ;
        if (v instanceof PhrasingContent) {
          var renderedLines = toList2(renderPhrasingContentNodes(options2)(singleton5(v.value0)));
          var mergedLines = appendWith$prime(flip(append13))(renderedLines)(acc);
          return mergedLines;
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 191, column 11 - line 212, column 20): " + [v.constructor.name]);
      };
    };
    var $729 = joinWith("\n");
    var $730 = dropWhile2($$null2);
    var $731 = foldl12(f)(Nil.value);
    return function($732) {
      return function(v) {
        return v + "\n";
      }($729(fromFoldable22(reverse3($730($731(fromFoldable6($732)))))));
    };
  };

  // output/Data.Set.NonEmpty/index.js
  var coerce4 = /* @__PURE__ */ coerce();
  var map13 = /* @__PURE__ */ map(functorTuple);
  var foldMap13 = /* @__PURE__ */ foldMap1(foldable1NonEmptyList);
  var foldr12 = /* @__PURE__ */ foldr1(foldable1NonEmptyList);
  var foldl13 = /* @__PURE__ */ foldl1(foldable1NonEmptyList);
  var toUnfoldable1 = function(dictUnfoldable1) {
    var stepNext = stepAscCps(function(k, v, next) {
      return new Just(new Tuple(k, next));
    })(function(v) {
      return Nothing.value;
    });
    var stepHead = stepAscCps(function(k, v, next) {
      return new Tuple(k, next);
    })(function(v) {
      return unsafeCrashWith("toUnfoldable1: impossible");
    });
    var $82 = unfoldr1(dictUnfoldable1)(function(v) {
      return map13(stepNext)(v);
    });
    return function($83) {
      return $82(stepHead(toMapIter(toMap(coerce4($83)))));
    };
  };
  var toUnfoldable12 = /* @__PURE__ */ toUnfoldable1(unfoldable1NonEmptyList);
  var ordNonEmptySet = function(dictOrd) {
    return ordSet(dictOrd);
  };
  var fromSet = function(s) {
    var $75 = isEmpty2(s);
    if ($75) {
      return Nothing.value;
    }
    ;
    return new Just(s);
  };
  var foldableNonEmptySet = foldableSet;
  var foldable1NonEmptySet = {
    foldMap1: function(dictSemigroup) {
      var foldMap11 = foldMap13(dictSemigroup);
      return function(f) {
        var $86 = foldMap11(f);
        return function($87) {
          return $86(toUnfoldable12($87));
        };
      };
    },
    foldr1: function(f) {
      var $88 = foldr12(f);
      return function($89) {
        return $88(toUnfoldable12($89));
      };
    },
    foldl1: function(f) {
      var $90 = foldl13(f);
      return function($91) {
        return $90(toUnfoldable12($91));
      };
    },
    Foldable0: function() {
      return foldableNonEmptySet;
    }
  };
  var eqNonEmptySet = function(dictEq) {
    return eqSet(dictEq);
  };

  // output/Show.NonEmpty/index.js
  var nes2 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "impossible";
    }
  }));
  var show1Number = {
    show1: /* @__PURE__ */ function() {
      var $27 = fromMaybe(nes2($$Proxy.value));
      var $28 = show(showNumber);
      return function($29) {
        return $27(fromString($28($29)));
      };
    }()
  };
  var show1 = function(dict) {
    return dict.show1;
  };

  // output/Docs.Document/index.js
  var singleton11 = /* @__PURE__ */ singleton4(plusArray);
  var paragraph2 = /* @__PURE__ */ paragraph(foldable1NonEmptyArray);
  var nes3 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "\u2205";
    }
  }));
  var orderedList2 = /* @__PURE__ */ orderedList(foldable1NonEmptyArray);
  var unorderedList2 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var map11 = /* @__PURE__ */ map(functorNonEmptyArray);
  var document2 = function(dict) {
    return dict.document;
  };
  var documentFoldable = function(dictDocument) {
    var document1 = document2(dictDocument);
    return function(dictFoldable) {
      var fromFoldable15 = fromFoldable3(dictFoldable);
      return function(isOrdered) {
        var $60 = maybe(paragraph2(singleton5(text(nes3($$Proxy.value)))))(function() {
          var $62 = function() {
            if (isOrdered) {
              return orderedList2;
            }
            ;
            return unorderedList2;
          }();
          var $63 = map11(function($65) {
            return fromNonEmpty(document1($65));
          });
          return function($64) {
            return $62($63($64));
          };
        }());
        return function($61) {
          return singleton11($60(fromFoldable15($61)));
        };
      };
    };
  };
  var documentSet = function(dictDocument) {
    return {
      document: documentFoldable(dictDocument)(foldableSet)(false)
    };
  };

  // output/CLI/index.js
  var document3 = /* @__PURE__ */ document(/* @__PURE__ */ foldableNonEmpty(foldableArray));
  var Json2 = /* @__PURE__ */ function() {
    function Json3() {
    }
    ;
    Json3.value = new Json3();
    return Json3;
  }();
  var Markdown = /* @__PURE__ */ function() {
    function Markdown2() {
    }
    ;
    Markdown2.value = new Markdown2();
    return Markdown2;
  }();
  var runProgram = function(dictDocument) {
    var document1 = document2(dictDocument);
    return function(dictEncodeJson) {
      var encodeJson3 = encodeJson(dictEncodeJson);
      return function(outputFormat) {
        return function(compute2) {
          var renderOutput = function() {
            if (outputFormat instanceof Json2) {
              var $14 = stringifyWithIndent(2);
              return function($15) {
                return $14(encodeJson3($15));
              };
            }
            ;
            if (outputFormat instanceof Markdown) {
              var $16 = render2({
                maxLineLength: 72
              });
              return function($17) {
                return $16(document3(document1($17)));
              };
            }
            ;
            throw new Error("Failed pattern match at CLI (line 36, column 18 - line 42, column 30): " + [outputFormat.constructor.name]);
          }();
          return function($18) {
            return function(v) {
              if (v instanceof Left) {
                return {
                  exitCode: 1,
                  stderr: v.value0 + "\n",
                  stdout: ""
                };
              }
              ;
              if (v instanceof Right) {
                return {
                  exitCode: 0,
                  stderr: "",
                  stdout: renderOutput(v.value0) + "\n"
                };
              }
              ;
              throw new Error("Failed pattern match at CLI (line 29, column 47 - line 33, column 69): " + [v.constructor.name]);
            }(compute2($18));
          };
        };
      };
    };
  };

  // output/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail2, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail2(e.message);
    }
  }

  // output/Data.Argonaut.Parser/index.js
  var jsonParser = function(j) {
    return _jsonParser(Left.create, Right.create, j);
  };

  // output/JsonValue/index.js
  var showJsonValue = {
    show: function(v) {
      return stringify(v);
    }
  };
  var ordJsonValue = ordJson;

  // output/JsonSchema/index.js
  var nes4 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "array";
    }
  }));
  var nes1 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "boolean";
    }
  }));
  var nes22 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "integer";
    }
  }));
  var nes32 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "null";
    }
  }));
  var nes42 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "number";
    }
  }));
  var nes5 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "object";
    }
  }));
  var nes6 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "string";
    }
  }));
  var wrap2 = /* @__PURE__ */ wrap();
  var map14 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable3(unfoldableArray);
  var append4 = /* @__PURE__ */ append(semigroupArray);
  var fromFoldable7 = /* @__PURE__ */ fromFoldable2(foldableArray);
  var map15 = /* @__PURE__ */ map(functorTuple);
  var unwrap2 = /* @__PURE__ */ unwrap();
  var JsonArray = /* @__PURE__ */ function() {
    function JsonArray2() {
    }
    ;
    JsonArray2.value = new JsonArray2();
    return JsonArray2;
  }();
  var JsonBoolean = /* @__PURE__ */ function() {
    function JsonBoolean2() {
    }
    ;
    JsonBoolean2.value = new JsonBoolean2();
    return JsonBoolean2;
  }();
  var JsonInteger = /* @__PURE__ */ function() {
    function JsonInteger2() {
    }
    ;
    JsonInteger2.value = new JsonInteger2();
    return JsonInteger2;
  }();
  var JsonNull = /* @__PURE__ */ function() {
    function JsonNull2() {
    }
    ;
    JsonNull2.value = new JsonNull2();
    return JsonNull2;
  }();
  var JsonNumber = /* @__PURE__ */ function() {
    function JsonNumber2() {
    }
    ;
    JsonNumber2.value = new JsonNumber2();
    return JsonNumber2;
  }();
  var JsonObject = /* @__PURE__ */ function() {
    function JsonObject2() {
    }
    ;
    JsonObject2.value = new JsonObject2();
    return JsonObject2;
  }();
  var JsonString = /* @__PURE__ */ function() {
    function JsonString2() {
    }
    ;
    JsonString2.value = new JsonString2();
    return JsonString2;
  }();
  var BooleanSchema = /* @__PURE__ */ function() {
    function BooleanSchema2(value0) {
      this.value0 = value0;
    }
    ;
    BooleanSchema2.create = function(value0) {
      return new BooleanSchema2(value0);
    };
    return BooleanSchema2;
  }();
  var ObjectSchema = /* @__PURE__ */ function() {
    function ObjectSchema2(value0) {
      this.value0 = value0;
    }
    ;
    ObjectSchema2.create = function(value0) {
      return new ObjectSchema2(value0);
    };
    return ObjectSchema2;
  }();
  var eqJsonValueType = {
    eq: function(x) {
      return function(y) {
        if (x instanceof JsonArray && y instanceof JsonArray) {
          return true;
        }
        ;
        if (x instanceof JsonBoolean && y instanceof JsonBoolean) {
          return true;
        }
        ;
        if (x instanceof JsonInteger && y instanceof JsonInteger) {
          return true;
        }
        ;
        if (x instanceof JsonNull && y instanceof JsonNull) {
          return true;
        }
        ;
        if (x instanceof JsonNumber && y instanceof JsonNumber) {
          return true;
        }
        ;
        if (x instanceof JsonObject && y instanceof JsonObject) {
          return true;
        }
        ;
        if (x instanceof JsonString && y instanceof JsonString) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordJsonValueType = {
    compare: function(x) {
      return function(y) {
        if (x instanceof JsonArray && y instanceof JsonArray) {
          return EQ.value;
        }
        ;
        if (x instanceof JsonArray) {
          return LT.value;
        }
        ;
        if (y instanceof JsonArray) {
          return GT.value;
        }
        ;
        if (x instanceof JsonBoolean && y instanceof JsonBoolean) {
          return EQ.value;
        }
        ;
        if (x instanceof JsonBoolean) {
          return LT.value;
        }
        ;
        if (y instanceof JsonBoolean) {
          return GT.value;
        }
        ;
        if (x instanceof JsonInteger && y instanceof JsonInteger) {
          return EQ.value;
        }
        ;
        if (x instanceof JsonInteger) {
          return LT.value;
        }
        ;
        if (y instanceof JsonInteger) {
          return GT.value;
        }
        ;
        if (x instanceof JsonNull && y instanceof JsonNull) {
          return EQ.value;
        }
        ;
        if (x instanceof JsonNull) {
          return LT.value;
        }
        ;
        if (y instanceof JsonNull) {
          return GT.value;
        }
        ;
        if (x instanceof JsonNumber && y instanceof JsonNumber) {
          return EQ.value;
        }
        ;
        if (x instanceof JsonNumber) {
          return LT.value;
        }
        ;
        if (y instanceof JsonNumber) {
          return GT.value;
        }
        ;
        if (x instanceof JsonObject && y instanceof JsonObject) {
          return EQ.value;
        }
        ;
        if (x instanceof JsonObject) {
          return LT.value;
        }
        ;
        if (y instanceof JsonObject) {
          return GT.value;
        }
        ;
        if (x instanceof JsonString && y instanceof JsonString) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at JsonSchema (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqJsonValueType;
    }
  };
  var renderJsonValueType = function(v) {
    if (v instanceof JsonArray) {
      return nes4($$Proxy.value);
    }
    ;
    if (v instanceof JsonBoolean) {
      return nes1($$Proxy.value);
    }
    ;
    if (v instanceof JsonInteger) {
      return nes22($$Proxy.value);
    }
    ;
    if (v instanceof JsonNull) {
      return nes32($$Proxy.value);
    }
    ;
    if (v instanceof JsonNumber) {
      return nes42($$Proxy.value);
    }
    ;
    if (v instanceof JsonObject) {
      return nes5($$Proxy.value);
    }
    ;
    if (v instanceof JsonString) {
      return nes6($$Proxy.value);
    }
    ;
    throw new Error("Failed pattern match at JsonSchema (line 97, column 23 - line 111, column 42): " + [v.constructor.name]);
  };
  var encodeJsonJsonValueType = {
    encodeJson: function($239) {
      return id(toString2(renderJsonValueType($239)));
    }
  };
  var printRequiredKeywordSpec = /* @__PURE__ */ function() {
    var $240 = map14(id);
    return function($241) {
      return wrap2(id($240(toUnfoldable6($241))));
    };
  }();
  var printRequired = function(propertyNames) {
    var $227 = isEmpty2(propertyNames);
    if ($227) {
      return [];
    }
    ;
    return [new Tuple("required", printRequiredKeywordSpec(propertyNames))];
  };
  var printOptionalNumber = function(name15) {
    return function(v) {
      if (v instanceof Just) {
        return [new Tuple(name15, wrap2(id(v.value0)))];
      }
      ;
      if (v instanceof Nothing) {
        return [];
      }
      ;
      throw new Error("Failed pattern match at JsonSchema (line 152, column 28 - line 155, column 15): " + [v.constructor.name]);
    };
  };
  var printMultipleOf = function(v) {
    if (v instanceof Just) {
      return [new Tuple("multipleOf", wrap2(id(v.value0)))];
    }
    ;
    if (v instanceof Nothing) {
      return [];
    }
    ;
    throw new Error("Failed pattern match at JsonSchema (line 158, column 19 - line 161, column 15): " + [v.constructor.name]);
  };
  var printJsonValueType = function($242) {
    return id(function(v) {
      if (v instanceof JsonArray) {
        return "array";
      }
      ;
      if (v instanceof JsonBoolean) {
        return "boolean";
      }
      ;
      if (v instanceof JsonInteger) {
        return "integer";
      }
      ;
      if (v instanceof JsonNull) {
        return "null";
      }
      ;
      if (v instanceof JsonNumber) {
        return "number";
      }
      ;
      if (v instanceof JsonObject) {
        return "object";
      }
      ;
      if (v instanceof JsonString) {
        return "string";
      }
      ;
      throw new Error("Failed pattern match at JsonSchema (line 196, column 39 - line 210, column 13): " + [v.constructor.name]);
    }($242));
  };
  var printTypeKeywordSpec = /* @__PURE__ */ function() {
    var $243 = map14(printJsonValueType);
    return function($244) {
      return wrap2(id($243(toUnfoldable6($244))));
    };
  }();
  var printTypeKeyword = function(v) {
    if (v instanceof Just) {
      return [new Tuple("type", printTypeKeywordSpec(v.value0))];
    }
    ;
    if (v instanceof Nothing) {
      return [];
    }
    ;
    throw new Error("Failed pattern match at JsonSchema (line 171, column 20 - line 176, column 7): " + [v.constructor.name]);
  };
  var defaultKeywords = /* @__PURE__ */ function() {
    return {
      exclusiveMaximum: Nothing.value,
      exclusiveMinimum: Nothing.value,
      items: Nothing.value,
      maximum: Nothing.value,
      minimum: Nothing.value,
      multipleOf: Nothing.value,
      not: Nothing.value,
      required: empty4,
      typeKeyword: Nothing.value,
      uniqueItems: false
    };
  }();
  var printUniqueItems = function(bool) {
    var $235 = bool === defaultKeywords.uniqueItems;
    if ($235) {
      return [];
    }
    ;
    return [new Tuple("uniqueItems", wrap2(id(bool)))];
  };
  var printObjectSchema = function(keywords) {
    var printNot = function(mbSchema) {
      return maybe([])(function($245) {
        return singleton2(function(v) {
          return new Tuple("not", v);
        }(print($245)));
      })(mbSchema);
    };
    var printItems = function(mbSchema) {
      return maybe([])(function($246) {
        return singleton2(function(v) {
          return new Tuple("items", v);
        }(print($246)));
      })(mbSchema);
    };
    var printKeywords = append4(printOptionalNumber("exclusiveMaximum")(keywords.exclusiveMaximum))(append4(printOptionalNumber("exclusiveMinimum")(keywords.exclusiveMinimum))(append4(printItems(keywords.items))(append4(printOptionalNumber("maximum")(keywords.maximum))(append4(printOptionalNumber("minimum")(keywords.minimum))(append4(printMultipleOf(keywords.multipleOf))(append4(printNot(keywords.not))(append4(printRequired(keywords.required))(append4(printTypeKeyword(keywords.typeKeyword))(printUniqueItems(keywords.uniqueItems))))))))));
    return wrap2(id(fromFoldable7(map14(map15(unwrap2))(printKeywords))));
  };
  var print = function(v) {
    if (v instanceof BooleanSchema) {
      return wrap2(id(v.value0));
    }
    ;
    if (v instanceof ObjectSchema) {
      return printObjectSchema(v.value0);
    }
    ;
    throw new Error("Failed pattern match at JsonSchema (line 114, column 9 - line 118, column 31): " + [v.constructor.name]);
  };

  // output/JsonSchema.Codec.Parsing/index.js
  var bind2 = /* @__PURE__ */ bind(bindEither);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEither);
  var pure2 = /* @__PURE__ */ pure(applicativeEither);
  var fromFoldable8 = /* @__PURE__ */ fromFoldable5(foldableArray);
  var fromFoldable13 = /* @__PURE__ */ fromFoldable8(ordString);
  var map16 = /* @__PURE__ */ map(functorEither);
  var fromFoldable23 = /* @__PURE__ */ fromFoldable8(ordJsonValueType);
  var alt2 = /* @__PURE__ */ alt(altEither);
  var unwrap3 = /* @__PURE__ */ unwrap();
  var map17 = /* @__PURE__ */ map(functorMaybe);
  var wrap3 = /* @__PURE__ */ wrap();
  var parsingErrorMessage = function(reason) {
    return "Invalid schema: " + reason;
  };
  var parseRequiredKeywordSpec = function(specJson) {
    return bind2(note("Property names are not an array.")(toArray(specJson)))(function(propertyNameJsons) {
      return bind2(traverse2(function() {
        var $45 = note("Property name is not a string.");
        return function($46) {
          return $45(toString($46));
        };
      }())(propertyNameJsons))(function(propertyNames) {
        return pure2(fromFoldable13(propertyNames));
      });
    });
  };
  var parseOptionalNumber = function(name15) {
    var $47 = lookup(name15);
    return function($48) {
      return function(v) {
        if (v instanceof Just) {
          var v1 = toNumber(v.value0);
          if (v1 instanceof Just) {
            return new Right(new Just(v1.value0));
          }
          ;
          if (v1 instanceof Nothing) {
            return new Left(name15 + " is not a number.");
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.Codec.Parsing (line 98, column 5 - line 102, column 43): " + [v1.constructor.name]);
        }
        ;
        if (v instanceof Nothing) {
          return new Right(Nothing.value);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Codec.Parsing (line 96, column 51 - line 104, column 18): " + [v.constructor.name]);
      }($47($48));
    };
  };
  var parseMultipleOf = /* @__PURE__ */ function() {
    var $49 = lookup("multipleOf");
    return function($50) {
      return function(v) {
        if (v instanceof Just) {
          var $28 = isNull(v.value0);
          if ($28) {
            return new Right(defaultKeywords.multipleOf);
          }
          ;
          var v1 = toNumber(v.value0);
          if (v1 instanceof Just) {
            var $30 = v1.value0 > 0;
            if ($30) {
              return new Right(new Just(v1.value0));
            }
            ;
            return new Left("multipleOf must be greater than zero.");
          }
          ;
          if (v1 instanceof Nothing) {
            return new Left("multipleOf is not a number.");
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.Codec.Parsing (line 110, column 10 - line 115, column 43): " + [v1.constructor.name]);
        }
        ;
        if (v instanceof Nothing) {
          return new Right(defaultKeywords.multipleOf);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Codec.Parsing (line 107, column 50 - line 117, column 18): " + [v.constructor.name]);
      }($49($50));
    };
  }();
  var parseJsonValueType = function(json) {
    var v = toString(json);
    if (v instanceof Just && v.value0 === "array") {
      return new Right(JsonArray.value);
    }
    ;
    if (v instanceof Just && v.value0 === "boolean") {
      return new Right(JsonBoolean.value);
    }
    ;
    if (v instanceof Just && v.value0 === "integer") {
      return new Right(JsonInteger.value);
    }
    ;
    if (v instanceof Just && v.value0 === "null") {
      return new Right(JsonNull.value);
    }
    ;
    if (v instanceof Just && v.value0 === "number") {
      return new Right(JsonNumber.value);
    }
    ;
    if (v instanceof Just && v.value0 === "object") {
      return new Right(JsonObject.value);
    }
    ;
    if (v instanceof Just && v.value0 === "string") {
      return new Right(JsonString.value);
    }
    ;
    if (v instanceof Just) {
      return new Left("Unsupported JSON value type: " + v.value0);
    }
    ;
    if (v instanceof Nothing) {
      return new Left("JSON value type is not a string");
    }
    ;
    throw new Error("Failed pattern match at JsonSchema.Codec.Parsing (line 145, column 27 - line 171, column 43): " + [v.constructor.name]);
  };
  var parseTypeKeywordSpec = function(specJson) {
    var parseStringSpec = function() {
      var $51 = map16(singleton8);
      return function($52) {
        return $51(parseJsonValueType($52));
      };
    }();
    var parseArraySpec = function(json) {
      return bind2(note("Types are not an array.")(toArray(json)))(function(typeJsons) {
        return bind2(traverse2(parseJsonValueType)(typeJsons))(function(types) {
          return pure2(fromFoldable23(types));
        });
      });
    };
    return alt2(parseStringSpec(specJson))(parseArraySpec(specJson));
  };
  var parseBooleanSchema = function(json) {
    return bind2(note(parsingErrorMessage("the JSON value is not a JSON boolean"))(toBoolean(unwrap3(json))))(function(bool) {
      return pure2(new BooleanSchema(bool));
    });
  };
  var parseSchema = function(json) {
    return alt2(parseBooleanSchema(json))(alt2(parseObjectSchema(json))(new Left("the JSON value is neither a boolean nor an object")));
  };
  var parseObjectSchema = function(keywordsJson) {
    return bind2(note(parsingErrorMessage("the JSON value is not a JSON object"))(toObject(unwrap3(keywordsJson))))(function(schemaObject) {
      return bind2(parseOptionalNumber("exclusiveMaximum")(schemaObject))(function(exclusiveMaximum) {
        return bind2(parseOptionalNumber("exclusiveMinimum")(schemaObject))(function(exclusiveMinimum) {
          return bind2(maybe(new Right(defaultKeywords.items))(function() {
            var $53 = map16(Just.create);
            return function($54) {
              return $53(parseSchema($54));
            };
          }())(map17(wrap3)(lookup("items")(schemaObject))))(function(items2) {
            return bind2(parseOptionalNumber("maximum")(schemaObject))(function(maximum2) {
              return bind2(parseOptionalNumber("minimum")(schemaObject))(function(minimum2) {
                return bind2(parseMultipleOf(schemaObject))(function(multipleOf) {
                  return bind2(maybe(new Right(defaultKeywords.not))(function() {
                    var $55 = map16(Just.create);
                    return function($56) {
                      return $55(parseSchema($56));
                    };
                  }())(map17(wrap3)(lookup("not")(schemaObject))))(function(not3) {
                    return bind2(maybe(new Right(defaultKeywords.required))(parseRequiredKeywordSpec)(lookup("required")(schemaObject)))(function(required4) {
                      return bind2(function() {
                        var v = lookup("type")(schemaObject);
                        if (v instanceof Just) {
                          return map16(Just.create)(parseTypeKeywordSpec(v.value0));
                        }
                        ;
                        if (v instanceof Nothing) {
                          return new Right(defaultKeywords.typeKeyword);
                        }
                        ;
                        throw new Error("Failed pattern match at JsonSchema.Codec.Parsing (line 69, column 17 - line 73, column 47): " + [v.constructor.name]);
                      }())(function(typeKeyword) {
                        return bind2(maybe(new Right(defaultKeywords.uniqueItems))(function(json) {
                          var $44 = isNull(json);
                          if ($44) {
                            return new Right(defaultKeywords.uniqueItems);
                          }
                          ;
                          return note("Unique items is not a boolean.")(toBoolean(json));
                        })(lookup("uniqueItems")(schemaObject)))(function(uniqueItems) {
                          return pure2(new ObjectSchema({
                            exclusiveMaximum,
                            exclusiveMinimum,
                            items: items2,
                            maximum: maximum2,
                            minimum: minimum2,
                            multipleOf,
                            not: not3,
                            required: required4,
                            typeKeyword,
                            uniqueItems
                          }));
                        });
                      });
                    });
                  });
                });
              });
            });
          });
        });
      });
    });
  };

  // output/Data.Argonaut.Encode.Combinators/index.js
  var extend3 = function(dictEncodeJson) {
    return extend2(encodeJson(dictEncodeJson));
  };
  var assoc2 = function(dictEncodeJson) {
    return assoc(encodeJson(dictEncodeJson));
  };

  // output/Data.Map/index.js
  var keys3 = /* @__PURE__ */ function() {
    var $38 = $$void(functorMap);
    return function($39) {
      return fromMap($38($39));
    };
  }();

  // output/JsonSchema.JsonPath/index.js
  var eq13 = /* @__PURE__ */ eq(eqNonEmptyString);
  var compare2 = /* @__PURE__ */ compare(ordInt);
  var compare13 = /* @__PURE__ */ compare(ordNonEmptyString);
  var append5 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes7 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "[";
    }
  }));
  var show3 = /* @__PURE__ */ show(showInt);
  var nes12 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "]";
    }
  }));
  var ItemIndex = /* @__PURE__ */ function() {
    function ItemIndex2(value0) {
      this.value0 = value0;
    }
    ;
    ItemIndex2.create = function(value0) {
      return new ItemIndex2(value0);
    };
    return ItemIndex2;
  }();
  var Property = /* @__PURE__ */ function() {
    function Property3(value0) {
      this.value0 = value0;
    }
    ;
    Property3.create = function(value0) {
      return new Property3(value0);
    };
    return Property3;
  }();
  var eqJsonPathSegment = {
    eq: function(x) {
      return function(y) {
        if (x instanceof ItemIndex && y instanceof ItemIndex) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof Property && y instanceof Property) {
          return eq13(x.value0)(y.value0);
        }
        ;
        return false;
      };
    }
  };
  var ordJsonPathSegment = {
    compare: function(x) {
      return function(y) {
        if (x instanceof ItemIndex && y instanceof ItemIndex) {
          return compare2(x.value0)(y.value0);
        }
        ;
        if (x instanceof ItemIndex) {
          return LT.value;
        }
        ;
        if (y instanceof ItemIndex) {
          return GT.value;
        }
        ;
        if (x instanceof Property && y instanceof Property) {
          return compare13(x.value0)(y.value0);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.JsonPath (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqJsonPathSegment;
    }
  };
  var encodeJsonJsonPathSegment = {
    encodeJson: function(v) {
      if (v instanceof ItemIndex) {
        return id(toNumber2(v.value0));
      }
      ;
      if (v instanceof Property) {
        return id(toString2(v.value0));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.JsonPath (line 43, column 16 - line 47, column 43): " + [v.constructor.name]);
    }
  };
  var render3 = /* @__PURE__ */ function() {
    var f = function(acc) {
      return function($78) {
        return function(v) {
          return append5(acc)(v);
        }(function(v) {
          if (v instanceof ItemIndex) {
            return append5(appendString(nes7($$Proxy.value))(show3(v.value0)))(nes12($$Proxy.value));
          }
          ;
          if (v instanceof Property) {
            return prependString("/")(v.value0);
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.JsonPath (line 27, column 26 - line 32, column 40): " + [v.constructor.name]);
        }($78));
      };
    };
    var $79 = foldl(foldableList)(f)(nes(nonEmptyNonEmpty({
      reflectSymbol: function() {
        return "$";
      }
    }))($$Proxy.value));
    return function($80) {
      return $79(reverse3($80));
    };
  }();

  // output/JsonSchema.Range/index.js
  var compare3 = /* @__PURE__ */ compare(ordNumber);
  var show4 = /* @__PURE__ */ show(showNumber);
  var nes8 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "]";
    }
  }));
  var nes13 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return ")";
    }
  }));
  var nes23 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "[";
    }
  }));
  var nes33 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "(";
    }
  }));
  var append14 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes43 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return ",";
    }
  }));
  var Closed = /* @__PURE__ */ function() {
    function Closed2(value0) {
      this.value0 = value0;
    }
    ;
    Closed2.create = function(value0) {
      return new Closed2(value0);
    };
    return Closed2;
  }();
  var Open = /* @__PURE__ */ function() {
    function Open2(value0) {
      this.value0 = value0;
    }
    ;
    Open2.create = function(value0) {
      return new Open2(value0);
    };
    return Open2;
  }();
  var eqBoundary = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Closed && y instanceof Closed) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof Open && y instanceof Open) {
          return x.value0 === y.value0;
        }
        ;
        return false;
      };
    }
  };
  var ordBoundary = {
    compare: function(x) {
      return function(y) {
        if (x instanceof Closed && y instanceof Closed) {
          return compare3(x.value0)(y.value0);
        }
        ;
        if (x instanceof Closed) {
          return LT.value;
        }
        ;
        if (y instanceof Closed) {
          return GT.value;
        }
        ;
        if (x instanceof Open && y instanceof Open) {
          return compare3(x.value0)(y.value0);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Range (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqBoundary;
    }
  };
  var encodeJsonBoundary = {
    encodeJson: function(v) {
      if (v instanceof Closed) {
        return id(show4(v.value0) + " (inclusively)");
      }
      ;
      if (v instanceof Open) {
        return id(show4(v.value0) + " (exclusively)");
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Range (line 42, column 16 - line 46, column 48): " + [v.constructor.name]);
    }
  };
  var renderRange = function(range3) {
    var renderTo = function() {
      if (range3.to instanceof Closed) {
        return prependString(show4(range3.to.value0))(nes8($$Proxy.value));
      }
      ;
      if (range3.to instanceof Open) {
        return prependString(show4(range3.to.value0))(nes13($$Proxy.value));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Range (line 29, column 14 - line 33, column 71): " + [range3.to.constructor.name]);
    }();
    var renderFrom = function() {
      if (range3.from instanceof Closed) {
        return appendString(nes23($$Proxy.value))(show4(range3.from.value0));
      }
      ;
      if (range3.from instanceof Open) {
        return appendString(nes33($$Proxy.value))(show4(range3.from.value0));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Range (line 22, column 16 - line 26, column 70): " + [range3.from.constructor.name]);
    }();
    return inlineCode(append14(renderFrom)(append14(nes43($$Proxy.value))(renderTo)));
  };

  // output/JsonSchema.SchemaPath/index.js
  var eq3 = /* @__PURE__ */ eq(eqNonEmptyString);
  var compare4 = /* @__PURE__ */ compare(ordNonEmptyString);
  var show5 = /* @__PURE__ */ show(showNonEmptyString);
  var append6 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes9 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "exclusiveMinimum";
    }
  }));
  var nes14 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "exclusiveMaximum";
    }
  }));
  var nes24 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "items";
    }
  }));
  var nes34 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "maximum";
    }
  }));
  var nes44 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "minimum";
    }
  }));
  var nes52 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "multipleOf";
    }
  }));
  var nes62 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "type";
    }
  }));
  var nes72 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "uniqueItems";
    }
  }));
  var ExclusiveMaximum = /* @__PURE__ */ function() {
    function ExclusiveMaximum2() {
    }
    ;
    ExclusiveMaximum2.value = new ExclusiveMaximum2();
    return ExclusiveMaximum2;
  }();
  var ExclusiveMinimum = /* @__PURE__ */ function() {
    function ExclusiveMinimum2() {
    }
    ;
    ExclusiveMinimum2.value = new ExclusiveMinimum2();
    return ExclusiveMinimum2;
  }();
  var Items = /* @__PURE__ */ function() {
    function Items2() {
    }
    ;
    Items2.value = new Items2();
    return Items2;
  }();
  var Maximum = /* @__PURE__ */ function() {
    function Maximum2() {
    }
    ;
    Maximum2.value = new Maximum2();
    return Maximum2;
  }();
  var Minimum = /* @__PURE__ */ function() {
    function Minimum2() {
    }
    ;
    Minimum2.value = new Minimum2();
    return Minimum2;
  }();
  var MultipleOf = /* @__PURE__ */ function() {
    function MultipleOf2() {
    }
    ;
    MultipleOf2.value = new MultipleOf2();
    return MultipleOf2;
  }();
  var Properties = /* @__PURE__ */ function() {
    function Properties2(value0) {
      this.value0 = value0;
    }
    ;
    Properties2.create = function(value0) {
      return new Properties2(value0);
    };
    return Properties2;
  }();
  var TypeKeyword = /* @__PURE__ */ function() {
    function TypeKeyword2() {
    }
    ;
    TypeKeyword2.value = new TypeKeyword2();
    return TypeKeyword2;
  }();
  var UniqueItems = /* @__PURE__ */ function() {
    function UniqueItems2() {
    }
    ;
    UniqueItems2.value = new UniqueItems2();
    return UniqueItems2;
  }();
  var eqSchemaPathSegment = {
    eq: function(x) {
      return function(y) {
        if (x instanceof ExclusiveMaximum && y instanceof ExclusiveMaximum) {
          return true;
        }
        ;
        if (x instanceof ExclusiveMinimum && y instanceof ExclusiveMinimum) {
          return true;
        }
        ;
        if (x instanceof Items && y instanceof Items) {
          return true;
        }
        ;
        if (x instanceof Maximum && y instanceof Maximum) {
          return true;
        }
        ;
        if (x instanceof Minimum && y instanceof Minimum) {
          return true;
        }
        ;
        if (x instanceof MultipleOf && y instanceof MultipleOf) {
          return true;
        }
        ;
        if (x instanceof Properties && y instanceof Properties) {
          return eq3(x.value0)(y.value0);
        }
        ;
        if (x instanceof TypeKeyword && y instanceof TypeKeyword) {
          return true;
        }
        ;
        if (x instanceof UniqueItems && y instanceof UniqueItems) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var ordSchemaPathSegment = {
    compare: function(x) {
      return function(y) {
        if (x instanceof ExclusiveMaximum && y instanceof ExclusiveMaximum) {
          return EQ.value;
        }
        ;
        if (x instanceof ExclusiveMaximum) {
          return LT.value;
        }
        ;
        if (y instanceof ExclusiveMaximum) {
          return GT.value;
        }
        ;
        if (x instanceof ExclusiveMinimum && y instanceof ExclusiveMinimum) {
          return EQ.value;
        }
        ;
        if (x instanceof ExclusiveMinimum) {
          return LT.value;
        }
        ;
        if (y instanceof ExclusiveMinimum) {
          return GT.value;
        }
        ;
        if (x instanceof Items && y instanceof Items) {
          return EQ.value;
        }
        ;
        if (x instanceof Items) {
          return LT.value;
        }
        ;
        if (y instanceof Items) {
          return GT.value;
        }
        ;
        if (x instanceof Maximum && y instanceof Maximum) {
          return EQ.value;
        }
        ;
        if (x instanceof Maximum) {
          return LT.value;
        }
        ;
        if (y instanceof Maximum) {
          return GT.value;
        }
        ;
        if (x instanceof Minimum && y instanceof Minimum) {
          return EQ.value;
        }
        ;
        if (x instanceof Minimum) {
          return LT.value;
        }
        ;
        if (y instanceof Minimum) {
          return GT.value;
        }
        ;
        if (x instanceof MultipleOf && y instanceof MultipleOf) {
          return EQ.value;
        }
        ;
        if (x instanceof MultipleOf) {
          return LT.value;
        }
        ;
        if (y instanceof MultipleOf) {
          return GT.value;
        }
        ;
        if (x instanceof Properties && y instanceof Properties) {
          return compare4(x.value0)(y.value0);
        }
        ;
        if (x instanceof Properties) {
          return LT.value;
        }
        ;
        if (y instanceof Properties) {
          return GT.value;
        }
        ;
        if (x instanceof TypeKeyword && y instanceof TypeKeyword) {
          return EQ.value;
        }
        ;
        if (x instanceof TypeKeyword) {
          return LT.value;
        }
        ;
        if (y instanceof TypeKeyword) {
          return GT.value;
        }
        ;
        if (x instanceof UniqueItems && y instanceof UniqueItems) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.SchemaPath (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqSchemaPathSegment;
    }
  };
  var encodeJsonSchemaPathSegme = {
    encodeJson: function(v) {
      if (v instanceof ExclusiveMinimum) {
        return id("exclusiveMinimum");
      }
      ;
      if (v instanceof ExclusiveMaximum) {
        return id("exclusiveMaximum");
      }
      ;
      if (v instanceof Items) {
        return id("items");
      }
      ;
      if (v instanceof Maximum) {
        return id("maximum");
      }
      ;
      if (v instanceof Minimum) {
        return id("minimum");
      }
      ;
      if (v instanceof MultipleOf) {
        return id("multipleOf");
      }
      ;
      if (v instanceof Properties) {
        return id(show5(v.value0));
      }
      ;
      if (v instanceof TypeKeyword) {
        return id("type");
      }
      ;
      if (v instanceof UniqueItems) {
        return id("uniqueItems");
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.SchemaPath (line 62, column 16 - line 80, column 33): " + [v.constructor.name]);
    }
  };
  var render4 = /* @__PURE__ */ function() {
    var f = function(acc) {
      return function($151) {
        return function(v) {
          return append6(appendString(acc)("/"))(v);
        }(function(v) {
          if (v instanceof ExclusiveMinimum) {
            return nes9($$Proxy.value);
          }
          ;
          if (v instanceof ExclusiveMaximum) {
            return nes14($$Proxy.value);
          }
          ;
          if (v instanceof Items) {
            return nes24($$Proxy.value);
          }
          ;
          if (v instanceof Maximum) {
            return nes34($$Proxy.value);
          }
          ;
          if (v instanceof Minimum) {
            return nes44($$Proxy.value);
          }
          ;
          if (v instanceof MultipleOf) {
            return nes52($$Proxy.value);
          }
          ;
          if (v instanceof Properties) {
            return prependString("properties/")(v.value0);
          }
          ;
          if (v instanceof TypeKeyword) {
            return nes62($$Proxy.value);
          }
          ;
          if (v instanceof UniqueItems) {
            return nes72($$Proxy.value);
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.SchemaPath (line 26, column 54 - line 44, column 49): " + [v.constructor.name]);
        }($151));
      };
    };
    var $152 = foldl(foldableList)(f)(nes(nonEmptyNonEmpty({
      reflectSymbol: function() {
        return "#";
      }
    }))($$Proxy.value));
    return function($153) {
      return $152(reverse3($153));
    };
  }();

  // output/Utils/index.js
  var isInteger = function(x) {
    return toNumber2(trunc2(x)) === x;
  };

  // output/JsonSchema.Validation/index.js
  var $runtime_lazy4 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var fromIsSymbol = {
    reflectSymbol: function() {
      return "from";
    }
  };
  var toIsSymbol = {
    reflectSymbol: function() {
      return "to";
    }
  };
  var jsonPathIsSymbol = {
    reflectSymbol: function() {
      return "jsonPath";
    }
  };
  var reasonIsSymbol = {
    reflectSymbol: function() {
      return "reason";
    }
  };
  var schemaPathIsSymbol = {
    reflectSymbol: function() {
      return "schemaPath";
    }
  };
  var eq14 = /* @__PURE__ */ eq(eqBoundary);
  var eq22 = /* @__PURE__ */ eq(eqJsonValueType);
  var eq32 = /* @__PURE__ */ eq(/* @__PURE__ */ eqSet(eqJsonValueType));
  var eq4 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqJsonPathSegment));
  var eq5 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqSchemaPathSegment));
  var compare5 = /* @__PURE__ */ compare(ordNumber);
  var compare14 = /* @__PURE__ */ compare(ordBoundary);
  var compare22 = /* @__PURE__ */ compare(ordJsonValueType);
  var compare32 = /* @__PURE__ */ compare(/* @__PURE__ */ ordSet(ordJsonValueType));
  var compare42 = /* @__PURE__ */ compare(/* @__PURE__ */ ordList(ordJsonPathSegment));
  var compare52 = /* @__PURE__ */ compare(/* @__PURE__ */ ordList(ordSchemaPathSegment));
  var map18 = /* @__PURE__ */ map(functorArray);
  var fromFoldable10 = /* @__PURE__ */ fromFoldable(foldableNonEmptySet);
  var extend4 = /* @__PURE__ */ extend3(encodeJsonJson);
  var assoc3 = /* @__PURE__ */ assoc2(encodeJsonJson);
  var gEncodeJsonCons2 = /* @__PURE__ */ gEncodeJsonCons(encodeJsonBoundary);
  var encodeJson2 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons2(gEncodeJsonNil)(toIsSymbol)())(fromIsSymbol)())());
  var encodeJson1 = /* @__PURE__ */ encodeJson(encodeJsonJsonValueType);
  var encodeJson22 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeSet2(ordJsonValueType)(encodeJsonJsonValueType));
  var singleton12 = /* @__PURE__ */ singleton4(plusArray);
  var paragraph3 = /* @__PURE__ */ paragraph(foldable1NonEmptyArray);
  var nes10 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Schema always fails validation.";
    }
  }));
  var nes15 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Invalid array:";
    }
  }));
  var unorderedList3 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var foldMap14 = /* @__PURE__ */ foldMap1(foldable1NonEmptySet)(semigroupNonEmptyArray);
  var append7 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var show12 = /* @__PURE__ */ show1(show1Number);
  var nes25 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " is not a multiple of ";
    }
  }));
  var show6 = /* @__PURE__ */ show(showNumber);
  var nes35 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " is outside of the valid range of ";
    }
  }));
  var nes45 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Non-unique array item.";
    }
  }));
  var nes53 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Invalid type. Expected ";
    }
  }));
  var fromFoldable14 = /* @__PURE__ */ fromFoldable3(foldableSet);
  var nes63 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "none";
    }
  }));
  var join1With2 = /* @__PURE__ */ join1With(foldable1NonEmptyArray);
  var map19 = /* @__PURE__ */ map(functorNonEmptyArray);
  var nes73 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " but got ";
    }
  }));
  var nes82 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return ".";
    }
  }));
  var nes92 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "JSON is valid against schema from 'not'.";
    }
  }));
  var nes102 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "JSON value path: ";
    }
  }));
  var nes11 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "JSON schema path: ";
    }
  }));
  var fromFoldable24 = /* @__PURE__ */ fromFoldable(/* @__PURE__ */ foldableNonEmpty(foldableArray));
  var foldl5 = /* @__PURE__ */ foldl(foldableArray);
  var insertWith2 = /* @__PURE__ */ insertWith(ordJsonValue);
  var add2 = /* @__PURE__ */ add(semiringInt);
  var filter5 = /* @__PURE__ */ filter3(ordJsonValue);
  var member3 = /* @__PURE__ */ member2(ordJsonValue);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var member1 = /* @__PURE__ */ member2(ordJsonValueType);
  var top4 = /* @__PURE__ */ top(boundedNumber);
  var bottom3 = /* @__PURE__ */ bottom(boundedNumber);
  var wrap4 = /* @__PURE__ */ wrap();
  var AlwaysFailingSchema = /* @__PURE__ */ function() {
    function AlwaysFailingSchema2() {
    }
    ;
    AlwaysFailingSchema2.value = new AlwaysFailingSchema2();
    return AlwaysFailingSchema2;
  }();
  var InvalidArray = /* @__PURE__ */ function() {
    function InvalidArray2(value0) {
      this.value0 = value0;
    }
    ;
    InvalidArray2.create = function(value0) {
      return new InvalidArray2(value0);
    };
    return InvalidArray2;
  }();
  var InvalidMultiple = /* @__PURE__ */ function() {
    function InvalidMultiple2(value0) {
      this.value0 = value0;
    }
    ;
    InvalidMultiple2.create = function(value0) {
      return new InvalidMultiple2(value0);
    };
    return InvalidMultiple2;
  }();
  var InvalidRange = /* @__PURE__ */ function() {
    function InvalidRange2(value0) {
      this.value0 = value0;
    }
    ;
    InvalidRange2.create = function(value0) {
      return new InvalidRange2(value0);
    };
    return InvalidRange2;
  }();
  var NonUniqueArrayItem = /* @__PURE__ */ function() {
    function NonUniqueArrayItem2() {
    }
    ;
    NonUniqueArrayItem2.value = new NonUniqueArrayItem2();
    return NonUniqueArrayItem2;
  }();
  var TypeMismatch = /* @__PURE__ */ function() {
    function TypeMismatch3(value0) {
      this.value0 = value0;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return new TypeMismatch3(value0);
    };
    return TypeMismatch3;
  }();
  var ValidAgainstNotSchema = /* @__PURE__ */ function() {
    function ValidAgainstNotSchema2() {
    }
    ;
    ValidAgainstNotSchema2.value = new ValidAgainstNotSchema2();
    return ValidAgainstNotSchema2;
  }();
  var eqViolationReason = {
    eq: function(x) {
      return function(y) {
        if (x instanceof AlwaysFailingSchema && y instanceof AlwaysFailingSchema) {
          return true;
        }
        ;
        if (x instanceof InvalidArray && y instanceof InvalidArray) {
          return eq(eqNonEmptySet(eqViolation))(x.value0)(y.value0);
        }
        ;
        if (x instanceof InvalidMultiple && y instanceof InvalidMultiple) {
          return x.value0.expectedMultiple === y.value0.expectedMultiple && x.value0.value === y.value0.value;
        }
        ;
        if (x instanceof InvalidRange && y instanceof InvalidRange) {
          return eq14(x.value0.validRange.from)(y.value0.validRange.from) && eq14(x.value0.validRange.to)(y.value0.validRange.to) && x.value0.value === y.value0.value;
        }
        ;
        if (x instanceof NonUniqueArrayItem && y instanceof NonUniqueArrayItem) {
          return true;
        }
        ;
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
          return eq22(x.value0.actualJsonValueType)(y.value0.actualJsonValueType) && eq32(x.value0.allowedJsonValueTypes)(y.value0.allowedJsonValueTypes);
        }
        ;
        if (x instanceof ValidAgainstNotSchema && y instanceof ValidAgainstNotSchema) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eqViolation = {
    eq: function(x) {
      return function(y) {
        return eq4(x.jsonPath)(y.jsonPath) && eq(eqViolationReason)(x.reason)(y.reason) && eq5(x.schemaPath)(y.schemaPath);
      };
    }
  };
  var ordViolationReason = {
    compare: function(x) {
      return function(y) {
        if (x instanceof AlwaysFailingSchema && y instanceof AlwaysFailingSchema) {
          return EQ.value;
        }
        ;
        if (x instanceof AlwaysFailingSchema) {
          return LT.value;
        }
        ;
        if (y instanceof AlwaysFailingSchema) {
          return GT.value;
        }
        ;
        if (x instanceof InvalidArray && y instanceof InvalidArray) {
          return compare(ordNonEmptySet(ordViolation))(x.value0)(y.value0);
        }
        ;
        if (x instanceof InvalidArray) {
          return LT.value;
        }
        ;
        if (y instanceof InvalidArray) {
          return GT.value;
        }
        ;
        if (x instanceof InvalidMultiple && y instanceof InvalidMultiple) {
          var v = compare5(x.value0.expectedMultiple)(y.value0.expectedMultiple);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare5(x.value0.value)(y.value0.value);
        }
        ;
        if (x instanceof InvalidMultiple) {
          return LT.value;
        }
        ;
        if (y instanceof InvalidMultiple) {
          return GT.value;
        }
        ;
        if (x instanceof InvalidRange && y instanceof InvalidRange) {
          var v = function() {
            var v1 = compare14(x.value0.validRange.from)(y.value0.validRange.from);
            if (v1 instanceof LT) {
              return LT.value;
            }
            ;
            if (v1 instanceof GT) {
              return GT.value;
            }
            ;
            return compare14(x.value0.validRange.to)(y.value0.validRange.to);
          }();
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare5(x.value0.value)(y.value0.value);
        }
        ;
        if (x instanceof InvalidRange) {
          return LT.value;
        }
        ;
        if (y instanceof InvalidRange) {
          return GT.value;
        }
        ;
        if (x instanceof NonUniqueArrayItem && y instanceof NonUniqueArrayItem) {
          return EQ.value;
        }
        ;
        if (x instanceof NonUniqueArrayItem) {
          return LT.value;
        }
        ;
        if (y instanceof NonUniqueArrayItem) {
          return GT.value;
        }
        ;
        if (x instanceof TypeMismatch && y instanceof TypeMismatch) {
          var v = compare22(x.value0.actualJsonValueType)(y.value0.actualJsonValueType);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare32(x.value0.allowedJsonValueTypes)(y.value0.allowedJsonValueTypes);
        }
        ;
        if (x instanceof TypeMismatch) {
          return LT.value;
        }
        ;
        if (y instanceof TypeMismatch) {
          return GT.value;
        }
        ;
        if (x instanceof ValidAgainstNotSchema && y instanceof ValidAgainstNotSchema) {
          return EQ.value;
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Validation (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqViolationReason;
    }
  };
  var ordViolation = {
    compare: function(x) {
      return function(y) {
        var v = compare42(x.jsonPath)(y.jsonPath);
        if (v instanceof LT) {
          return LT.value;
        }
        ;
        if (v instanceof GT) {
          return GT.value;
        }
        ;
        var v1 = compare(ordViolationReason)(x.reason)(y.reason);
        if (v1 instanceof LT) {
          return LT.value;
        }
        ;
        if (v1 instanceof GT) {
          return GT.value;
        }
        ;
        return compare52(x.schemaPath)(y.schemaPath);
      };
    },
    Eq0: function() {
      return eqViolation;
    }
  };
  var foldMapWithIndex2 = /* @__PURE__ */ foldMapWithIndex(foldableWithIndexArray)(/* @__PURE__ */ monoidSet(ordViolation));
  var append15 = /* @__PURE__ */ append(/* @__PURE__ */ semigroupSet(ordViolation));
  var encodeJsonViolationReason = {
    encodeJson: function(v) {
      if (v instanceof AlwaysFailingSchema) {
        return id("always failing schema");
      }
      ;
      if (v instanceof InvalidArray) {
        return id(map18(encodeJson($lazy_encodeJsonViolation(0)))(fromFoldable10(v.value0)));
      }
      ;
      if (v instanceof InvalidMultiple) {
        return extend4(assoc3("expectedMultiple")(id(v.value0.expectedMultiple)))(extend4(assoc3("value")(id(v.value0.value)))(jsonEmptyObject));
      }
      ;
      if (v instanceof InvalidRange) {
        return extend4(assoc3("validRange")(encodeJson2(v.value0.validRange)))(extend4(assoc3("value")(id(v.value0.value)))(jsonEmptyObject));
      }
      ;
      if (v instanceof NonUniqueArrayItem) {
        return id("non-unique array item");
      }
      ;
      if (v instanceof TypeMismatch) {
        return extend4(assoc3("actualJsonValueType")(encodeJson1(v.value0.actualJsonValueType)))(extend4(assoc3("allowedJsonValueTypes")(encodeJson22(v.value0.allowedJsonValueTypes)))(jsonEmptyObject));
      }
      ;
      if (v instanceof ValidAgainstNotSchema) {
        return id("valid against 'not' schema");
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Validation (line 90, column 16 - line 111, column 48): " + [v.constructor.name]);
    }
  };
  var $lazy_encodeJsonViolation = /* @__PURE__ */ $runtime_lazy4("encodeJsonViolation", "JsonSchema.Validation", /* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonList(encodeJsonJsonPathSegment))(/* @__PURE__ */ gEncodeJsonCons(encodeJsonViolationReason)(/* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonList(encodeJsonSchemaPathSegme))(gEncodeJsonNil)(schemaPathIsSymbol)())(reasonIsSymbol)())(jsonPathIsSymbol)()));
  var encodeJsonViolation = /* @__PURE__ */ $lazy_encodeJsonViolation(56);
  var documentViolationReason = {
    document: function(v) {
      if (v instanceof AlwaysFailingSchema) {
        return singleton12(paragraph3(singleton5(text(nes10($$Proxy.value)))));
      }
      ;
      if (v instanceof InvalidArray) {
        return new NonEmpty(paragraph3(singleton5(text(nes15($$Proxy.value)))), [unorderedList3(foldMap14(function() {
          var $390 = document2(documentViolation);
          return function($391) {
            return singleton5(fromNonEmpty($390($391)));
          };
        }())(v.value0))]);
      }
      ;
      if (v instanceof InvalidMultiple) {
        return singleton12(paragraph3(singleton5(text(append7(show12(v.value0.value))(appendString(nes25($$Proxy.value))(show6(v.value0.expectedMultiple)))))));
      }
      ;
      if (v instanceof InvalidRange) {
        return singleton12(paragraph3(cons$prime(text(prependString(show6(v.value0.value))(nes35($$Proxy.value))))([renderRange(v.value0.validRange)])));
      }
      ;
      if (v instanceof NonUniqueArrayItem) {
        return singleton12(paragraph3(singleton5(text(nes45($$Proxy.value)))));
      }
      ;
      if (v instanceof TypeMismatch) {
        return singleton12(paragraph3(singleton5(text(append7(nes53($$Proxy.value))(append7(function() {
          var v1 = fromFoldable14(v.value0.allowedJsonValueTypes);
          if (v1 instanceof Nothing) {
            return nes63($$Proxy.value);
          }
          ;
          if (v1 instanceof Just) {
            return join1With2(" or ")(map19(renderJsonValueType)(v1.value0));
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.Validation (line 163, column 15 - line 168, column 66): " + [v1.constructor.name]);
        }())(append7(nes73($$Proxy.value))(append7(renderJsonValueType(v.value0.actualJsonValueType))(nes82($$Proxy.value)))))))));
      }
      ;
      if (v instanceof ValidAgainstNotSchema) {
        return singleton12(paragraph3(singleton5(text(nes92($$Proxy.value)))));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Validation (line 117, column 14 - line 179, column 71): " + [v.constructor.name]);
    }
  };
  var documentViolation = {
    document: function(v) {
      return new NonEmpty(paragraph3(cons$prime(text(nes102($$Proxy.value)))([inlineCode(render3(v.jsonPath)), lineBreak, text(nes11($$Proxy.value)), inlineCode(render4(v.schemaPath))])), fromFoldable24(document2(documentViolationReason)(v.reason)));
    }
  };
  var validateUniqueItems = function(schemaPath) {
    return function(jsonPath) {
      return function(itemJsons) {
        var frequencies = foldl5(function(acc) {
          return function(json) {
            return insertWith2(add2)(json)(1)(acc);
          };
        })(empty3)(itemJsons);
        var duplicates = keys3(filter5(function(v) {
          return v > 1;
        })(frequencies));
        var f = function(itemIndex) {
          return function(itemJson) {
            var $352 = member3(itemJson)(duplicates);
            if ($352) {
              return singleton8({
                jsonPath: new Cons(new ItemIndex(itemIndex), jsonPath),
                reason: NonUniqueArrayItem.value,
                schemaPath
              });
            }
            ;
            return empty4;
          };
        };
        return foldMapWithIndex2(f)(itemJsons);
      };
    };
  };
  var validateTypeKeyword = function(schemaPath) {
    return function(jsonPath) {
      return function(json) {
        return function(allowedJsonValueTypes) {
          var jsonValueType = caseJson($$const(JsonNull.value))($$const(JsonBoolean.value))(function(x) {
            var $353 = isInteger(x);
            if ($353) {
              return JsonInteger.value;
            }
            ;
            return JsonNumber.value;
          })($$const(JsonString.value))($$const(JsonArray.value))($$const(JsonObject.value))(unwrap4(json));
          var integersAreAllowed = member1(JsonInteger.value)(allowedJsonValueTypes) || member1(JsonNumber.value)(allowedJsonValueTypes);
          var $354 = eq22(jsonValueType)(JsonInteger.value) && integersAreAllowed;
          if ($354) {
            return empty4;
          }
          ;
          var $355 = member1(jsonValueType)(allowedJsonValueTypes);
          if ($355) {
            return empty4;
          }
          ;
          return singleton8({
            jsonPath,
            reason: new TypeMismatch({
              actualJsonValueType: jsonValueType,
              allowedJsonValueTypes
            }),
            schemaPath: new Cons(TypeKeyword.value, schemaPath)
          });
        };
      };
    };
  };
  var validateMultipleOf = function(schemaPath) {
    return function(jsonPath) {
      return function(x) {
        return function(v) {
          if (v instanceof Just) {
            var $357 = isInteger(x / v.value0);
            if ($357) {
              return empty4;
            }
            ;
            return singleton8({
              jsonPath,
              reason: new InvalidMultiple({
                expectedMultiple: v.value0,
                value: x
              }),
              schemaPath: new Cons(MultipleOf.value, schemaPath)
            });
          }
          ;
          if (v instanceof Nothing) {
            return empty4;
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.Validation (line 419, column 44 - line 429, column 14): " + [v.constructor.name]);
        };
      };
    };
  };
  var validateNumber = function(schemaPath) {
    return function(jsonPath) {
      return function(constraints) {
        return function(x) {
          var validUpperBoundary = function() {
            if (constraints.exclusiveMaximum instanceof Nothing && constraints.maximum instanceof Nothing) {
              return new Open(top4);
            }
            ;
            if (constraints.exclusiveMaximum instanceof Nothing && constraints.maximum instanceof Just) {
              return new Closed(constraints.maximum.value0);
            }
            ;
            if (constraints.exclusiveMaximum instanceof Just && constraints.maximum instanceof Nothing) {
              return new Open(constraints.exclusiveMaximum.value0);
            }
            ;
            if (constraints.exclusiveMaximum instanceof Just && constraints.maximum instanceof Just) {
              var $363 = constraints.maximum.value0 > constraints.exclusiveMaximum.value0;
              if ($363) {
                return new Closed(constraints.maximum.value0);
              }
              ;
              return new Open(constraints.exclusiveMaximum.value0);
            }
            ;
            throw new Error("Failed pattern match at JsonSchema.Validation (line 382, column 5 - line 391, column 35): " + [constraints.exclusiveMaximum.constructor.name, constraints.maximum.constructor.name]);
          }();
          var validLowerBoundary = function() {
            if (constraints.exclusiveMinimum instanceof Nothing && constraints.minimum instanceof Nothing) {
              return new Open(bottom3);
            }
            ;
            if (constraints.exclusiveMinimum instanceof Nothing && constraints.minimum instanceof Just) {
              return new Closed(constraints.minimum.value0);
            }
            ;
            if (constraints.exclusiveMinimum instanceof Just && constraints.minimum instanceof Nothing) {
              return new Open(constraints.exclusiveMinimum.value0);
            }
            ;
            if (constraints.exclusiveMinimum instanceof Just && constraints.minimum instanceof Just) {
              var $370 = constraints.minimum.value0 < constraints.exclusiveMinimum.value0;
              if ($370) {
                return new Closed(constraints.minimum.value0);
              }
              ;
              return new Open(constraints.exclusiveMinimum.value0);
            }
            ;
            throw new Error("Failed pattern match at JsonSchema.Validation (line 369, column 5 - line 378, column 35): " + [constraints.exclusiveMinimum.constructor.name, constraints.minimum.constructor.name]);
          }();
          var validRange = {
            from: validLowerBoundary,
            to: validUpperBoundary
          };
          var minimumViolations = maybe(empty4)(function(minimum2) {
            var $373 = x >= minimum2;
            if ($373) {
              return empty4;
            }
            ;
            return singleton8({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(Minimum.value, schemaPath)
            });
          })(constraints.minimum);
          var maximumViolations = maybe(empty4)(function(maximum2) {
            var $374 = x <= maximum2;
            if ($374) {
              return empty4;
            }
            ;
            return singleton8({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(Maximum.value, schemaPath)
            });
          })(constraints.maximum);
          var exclusiveMinimumViolations = maybe(empty4)(function(exclusiveMinimum) {
            var $375 = x > exclusiveMinimum;
            if ($375) {
              return empty4;
            }
            ;
            return singleton8({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(ExclusiveMinimum.value, schemaPath)
            });
          })(constraints.exclusiveMinimum);
          var exclusiveMaximumViolations = maybe(empty4)(function(exclusiveMaximum) {
            var $376 = x < exclusiveMaximum;
            if ($376) {
              return empty4;
            }
            ;
            return singleton8({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(ExclusiveMaximum.value, schemaPath)
            });
          })(constraints.exclusiveMaximum);
          var rangeViolations = append15(exclusiveMaximumViolations)(append15(exclusiveMinimumViolations)(append15(maximumViolations)(minimumViolations)));
          return append15(validateMultipleOf(schemaPath)(jsonPath)(x)(constraints.multipleOf))(rangeViolations);
        };
      };
    };
  };
  var $lazy_validateAgainst = /* @__PURE__ */ $runtime_lazy4("validateAgainst", "JsonSchema.Validation", function() {
    var validateItems = function(schemaPath) {
      return function(jsonPath) {
        return function(itemJsons) {
          return function(schema) {
            var f = function(itemIndex) {
              return function(itemJson) {
                return go2(schemaPath)(new Cons(new ItemIndex(itemIndex), jsonPath))(itemJson)(schema);
              };
            };
            return foldMapWithIndex2(f)(itemJsons);
          };
        };
      };
    };
    var validateArray = function(schemaPath) {
      return function(jsonPath) {
        return function(array) {
          return function(constraints) {
            var uniqueItemsViolations = function() {
              if (constraints.uniqueItems) {
                return validateUniqueItems(new Cons(UniqueItems.value, schemaPath))(jsonPath)(array);
              }
              ;
              return empty4;
            }();
            var itemsViolations = maybe(empty4)(validateItems(new Cons(Items.value, schemaPath))(jsonPath)(array))(constraints.items);
            return append15(itemsViolations)(uniqueItemsViolations);
          };
        };
      };
    };
    var validateAgainstObjectSchema = function(schemaPath) {
      return function(jsonPath) {
        return function(json) {
          return function(keywords) {
            var typeKeywordViolations = maybe(empty4)(validateTypeKeyword(schemaPath)(jsonPath)(json))(keywords.typeKeyword);
            var notViolations = function() {
              if (keywords.not instanceof Just) {
                var $379 = isEmpty2($lazy_validateAgainst(235)(json)(keywords.not.value0));
                if ($379) {
                  return singleton8({
                    jsonPath,
                    reason: ValidAgainstNotSchema.value,
                    schemaPath
                  });
                }
                ;
                return empty4;
              }
              ;
              if (keywords.not instanceof Nothing) {
                return empty4;
              }
              ;
              throw new Error("Failed pattern match at JsonSchema.Validation (line 233, column 21 - line 243, column 18): " + [keywords.not.constructor.name]);
            }();
            return append15(notViolations)(append15(typeKeywordViolations)(caseJson($$const(empty4))($$const(empty4))(validateNumber(schemaPath)(jsonPath)(keywords))($$const(empty4))(function(array) {
              var shouldValidate = function() {
                if (keywords.typeKeyword instanceof Just) {
                  return member1(JsonArray.value)(keywords.typeKeyword.value0);
                }
                ;
                if (keywords.typeKeyword instanceof Nothing) {
                  return true;
                }
                ;
                throw new Error("Failed pattern match at JsonSchema.Validation (line 204, column 30 - line 208, column 21): " + [keywords.typeKeyword.constructor.name]);
              }();
              if (shouldValidate) {
                var itemViolations = validateArray(schemaPath)(jsonPath)(map18(wrap4)(array))(keywords);
                var v = fromSet(itemViolations);
                if (v instanceof Just) {
                  return singleton8({
                    jsonPath,
                    reason: new InvalidArray(v.value0),
                    schemaPath
                  });
                }
                ;
                if (v instanceof Nothing) {
                  return empty4;
                }
                ;
                throw new Error("Failed pattern match at JsonSchema.Validation (line 218, column 17 - line 226, column 30): " + [v.constructor.name]);
              }
              ;
              return empty4;
            })($$const(empty4))(unwrap4(json))));
          };
        };
      };
    };
    var go2 = function(schemaPath) {
      return function(jsonPath) {
        return function(json) {
          return function(schema) {
            if (schema instanceof BooleanSchema) {
              if (schema.value0) {
                return empty4;
              }
              ;
              return singleton8({
                jsonPath,
                reason: AlwaysFailingSchema.value,
                schemaPath
              });
            }
            ;
            if (schema instanceof ObjectSchema) {
              return validateAgainstObjectSchema(schemaPath)(jsonPath)(json)(schema.value0);
            }
            ;
            throw new Error("Failed pattern match at JsonSchema.Validation (line 185, column 40 - line 192, column 68): " + [schema.constructor.name]);
          };
        };
      };
    };
    return go2(Nil.value)(Nil.value);
  });
  var validateAgainst = /* @__PURE__ */ $lazy_validateAgainst(181);

  // output/CLI.Validate/index.js
  var bind3 = /* @__PURE__ */ bind(bindEither);
  var map20 = /* @__PURE__ */ map(functorEither);
  var wrap5 = /* @__PURE__ */ wrap();
  var compute = function(v) {
    var parseSchema2 = function(s) {
      return bind3(map20(wrap5)(jsonParser(s)))(function(json) {
        return parseSchema(json);
      });
    };
    var parseJson = function() {
      var $17 = map20(wrap5);
      return function($18) {
        return $17(jsonParser($18));
      };
    }();
    return bind3(function() {
      var v1 = parseSchema2(v.schemaText);
      if (v1 instanceof Left) {
        return new Left("Failed to parse the JSON schema document: " + v1.value0);
      }
      ;
      if (v1 instanceof Right) {
        return new Right(v1.value0);
      }
      ;
      throw new Error("Failed pattern match at CLI.Validate (line 34, column 12 - line 39, column 19): " + [v1.constructor.name]);
    }())(function(schema) {
      return bind3(function() {
        var v1 = parseJson(v.jsonText);
        if (v1 instanceof Left) {
          return new Left("Failed to parse the JSON value document: " + v1.value0);
        }
        ;
        if (v1 instanceof Right) {
          return new Right(v1.value0);
        }
        ;
        throw new Error("Failed pattern match at CLI.Validate (line 41, column 15 - line 46, column 17): " + [v1.constructor.name]);
      }())(function(jsonValue) {
        return new Right(validateAgainst(jsonValue)(schema));
      });
    });
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var modify_2 = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };

  // output/DOM.HTML.Indexed.ButtonType/index.js
  var ButtonButton = /* @__PURE__ */ function() {
    function ButtonButton2() {
    }
    ;
    ButtonButton2.value = new ButtonButton2();
    return ButtonButton2;
  }();
  var ButtonSubmit = /* @__PURE__ */ function() {
    function ButtonSubmit2() {
    }
    ;
    ButtonSubmit2.value = new ButtonSubmit2();
    return ButtonSubmit2;
  }();
  var ButtonReset = /* @__PURE__ */ function() {
    function ButtonReset2() {
    }
    ;
    ButtonReset2.value = new ButtonReset2();
    return ButtonReset2;
  }();
  var renderButtonType = function(v) {
    if (v instanceof ButtonButton) {
      return "button";
    }
    ;
    if (v instanceof ButtonSubmit) {
      return "submit";
    }
    ;
    if (v instanceof ButtonReset) {
      return "reset";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.ButtonType (line 14, column 20 - line 17, column 25): " + [v.constructor.name]);
  };

  // output/Effect.Aff/foreign.js
  var Aff = function() {
    var EMPTY = {};
    var PURE = "Pure";
    var THROW = "Throw";
    var CATCH = "Catch";
    var SYNC = "Sync";
    var ASYNC = "Async";
    var BIND = "Bind";
    var BRACKET = "Bracket";
    var FORK = "Fork";
    var SEQ = "Sequential";
    var MAP = "Map";
    var APPLY = "Apply";
    var ALT = "Alt";
    var CONS = "Cons";
    var RESUME = "Resume";
    var RELEASE = "Release";
    var FINALIZER = "Finalizer";
    var FINALIZED = "Finalized";
    var FORKED = "Forked";
    var FIBER = "Fiber";
    var THUNK = "Thunk";
    function Aff2(tag, _1, _2, _3) {
      this.tag = tag;
      this._1 = _1;
      this._2 = _2;
      this._3 = _3;
    }
    function AffCtr(tag) {
      var fn = function(_1, _2, _3) {
        return new Aff2(tag, _1, _2, _3);
      };
      fn.tag = tag;
      return fn;
    }
    function nonCanceler2(error4) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error4) {
        setTimeout(function() {
          throw error4;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error4) {
        return left(error4);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error4) {
        k(left(error4))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size6 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk;
        draining = true;
        while (size6 !== 0) {
          size6--;
          thunk = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i2, tmp;
          if (size6 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size6) % limit] = cb;
          size6++;
          if (!draining) {
            drain();
          }
        }
      };
    }();
    function Supervisor(util) {
      var fibers = {};
      var fiberId = 0;
      var count = 0;
      return {
        register: function(fiber) {
          var fid = fiberId++;
          fiber.onComplete({
            rethrow: true,
            handler: function(result) {
              return function() {
                count--;
                delete fibers[fid];
              };
            }
          })();
          fibers[fid] = fiber;
          count++;
        },
        isEmpty: function() {
          return count === 0;
        },
        killAll: function(killError, cb) {
          return function() {
            if (count === 0) {
              return cb();
            }
            var killCount = 0;
            var kills = {};
            function kill2(fid) {
              kills[fid] = fibers[fid].kill(killError, function(result) {
                return function() {
                  delete kills[fid];
                  killCount--;
                  if (util.isLeft(result) && util.fromLeft(result)) {
                    setTimeout(function() {
                      throw util.fromLeft(result);
                    }, 0);
                  }
                  if (killCount === 0) {
                    cb();
                  }
                };
              })();
            }
            for (var k in fibers) {
              if (fibers.hasOwnProperty(k)) {
                killCount++;
                kill2(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error4) {
              return new Aff2(SYNC, function() {
                for (var k2 in kills) {
                  if (kills.hasOwnProperty(k2)) {
                    kills[k2]();
                  }
                }
              });
            };
          };
        }
      };
    }
    var SUSPENDED = 0;
    var CONTINUE = 1;
    var STEP_BIND = 2;
    var STEP_RESULT = 3;
    var PENDING = 4;
    var RETURN = 5;
    var COMPLETED = 6;
    function Fiber(util, supervisor, aff) {
      var runTick = 0;
      var status = SUSPENDED;
      var step3 = aff;
      var fail2 = null;
      var interrupt = null;
      var bhead = null;
      var btail = null;
      var attempts = null;
      var bracketCount = 0;
      var joinId = 0;
      var joins = null;
      var rethrow = true;
      function run3(localRunTick) {
        var tmp, result, attempt;
        while (true) {
          tmp = null;
          result = null;
          attempt = null;
          switch (status) {
            case STEP_BIND:
              status = CONTINUE;
              try {
                step3 = bhead(step3);
                if (btail === null) {
                  bhead = null;
                } else {
                  bhead = btail._1;
                  btail = btail._2;
                }
              } catch (e) {
                status = RETURN;
                fail2 = util.left(e);
                step3 = null;
              }
              break;
            case STEP_RESULT:
              if (util.isLeft(step3)) {
                status = RETURN;
                fail2 = step3;
                step3 = null;
              } else if (bhead === null) {
                status = RETURN;
              } else {
                status = STEP_BIND;
                step3 = util.fromRight(step3);
              }
              break;
            case CONTINUE:
              switch (step3.tag) {
                case BIND:
                  if (bhead) {
                    btail = new Aff2(CONS, bhead, btail);
                  }
                  bhead = step3._2;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                case PURE:
                  if (bhead === null) {
                    status = RETURN;
                    step3 = util.right(step3._1);
                  } else {
                    status = STEP_BIND;
                    step3 = step3._1;
                  }
                  break;
                case SYNC:
                  status = STEP_RESULT;
                  step3 = runSync(util.left, util.right, step3._1);
                  break;
                case ASYNC:
                  status = PENDING;
                  step3 = runAsync(util.left, step3._1, function(result2) {
                    return function() {
                      if (runTick !== localRunTick) {
                        return;
                      }
                      runTick++;
                      Scheduler.enqueue(function() {
                        if (runTick !== localRunTick + 1) {
                          return;
                        }
                        status = STEP_RESULT;
                        step3 = result2;
                        run3(runTick);
                      });
                    };
                  });
                  return;
                case THROW:
                  status = RETURN;
                  fail2 = util.left(step3._1);
                  step3 = null;
                  break;
                case CATCH:
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step3, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                case BRACKET:
                  bracketCount++;
                  if (bhead === null) {
                    attempts = new Aff2(CONS, step3, attempts, interrupt);
                  } else {
                    attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                  }
                  bhead = null;
                  btail = null;
                  status = CONTINUE;
                  step3 = step3._1;
                  break;
                case FORK:
                  status = STEP_RESULT;
                  tmp = Fiber(util, supervisor, step3._2);
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
                  if (step3._1) {
                    tmp.run();
                  }
                  step3 = util.right(tmp);
                  break;
                case SEQ:
                  status = CONTINUE;
                  step3 = sequential3(util, supervisor, step3._1);
                  break;
              }
              break;
            case RETURN:
              bhead = null;
              btail = null;
              if (attempts === null) {
                status = COMPLETED;
                step3 = interrupt || fail2 || step3;
              } else {
                tmp = attempts._3;
                attempt = attempts._1;
                attempts = attempts._2;
                switch (attempt.tag) {
                  case CATCH:
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      status = RETURN;
                    } else if (fail2) {
                      status = CONTINUE;
                      step3 = attempt._2(util.fromLeft(fail2));
                      fail2 = null;
                    }
                    break;
                  case RESUME:
                    if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                      status = RETURN;
                    } else {
                      bhead = attempt._1;
                      btail = attempt._2;
                      status = STEP_BIND;
                      step3 = util.fromRight(step3);
                    }
                    break;
                  case BRACKET:
                    bracketCount--;
                    if (fail2 === null) {
                      result = util.fromRight(step3);
                      attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                      if (interrupt === tmp || bracketCount > 0) {
                        status = CONTINUE;
                        step3 = attempt._3(result);
                      }
                    }
                    break;
                  case RELEASE:
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                    status = CONTINUE;
                    if (interrupt && interrupt !== tmp && bracketCount === 0) {
                      step3 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                    } else if (fail2) {
                      step3 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                    } else {
                      step3 = attempt._1.completed(util.fromRight(step3))(attempt._2);
                    }
                    fail2 = null;
                    bracketCount++;
                    break;
                  case FINALIZER:
                    bracketCount++;
                    attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                    status = CONTINUE;
                    step3 = attempt._1;
                    break;
                  case FINALIZED:
                    bracketCount--;
                    status = RETURN;
                    step3 = attempt._1;
                    fail2 = attempt._2;
                    break;
                }
              }
              break;
            case COMPLETED:
              for (var k in joins) {
                if (joins.hasOwnProperty(k)) {
                  rethrow = rethrow && joins[k].rethrow;
                  runEff(joins[k].handler(step3));
                }
              }
              joins = null;
              if (interrupt && fail2) {
                setTimeout(function() {
                  throw util.fromLeft(fail2);
                }, 0);
              } else if (util.isLeft(step3) && rethrow) {
                setTimeout(function() {
                  if (rethrow) {
                    throw util.fromLeft(step3);
                  }
                }, 0);
              }
              return;
            case SUSPENDED:
              status = CONTINUE;
              break;
            case PENDING:
              return;
          }
        }
      }
      function onComplete(join4) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join4.rethrow;
            join4.handler(step3)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join4;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill2(error4, cb) {
        return function() {
          if (status === COMPLETED) {
            cb(util.right(void 0))();
            return function() {
            };
          }
          var canceler = onComplete({
            rethrow: false,
            handler: function() {
              return cb(util.right(void 0));
            }
          })();
          switch (status) {
            case SUSPENDED:
              interrupt = util.left(error4);
              status = COMPLETED;
              step3 = interrupt;
              run3(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step3(error4)), attempts, interrupt);
                }
                status = RETURN;
                step3 = null;
                fail2 = null;
                run3(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error4);
              }
              if (bracketCount === 0) {
                status = RETURN;
                step3 = null;
                fail2 = null;
              }
          }
          return canceler;
        };
      }
      function join3(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run3(runTick);
          }
          return canceler;
        };
      }
      return {
        kill: kill2,
        join: join3,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run3(runTick);
              });
            } else {
              run3(runTick);
            }
          }
        }
      };
    }
    function runPar(util, supervisor, par, cb) {
      var fiberId = 0;
      var fibers = {};
      var killId = 0;
      var kills = {};
      var early = new Error("[ParAff] Early exit");
      var interrupt = null;
      var root = EMPTY;
      function kill2(error4, par2, cb2) {
        var step3 = par2;
        var head3 = null;
        var tail2 = null;
        var count = 0;
        var kills2 = {};
        var tmp, kid;
        loop:
          while (true) {
            tmp = null;
            switch (step3.tag) {
              case FORKED:
                if (step3._3 === EMPTY) {
                  tmp = fibers[step3._1];
                  kills2[count++] = tmp.kill(error4, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head3 === null) {
                  break loop;
                }
                step3 = head3._2;
                if (tail2 === null) {
                  head3 = null;
                } else {
                  head3 = tail2._1;
                  tail2 = tail2._2;
                }
                break;
              case MAP:
                step3 = step3._2;
                break;
              case APPLY:
              case ALT:
                if (head3) {
                  tail2 = new Aff2(CONS, head3, tail2);
                }
                head3 = step3;
                step3 = step3._1;
                break;
            }
          }
        if (count === 0) {
          cb2(util.right(void 0))();
        } else {
          kid = 0;
          tmp = count;
          for (; kid < tmp; kid++) {
            kills2[kid] = kills2[kid]();
          }
        }
        return kills2;
      }
      function join3(result, head3, tail2) {
        var fail2, step3, lhs, rhs, tmp, kid;
        if (util.isLeft(result)) {
          fail2 = result;
          step3 = null;
        } else {
          step3 = result;
          fail2 = null;
        }
        loop:
          while (true) {
            lhs = null;
            rhs = null;
            tmp = null;
            kid = null;
            if (interrupt !== null) {
              return;
            }
            if (head3 === null) {
              cb(fail2 || step3)();
              return;
            }
            if (head3._3 !== EMPTY) {
              return;
            }
            switch (head3.tag) {
              case MAP:
                if (fail2 === null) {
                  head3._3 = util.right(head3._1(util.fromRight(step3)));
                  step3 = head3._3;
                } else {
                  head3._3 = fail2;
                }
                break;
              case APPLY:
                lhs = head3._1._3;
                rhs = head3._2._3;
                if (fail2) {
                  head3._3 = fail2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, fail2 === lhs ? head3._2 : head3._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(fail2, null, null);
                      } else {
                        join3(fail2, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                } else if (lhs === EMPTY || rhs === EMPTY) {
                  return;
                } else {
                  step3 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                  head3._3 = step3;
                }
                break;
              case ALT:
                lhs = head3._1._3;
                rhs = head3._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail2 = step3 === lhs ? rhs : lhs;
                  step3 = null;
                  head3._3 = fail2;
                } else {
                  head3._3 = step3;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill2(early, step3 === lhs ? head3._2 : head3._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail2 === null) {
                        join3(step3, null, null);
                      } else {
                        join3(step3, tail2._1, tail2._2);
                      }
                    };
                  });
                  if (tmp) {
                    tmp = false;
                    return;
                  }
                }
                break;
            }
            if (tail2 === null) {
              head3 = null;
            } else {
              head3 = tail2._1;
              tail2 = tail2._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join3(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run3() {
        var status = CONTINUE;
        var step3 = par;
        var head3 = null;
        var tail2 = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step3.tag) {
                  case MAP:
                    if (head3) {
                      tail2 = new Aff2(CONS, head3, tail2);
                    }
                    head3 = new Aff2(MAP, step3._1, EMPTY, EMPTY);
                    step3 = step3._2;
                    break;
                  case APPLY:
                    if (head3) {
                      tail2 = new Aff2(CONS, head3, tail2);
                    }
                    head3 = new Aff2(APPLY, EMPTY, step3._2, EMPTY);
                    step3 = step3._1;
                    break;
                  case ALT:
                    if (head3) {
                      tail2 = new Aff2(CONS, head3, tail2);
                    }
                    head3 = new Aff2(ALT, EMPTY, step3._2, EMPTY);
                    step3 = step3._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step3;
                    step3 = new Aff2(FORKED, fid, new Aff2(CONS, head3, tail2), EMPTY);
                    tmp = Fiber(util, supervisor, tmp);
                    tmp.onComplete({
                      rethrow: false,
                      handler: resolve(step3)
                    })();
                    fibers[fid] = tmp;
                    if (supervisor) {
                      supervisor.register(tmp);
                    }
                }
                break;
              case RETURN:
                if (head3 === null) {
                  break loop;
                }
                if (head3._1 === EMPTY) {
                  head3._1 = step3;
                  status = CONTINUE;
                  step3 = head3._2;
                  head3._2 = EMPTY;
                } else {
                  head3._2 = step3;
                  step3 = head3;
                  if (tail2 === null) {
                    head3 = null;
                  } else {
                    head3 = tail2._1;
                    tail2 = tail2._2;
                  }
                }
            }
          }
        root = step3;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error4, cb2) {
        interrupt = util.left(error4);
        var innerKills;
        for (var kid in kills) {
          if (kills.hasOwnProperty(kid)) {
            innerKills = kills[kid];
            for (kid in innerKills) {
              if (innerKills.hasOwnProperty(kid)) {
                innerKills[kid]();
              }
            }
          }
        }
        kills = null;
        var newKills = kill2(error4, root, cb2);
        return function(killError) {
          return new Aff2(ASYNC, function(killCb) {
            return function() {
              for (var kid2 in newKills) {
                if (newKills.hasOwnProperty(kid2)) {
                  newKills[kid2]();
                }
              }
              return nonCanceler2;
            };
          });
        };
      }
      run3();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential3(util, supervisor, par) {
      return new Aff2(ASYNC, function(cb) {
        return function() {
          return runPar(util, supervisor, par, cb);
        };
      });
    }
    Aff2.EMPTY = EMPTY;
    Aff2.Pure = AffCtr(PURE);
    Aff2.Throw = AffCtr(THROW);
    Aff2.Catch = AffCtr(CATCH);
    Aff2.Sync = AffCtr(SYNC);
    Aff2.Async = AffCtr(ASYNC);
    Aff2.Bind = AffCtr(BIND);
    Aff2.Bracket = AffCtr(BRACKET);
    Aff2.Fork = AffCtr(FORK);
    Aff2.Seq = AffCtr(SEQ);
    Aff2.ParMap = AffCtr(MAP);
    Aff2.ParApply = AffCtr(APPLY);
    Aff2.ParAlt = AffCtr(ALT);
    Aff2.Fiber = Fiber;
    Aff2.Supervisor = Supervisor;
    Aff2.Scheduler = Scheduler;
    Aff2.nonCanceler = nonCanceler2;
    return Aff2;
  }();
  var _pure = Aff.Pure;
  var _throwError = Aff.Throw;
  function _catchError(aff) {
    return function(k) {
      return Aff.Catch(aff, k);
    };
  }
  function _map(f) {
    return function(aff) {
      if (aff.tag === Aff.Pure.tag) {
        return Aff.Pure(f(aff._1));
      } else {
        return Aff.Bind(aff, function(value14) {
          return Aff.Pure(f(value14));
        });
      }
    };
  }
  function _bind(aff) {
    return function(k) {
      return Aff.Bind(aff, k);
    };
  }
  function _fork(immediate) {
    return function(aff) {
      return Aff.Fork(immediate, aff);
    };
  }
  var _liftEffect = Aff.Sync;
  function _parAffMap(f) {
    return function(aff) {
      return Aff.ParMap(f, aff);
    };
  }
  function _parAffApply(aff1) {
    return function(aff2) {
      return Aff.ParApply(aff1, aff2);
    };
  }
  var makeAff = Aff.Async;
  function generalBracket(acquire) {
    return function(options2) {
      return function(k) {
        return Aff.Bracket(acquire, options2, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _delay = function() {
    function setDelay(n, k) {
      if (n === 0 && typeof setImmediate !== "undefined") {
        return setImmediate(k);
      } else {
        return setTimeout(k, n);
      }
    }
    function clearDelay(n, t) {
      if (n === 0 && typeof clearImmediate !== "undefined") {
        return clearImmediate(t);
      } else {
        return clearTimeout(t);
      }
    }
    return function(right, ms) {
      return Aff.Async(function(cb) {
        return function() {
          var timer = setDelay(ms, cb(right()));
          return function() {
            return Aff.Sync(function() {
              return right(clearDelay(ms, timer));
            });
          };
        };
      });
    };
  }();
  var _sequential = Aff.Seq;

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }

  // output/Effect.Exception/index.js
  var $$throw = function($4) {
    return throwException(error($4));
  };

  // output/Control.Monad.Error.Class/index.js
  var throwError = function(dict) {
    return dict.throwError;
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map36 = map(Monad0.Bind1().Apply0().Functor0());
    var pure10 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map36(Right.create)(a2))(function($52) {
        return pure10(Left.create($52));
      });
    };
  };

  // output/Effect.Class/index.js
  var monadEffectEffect = {
    liftEffect: /* @__PURE__ */ identity(categoryFn),
    Monad0: function() {
      return monadEffect;
    }
  };
  var liftEffect = function(dict) {
    return dict.liftEffect;
  };

  // output/Control.Monad.Except.Trans/index.js
  var map21 = /* @__PURE__ */ map(functorEither);
  var ExceptT = function(x) {
    return x;
  };
  var runExceptT = function(v) {
    return v;
  };
  var mapExceptT = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var functorExceptT = function(dictFunctor) {
    var map113 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map113(map21(f)));
      }
    };
  };
  var monadExceptT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeExceptT(dictMonad);
      },
      Bind1: function() {
        return bindExceptT(dictMonad);
      }
    };
  };
  var bindExceptT = function(dictMonad) {
    var bind7 = bind(dictMonad.Bind1());
    var pure10 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind7(v)(either(function($187) {
            return pure10(Left.create($187));
          })(function(a2) {
            var v1 = k(a2);
            return v1;
          }));
        };
      },
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var applyExceptT = function(dictMonad) {
    var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadExceptT(dictMonad)),
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
  var applicativeExceptT = function(dictMonad) {
    return {
      pure: function() {
        var $188 = pure(dictMonad.Applicative0());
        return function($189) {
          return ExceptT($188(Right.create($189)));
        };
      }(),
      Apply0: function() {
        return applyExceptT(dictMonad);
      }
    };
  };
  var monadThrowExceptT = function(dictMonad) {
    var monadExceptT1 = monadExceptT(dictMonad);
    return {
      throwError: function() {
        var $198 = pure(dictMonad.Applicative0());
        return function($199) {
          return ExceptT($198(Left.create($199)));
        };
      }(),
      Monad0: function() {
        return monadExceptT1;
      }
    };
  };

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Control.Parallel/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
  var parTraverse_ = function(dictParallel) {
    var sequential3 = sequential(dictParallel);
    var parallel4 = parallel(dictParallel);
    return function(dictApplicative) {
      var traverse_7 = traverse_(dictApplicative);
      return function(dictFoldable) {
        var traverse_14 = traverse_7(dictFoldable);
        return function(f) {
          var $51 = traverse_14(function($53) {
            return parallel4(f($53));
          });
          return function($52) {
            return sequential3($51($52));
          };
        };
      };
    };
  };
  var parSequence_ = function(dictParallel) {
    var parTraverse_1 = parTraverse_(dictParallel);
    return function(dictApplicative) {
      var parTraverse_2 = parTraverse_1(dictApplicative);
      return function(dictFoldable) {
        return parTraverse_2(dictFoldable)(identity5);
      };
    };
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy5 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure3 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var map23 = /* @__PURE__ */ map(functorEffect);
  var Canceler = function(x) {
    return x;
  };
  var suspendAff = /* @__PURE__ */ _fork(false);
  var functorParAff = {
    map: _parAffMap
  };
  var functorAff = {
    map: _map
  };
  var map110 = /* @__PURE__ */ map(functorAff);
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = /* @__PURE__ */ function() {
    var unsafeFromRight = function(v) {
      if (v instanceof Right) {
        return v.value0;
      }
      ;
      if (v instanceof Left) {
        return unsafeCrashWith("unsafeFromRight: Left");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
    };
    var unsafeFromLeft = function(v) {
      if (v instanceof Left) {
        return v.value0;
      }
      ;
      if (v instanceof Right) {
        return unsafeCrashWith("unsafeFromLeft: Right");
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
    };
    var isLeft = function(v) {
      if (v instanceof Left) {
        return true;
      }
      ;
      if (v instanceof Right) {
        return false;
      }
      ;
      throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
    };
    return {
      isLeft,
      fromLeft: unsafeFromLeft,
      fromRight: unsafeFromRight,
      left: Left.create,
      right: Right.create
    };
  }();
  var makeFiber = function(aff) {
    return _makeFiber(ffiUtil, aff);
  };
  var launchAff = function(aff) {
    return function __do2() {
      var fiber = makeFiber(aff)();
      fiber.run();
      return fiber;
    };
  };
  var bracket = function(acquire) {
    return function(completed) {
      return generalBracket(acquire)({
        killed: $$const(completed),
        failed: $$const(completed),
        completed: $$const(completed)
      });
    };
  };
  var applyParAff = {
    apply: _parAffApply,
    Functor0: function() {
      return functorParAff;
    }
  };
  var monadAff = {
    Applicative0: function() {
      return applicativeAff;
    },
    Bind1: function() {
      return bindAff;
    }
  };
  var bindAff = {
    bind: _bind,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var applicativeAff = {
    pure: _pure,
    Apply0: function() {
      return $lazy_applyAff(0);
    }
  };
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy5("applyAff", "Effect.Aff", function() {
    return {
      apply: ap(monadAff),
      Functor0: function() {
        return functorAff;
      }
    };
  });
  var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
  var pure22 = /* @__PURE__ */ pure(applicativeAff);
  var bind1 = /* @__PURE__ */ bind(bindAff);
  var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindAff);
  var $$finally = function(fin) {
    return function(a2) {
      return bracket(pure22(unit))($$const(fin))($$const(a2));
    };
  };
  var parallelAff = {
    parallel: unsafeCoerce2,
    sequential: _sequential,
    Apply0: function() {
      return applyAff;
    },
    Apply1: function() {
      return applyParAff;
    }
  };
  var parallel2 = /* @__PURE__ */ parallel(parallelAff);
  var applicativeParAff = {
    pure: function($76) {
      return parallel2(pure22($76));
    },
    Apply0: function() {
      return applyParAff;
    }
  };
  var monadEffectAff = {
    liftEffect: _liftEffect,
    Monad0: function() {
      return monadAff;
    }
  };
  var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var effectCanceler = function($77) {
    return Canceler($$const(liftEffect2($77)));
  };
  var joinFiber = function(v) {
    return makeAff(function(k) {
      return map23(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map110(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void4(v.kill(e, $$const(pure3(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map23(effectCanceler)(v.kill(e, k));
        });
      });
    };
  };
  var monadThrowAff = {
    throwError: _throwError,
    Monad0: function() {
      return monadAff;
    }
  };
  var monadErrorAff = {
    catchError: _catchError,
    MonadThrow0: function() {
      return monadThrowAff;
    }
  };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = function(k) {
    return function(aff) {
      return launchAff(bindFlipped4(function($83) {
        return liftEffect2(k($83));
      })($$try2(aff)));
    };
  };
  var runAff_ = function(k) {
    return function(aff) {
      return $$void4(runAff(k)(aff));
    };
  };
  var monadRecAff = {
    tailRecM: function(k) {
      var go2 = function(a2) {
        return bind1(k(a2))(function(res) {
          if (res instanceof Done) {
            return pure22(res.value0);
          }
          ;
          if (res instanceof Loop) {
            return go2(res.value0);
          }
          ;
          throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
        });
      };
      return go2;
    },
    Monad0: function() {
      return monadAff;
    }
  };
  var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var children = getEffProp("children");
  var _firstElementChild = getEffProp("firstElementChild");
  var _lastElementChild = getEffProp("lastElementChild");
  var childElementCount = getEffProp("childElementCount");
  function _querySelector(selector) {
    return function(node) {
      return function() {
        return node.querySelector(selector);
      };
    };
  }

  // output/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a2, r, f) {
    return a2 == null ? r : f(a2);
  }
  function notNull(x) {
    return x;
  }

  // output/Data.Nullable/index.js
  var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
  var toMaybe = function(n) {
    return nullable(n, Nothing.value, Just.create);
  };

  // output/Web.DOM.ParentNode/index.js
  var map24 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map24(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.Event.EventTarget/foreign.js
  function eventListener(fn) {
    return function() {
      return function(event) {
        return fn(event)();
      };
    };
  }
  function addEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target6) {
          return function() {
            return target6.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }

  // output/Web.HTML.HTMLDocument.ReadyState/index.js
  var Loading = /* @__PURE__ */ function() {
    function Loading2() {
    }
    ;
    Loading2.value = new Loading2();
    return Loading2;
  }();
  var Interactive = /* @__PURE__ */ function() {
    function Interactive2() {
    }
    ;
    Interactive2.value = new Interactive2();
    return Interactive2;
  }();
  var Complete = /* @__PURE__ */ function() {
    function Complete2() {
    }
    ;
    Complete2.value = new Complete2();
    return Complete2;
  }();
  var parse = function(v) {
    if (v === "loading") {
      return new Just(Loading.value);
    }
    ;
    if (v === "interactive") {
      return new Just(Interactive.value);
    }
    ;
    if (v === "complete") {
      return new Just(Complete.value);
    }
    ;
    return Nothing.value;
  };

  // output/Web.HTML.HTMLDocument/index.js
  var map25 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map25(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }())(function() {
      return _readyState(doc);
    });
  };

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value14) {
    var tag = Object.prototype.toString.call(value14);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value14);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Web.HTML.Window/foreign.js
  function document4(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Web.HTML.Event.EventTypes/index.js
  var input = "input";
  var domcontentloaded = "DOMContentLoaded";

  // output/Halogen.Aff.Util/index.js
  var bind4 = /* @__PURE__ */ bind(bindAff);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure4 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure1 = /* @__PURE__ */ pure(applicativeEffect);
  var map26 = /* @__PURE__ */ map(functorEffect);
  var discard2 = /* @__PURE__ */ discard(discardUnit);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query2) {
    return bind4(liftEffect3(bindFlipped5(composeKleisliFlipped3(function() {
      var $16 = querySelector(query2);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document4))(windowImpl)))(function(mel) {
      return pure4(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped5(readyState)(bindFlipped5(document4)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map26(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
    return bind4(selectElement("body"))(function(body2) {
      return maybe(throwError2(error("Could not find body")))(pure4)(body2);
    });
  });

  // output/Data.Exists/index.js
  var runExists = unsafeCoerce2;
  var mkExists = unsafeCoerce2;

  // output/Data.Coyoneda/index.js
  var CoyonedaF = /* @__PURE__ */ function() {
    function CoyonedaF2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CoyonedaF2.create = function(value0) {
      return function(value1) {
        return new CoyonedaF2(value0, value1);
      };
    };
    return CoyonedaF2;
  }();
  var unCoyoneda = function(f) {
    return function(v) {
      return runExists(function(v1) {
        return f(v1.value0)(v1.value1);
      })(v);
    };
  };
  var coyoneda = function(k) {
    return function(fi) {
      return mkExists(new CoyonedaF(k, fi));
    };
  };
  var functorCoyoneda = {
    map: function(f) {
      return function(v) {
        return runExists(function(v1) {
          return coyoneda(function($180) {
            return f(v1.value0($180));
          })(v1.value1);
        })(v);
      };
    }
  };
  var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

  // output/Halogen.Data.Slot/index.js
  var foreachSlot = function(dictApplicative) {
    var traverse_7 = traverse_(dictApplicative)(foldableMap);
    return function(v) {
      return function(k) {
        return traverse_7(function($54) {
          return k($54);
        })(v);
      };
    };
  };
  var empty5 = empty3;

  // output/Halogen.Query.Input/index.js
  var RefUpdate = /* @__PURE__ */ function() {
    function RefUpdate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    RefUpdate2.create = function(value0) {
      return function(value1) {
        return new RefUpdate2(value0, value1);
      };
    };
    return RefUpdate2;
  }();
  var Action = /* @__PURE__ */ function() {
    function Action3(value0) {
      this.value0 = value0;
    }
    ;
    Action3.create = function(value0) {
      return new Action3(value0);
    };
    return Action3;
  }();

  // output/Halogen.VDom.Machine/index.js
  var Step = /* @__PURE__ */ function() {
    function Step3(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Step3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Step3(value0, value1, value22, value32);
          };
        };
      };
    };
    return Step3;
  }();
  var unStep = unsafeCoerce2;
  var step2 = function(v, a2) {
    return v.value2(v.value1, a2);
  };
  var mkStep = unsafeCoerce2;
  var halt = function(v) {
    return v.value3(v.value1);
  };
  var extract2 = /* @__PURE__ */ unStep(function(v) {
    return v.value0;
  });

  // output/Halogen.VDom.Types/index.js
  var map27 = /* @__PURE__ */ map(functorArray);
  var map111 = /* @__PURE__ */ map(functorTuple);
  var Text2 = /* @__PURE__ */ function() {
    function Text3(value0) {
      this.value0 = value0;
    }
    ;
    Text3.create = function(value0) {
      return new Text3(value0);
    };
    return Text3;
  }();
  var Elem = /* @__PURE__ */ function() {
    function Elem2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Elem2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Elem2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Elem2;
  }();
  var Keyed = /* @__PURE__ */ function() {
    function Keyed2(value0, value1, value22, value32) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
      this.value3 = value32;
    }
    ;
    Keyed2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return function(value32) {
            return new Keyed2(value0, value1, value22, value32);
          };
        };
      };
    };
    return Keyed2;
  }();
  var Widget = /* @__PURE__ */ function() {
    function Widget2(value0) {
      this.value0 = value0;
    }
    ;
    Widget2.create = function(value0) {
      return new Widget2(value0);
    };
    return Widget2;
  }();
  var Grafted = /* @__PURE__ */ function() {
    function Grafted2(value0) {
      this.value0 = value0;
    }
    ;
    Grafted2.create = function(value0) {
      return new Grafted2(value0);
    };
    return Grafted2;
  }();
  var Graft = /* @__PURE__ */ function() {
    function Graft2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Graft2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Graft2(value0, value1, value22);
        };
      };
    };
    return Graft2;
  }();
  var unGraft = function(f) {
    return function($61) {
      return f($61);
    };
  };
  var graft = unsafeCoerce2;
  var bifunctorGraft = {
    bimap: function(f) {
      return function(g) {
        return unGraft(function(v) {
          return graft(new Graft(function($63) {
            return f(v.value0($63));
          }, function($64) {
            return g(v.value1($64));
          }, v.value2));
        });
      };
    }
  };
  var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
  var runGraft = /* @__PURE__ */ unGraft(function(v) {
    var go2 = function(v2) {
      if (v2 instanceof Text2) {
        return new Text2(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map27(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map27(map111(go2))(v2.value3));
      }
      ;
      if (v2 instanceof Widget) {
        return new Widget(v.value1(v2.value0));
      }
      ;
      if (v2 instanceof Grafted) {
        return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
    };
    return go2(v.value2);
  });

  // output/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a2, f) {
    var b2 = [];
    for (var i2 = 0; i2 < a2.length; i2++) {
      b2.push(f(i2, a2[i2]));
    }
    return b2;
  }
  function forEachE(a2, f) {
    for (var i2 = 0; i2 < a2.length; i2++) {
      f(a2[i2]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i2 = 0; i2 < ks.length; i2++) {
      var k = ks[i2];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i2 = 0;
    while (1) {
      if (i2 < l1) {
        if (i2 < l2) {
          a3.push(f1(i2, a1[i2], a2[i2]));
        } else {
          f2(i2, a1[i2]);
        }
      } else if (i2 < l2) {
        a3.push(f3(i2, a2[i2]));
      } else {
        break;
      }
      i2++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      o[k] = f(k, i2, a2);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i2 = 0; i2 < as.length; i2++) {
      var a2 = as[i2];
      var k = fk(a2);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i2, o1[k], a2);
      } else {
        o2[k] = f3(k, i2, a2);
      }
    }
    for (var k in o1) {
      if (k in o2) {
        continue;
      }
      f2(k, o1[k]);
    }
    return o2;
  }
  function refEq2(a2, b2) {
    return a2 === b2;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name15, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name15);
    } else {
      return doc.createElement(name15);
    }
  }
  function insertChildIx(i2, a2, b2) {
    var n = b2.childNodes.item(i2) || null;
    if (n !== a2) {
      b2.insertBefore(a2, n);
    }
  }
  function removeChild(a2, b2) {
    if (b2 && a2.parentNode === b2) {
      b2.removeChild(a2);
    }
  }
  function parentNode(a2) {
    return a2.parentNode;
  }
  function setAttribute(ns, attr3, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr3, val);
    } else {
      el.setAttribute(attr3, val);
    }
  }
  function removeAttribute(ns, attr3, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr3);
    } else {
      el.removeAttribute(attr3);
    }
  }
  function hasAttribute(ns, attr3, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr3);
    } else {
      return el.hasAttribute(attr3);
    }
  }
  function addEventListener2(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener2(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name15) {
    return function(doctype) {
      return doctype[name15];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.Element/index.js
  var toNode2 = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy6 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy6("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step2(state3.widget, vdom.value0);
        var res$prime = unStep(function(v) {
          return mkStep(new Step(v.value0, {
            build: state3.build,
            widget: res
          }, $lazy_patchWidget(296), haltWidget));
        })(res);
        return res$prime;
      }
      ;
      haltWidget(state3);
      return state3.build(vdom);
    };
  });
  var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
  var haltText = function(v) {
    var parent2 = parentNode(v.node);
    return removeChild(v.node, parent2);
  };
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy6("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text2) {
        if (state3.value === vdom.value0) {
          return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
        }
        ;
        if (otherwise) {
          var nextState = {
            build: state3.build,
            node: state3.node,
            value: vdom.value0
          };
          setTextContent(vdom.value0, state3.node);
          return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
        }
        ;
      }
      ;
      haltText(state3);
      return state3.build(vdom);
    };
  });
  var patchText = /* @__PURE__ */ $lazy_patchText(77);
  var haltKeyed = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forInE(v.children, function(v1, s) {
      return halt(s);
    });
    return halt(v.attrs);
  };
  var haltElem = function(v) {
    var parent2 = parentNode(v.node);
    removeChild(v.node, parent2);
    forEachE(v.children, halt);
    return halt(v.attrs);
  };
  var eqElemSpec = function(ns1, v, ns2, v1) {
    var $63 = v === v1;
    if ($63) {
      if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
        return true;
      }
      ;
      if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
        return true;
      }
      ;
      return false;
    }
    ;
    return false;
  };
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy6("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(ix, s, v2) {
          var res = step2(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
      }
      ;
      haltElem(state3);
      return state3.build(vdom);
    };
  });
  var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy6("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step2(state3.attrs, vdom.value2);
          var nextState = {
            build: state3.build,
            node: state3.node,
            attrs: attrs2,
            ns: vdom.value0,
            name: vdom.value1,
            children: state3.children,
            length: 0
          };
          return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
        }
        ;
        var onThis = function(v2, s) {
          return halt(s);
        };
        var onThese = function(v2, ix$prime, s, v3) {
          var res = step2(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step2(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: children2,
          length: v
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
      }
      ;
      haltKeyed(state3);
      return state3.build(vdom);
    };
  });
  var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
  var buildWidget = function(v, build, w) {
    var res = v.buildWidget(v)(w);
    var res$prime = unStep(function(v1) {
      return mkStep(new Step(v1.value0, {
        build,
        widget: res
      }, patchWidget, haltWidget));
    })(res);
    return res$prime;
  };
  var buildText = function(v, build, s) {
    var node = createTextNode(s, v.document);
    var state3 = {
      build,
      node,
      value: s
    };
    return mkStep(new Step(node, state3, patchText, haltText));
  };
  var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(v1, ix, v2) {
      var res = build(v2.value1);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = strMapWithIxE(ch1, fst, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2,
      length: length(ch1)
    };
    return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
  };
  var buildElem = function(v, build, ns1, name1, as1, ch1) {
    var el = createElement(toNullable(ns1), name1, v.document);
    var node = toNode2(el);
    var onChild = function(ix, child) {
      var res = build(child);
      insertChildIx(ix, extract2(res), node);
      return res;
    };
    var children2 = forE2(ch1, onChild);
    var attrs = v.buildAttributes(el)(as1);
    var state3 = {
      build,
      node,
      attrs,
      ns: ns1,
      name: name1,
      children: children2
    };
    return mkStep(new Step(node, state3, patchElem, haltElem));
  };
  var buildVDom = function(spec) {
    var $lazy_build = $runtime_lazy6("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text2) {
          return buildText(spec, $lazy_build(59), v.value0);
        }
        ;
        if (v instanceof Elem) {
          return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Keyed) {
          return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
        }
        ;
        if (v instanceof Widget) {
          return buildWidget(spec, $lazy_build(62), v.value0);
        }
        ;
        if (v instanceof Grafted) {
          return $lazy_build(63)(runGraft(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
      };
    });
    var build = $lazy_build(58);
    return build;
  };

  // output/Foreign/foreign.js
  function typeOf(value14) {
    return typeof value14;
  }
  function tagOf(value14) {
    return Object.prototype.toString.call(value14).slice(8, -1);
  }
  var isArray2 = Array.isArray || function(value14) {
    return Object.prototype.toString.call(value14) === "[object Array]";
  };

  // output/Foreign/index.js
  var TypeMismatch2 = /* @__PURE__ */ function() {
    function TypeMismatch3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeMismatch3.create = function(value0) {
      return function(value1) {
        return new TypeMismatch3(value0, value1);
      };
    };
    return TypeMismatch3;
  }();
  var unsafeToForeign = unsafeCoerce2;
  var unsafeFromForeign = unsafeCoerce2;
  var fail = function(dictMonad) {
    var $153 = throwError(monadThrowExceptT(dictMonad));
    return function($154) {
      return $153(singleton6($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure13 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value14) {
        if (tagOf(value14) === tag) {
          return pure13(unsafeFromForeign(value14));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch2(tag, tagOf(value14)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value14.constructor.name]);
      };
    };
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
  };

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy7 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var Created = /* @__PURE__ */ function() {
    function Created2(value0) {
      this.value0 = value0;
    }
    ;
    Created2.create = function(value0) {
      return new Created2(value0);
    };
    return Created2;
  }();
  var Removed = /* @__PURE__ */ function() {
    function Removed2(value0) {
      this.value0 = value0;
    }
    ;
    Removed2.create = function(value0) {
      return new Removed2(value0);
    };
    return Removed2;
  }();
  var Attribute = /* @__PURE__ */ function() {
    function Attribute2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    Attribute2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new Attribute2(value0, value1, value22);
        };
      };
    };
    return Attribute2;
  }();
  var Property2 = /* @__PURE__ */ function() {
    function Property3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Property3.create = function(value0) {
      return function(value1) {
        return new Property3(value0, value1);
      };
    };
    return Property3;
  }();
  var Handler = /* @__PURE__ */ function() {
    function Handler2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Handler2.create = function(value0) {
      return function(value1) {
        return new Handler2(value0, value1);
      };
    };
    return Handler2;
  }();
  var Ref = /* @__PURE__ */ function() {
    function Ref2(value0) {
      this.value0 = value0;
    }
    ;
    Ref2.create = function(value0) {
      return new Ref2(value0);
    };
    return Ref2;
  }();
  var unsafeGetProperty = unsafeGetAny;
  var setProperty = unsafeSetAny;
  var removeProperty = function(key, el) {
    var v = hasAttribute(nullImpl, key, el);
    if (v) {
      return removeAttribute(nullImpl, key, el);
    }
    ;
    var v1 = typeOf(unsafeGetAny(key, el));
    if (v1 === "string") {
      return unsafeSetAny(key, "", el);
    }
    ;
    if (key === "rowSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    if (key === "colSpan") {
      return unsafeSetAny(key, 1, el);
    }
    ;
    return unsafeSetAny(key, jsUndefined, el);
  };
  var propToStrKey = function(v) {
    if (v instanceof Attribute && v.value0 instanceof Just) {
      return "attr/" + (v.value0.value0 + (":" + v.value1));
    }
    ;
    if (v instanceof Attribute) {
      return "attr/:" + v.value1;
    }
    ;
    if (v instanceof Property2) {
      return "prop/" + v.value0;
    }
    ;
    if (v instanceof Handler) {
      return "handler/" + v.value0;
    }
    ;
    if (v instanceof Ref) {
      return "ref";
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
  };
  var propFromString = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property2) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener2(v1.value0, fst(handler3), el);
          }
          ;
          if (v1 instanceof Ref) {
            return unit;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
        };
      };
      var mbEmit = function(v) {
        if (v instanceof Just) {
          return emit(v.value0)();
        }
        ;
        return unit;
      };
      var haltProp = function(state3) {
        var v = lookup("ref")(state3.props);
        if (v instanceof Just && v.value0 instanceof Ref) {
          return mbEmit(v.value0.value0(new Removed(el)));
        }
        ;
        return unit;
      };
      var diffProp = function(prevEvents, events) {
        return function(v, v1, v11, v2) {
          if (v11 instanceof Attribute && v2 instanceof Attribute) {
            var $66 = v11.value2 === v2.value2;
            if ($66) {
              return v2;
            }
            ;
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v11 instanceof Property2 && v2 instanceof Property2) {
            var v4 = refEq2(v11.value1, v2.value1);
            if (v4) {
              return v2;
            }
            ;
            if (v2.value0 === "value") {
              var elVal = unsafeGetProperty("value", el);
              var $75 = refEq2(elVal, v2.value1);
              if ($75) {
                return v2;
              }
              ;
              setProperty(v2.value0, v2.value1, el);
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v11 instanceof Handler && v2 instanceof Handler) {
            var handler3 = unsafeLookup(v2.value0, prevEvents);
            write(v2.value1)(snd(handler3))();
            pokeMutMap(v2.value0, handler3, events);
            return v2;
          }
          ;
          return v2;
        };
      };
      var applyProp = function(events) {
        return function(v, v1, v2) {
          if (v2 instanceof Attribute) {
            setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
            return v2;
          }
          ;
          if (v2 instanceof Property2) {
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          if (v2 instanceof Handler) {
            var v3 = unsafeGetAny(v2.value0, events);
            if (unsafeHasAny(v2.value0, events)) {
              write(v2.value1)(snd(v3))();
              return v2;
            }
            ;
            var ref2 = $$new(v2.value1)();
            var listener = eventListener(function(ev) {
              return function __do2() {
                var f$prime = read(ref2)();
                return mbEmit(f$prime(ev));
              };
            })();
            pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
            addEventListener2(v2.value0, listener, el);
            return v2;
          }
          ;
          if (v2 instanceof Ref) {
            mbEmit(v2.value0(new Created(el)));
            return v2;
          }
          ;
          throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
        };
      };
      var $lazy_patchProp = $runtime_lazy7("patchProp", "Halogen.VDom.DOM.Prop", function() {
        return function(state3, ps2) {
          var events = newMutMap();
          var onThis = removeProp(state3.events);
          var onThese = diffProp(state3.events, events);
          var onThat = applyProp(events);
          var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
          var nextState = {
            events: unsafeFreeze2(events),
            props
          };
          return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
        };
      });
      var patchProp = $lazy_patchProp(87);
      var renderProp = function(ps1) {
        var events = newMutMap();
        var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
        var state3 = {
          events: unsafeFreeze2(events),
          props: ps1$prime
        };
        return mkStep(new Step(unit, state3, patchProp, haltProp));
      };
      return renderProp;
    };
  };

  // output/Halogen.HTML.Core/index.js
  var HTML = function(x) {
    return x;
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text6 = function($29) {
    return HTML(Text2.create($29));
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property2.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropButtonType = {
    toPropValue: function($50) {
      return propFromString(renderButtonType($50));
    }
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name15) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name15, props, children2);
        };
      };
    };
  };

  // output/Control.Applicative.Free/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
  var Pure = /* @__PURE__ */ function() {
    function Pure2(value0) {
      this.value0 = value0;
    }
    ;
    Pure2.create = function(value0) {
      return new Pure2(value0);
    };
    return Pure2;
  }();
  var Lift = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var Ap = /* @__PURE__ */ function() {
    function Ap2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Ap2.create = function(value0) {
      return function(value1) {
        return new Ap2(value0, value1);
      };
    };
    return Ap2;
  }();
  var mkAp = function(fba) {
    return function(fb) {
      return new Ap(fba, fb);
    };
  };
  var liftFreeAp = /* @__PURE__ */ function() {
    return Lift.create;
  }();
  var goLeft = function(dictApplicative) {
    var pure10 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure10(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Lift) {
                return new Tuple(new Cons({
                  func: nat(func.value0),
                  count
                }, fStack), valStack);
              }
              ;
              if (func instanceof Ap) {
                return goLeft(dictApplicative)(fStack)(cons2(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
            };
          };
        };
      };
    };
  };
  var goApply = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    return function(fStack) {
      return function(vals) {
        return function(gVal) {
          if (fStack instanceof Nil) {
            return new Left(gVal);
          }
          ;
          if (fStack instanceof Cons) {
            var gRes = apply2(fStack.value0.func)(gVal);
            var $31 = fStack.value0.count === 1;
            if ($31) {
              if (fStack.value1 instanceof Nil) {
                return new Left(gRes);
              }
              ;
              return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
            }
            ;
            if (vals instanceof Nil) {
              return new Left(gRes);
            }
            ;
            if (vals instanceof Cons) {
              return new Right(new Tuple(new Cons({
                func: gRes,
                count: fStack.value0.count - 1 | 0
              }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
        };
      };
    };
  };
  var functorFreeAp = {
    map: function(f) {
      return function(x) {
        return mkAp(new Pure(f))(x);
      };
    }
  };
  var foldFreeAp = function(dictApplicative) {
    var goApply1 = goApply(dictApplicative);
    var pure10 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure10(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Lift) {
              var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
              if (v1 instanceof Left) {
                $tco_done = true;
                return v1.value0;
              }
              ;
              if (v1 instanceof Right) {
                $copy_v = v1.value0;
                return;
              }
              ;
              throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
            }
            ;
            if (v.value1.value0 instanceof Ap) {
              var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
              $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
          }
          ;
          while (!$tco_done) {
            $tco_result = $tco_loop($copy_v);
          }
          ;
          return $tco_result;
        };
        return go2(new Tuple(Nil.value, singleton6(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity6);
  };
  var applyFreeAp = {
    apply: function(fba) {
      return function(fb) {
        return mkAp(fba)(fb);
      };
    },
    Functor0: function() {
      return functorFreeAp;
    }
  };
  var applicativeFreeAp = /* @__PURE__ */ function() {
    return {
      pure: Pure.create,
      Apply0: function() {
        return applyFreeAp;
      }
    };
  }();
  var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
  var hoistFreeAp = function(f) {
    return foldFreeAp1(function($54) {
      return liftFreeAp(f($54));
    });
  };

  // output/Data.CatQueue/index.js
  var CatQueue = /* @__PURE__ */ function() {
    function CatQueue2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatQueue2.create = function(value0) {
      return function(value1) {
        return new CatQueue2(value0, value1);
      };
    };
    return CatQueue2;
  }();
  var uncons5 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse3(v.value1), Nil.value);
        return;
      }
      ;
      if (v.value0 instanceof Cons) {
        $tco_done = true;
        return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
      }
      ;
      throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var snoc3 = function(v) {
    return function(a2) {
      return new CatQueue(v.value0, new Cons(a2, v.value1));
    };
  };
  var $$null3 = function(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var empty6 = /* @__PURE__ */ function() {
    return new CatQueue(Nil.value, Nil.value);
  }();

  // output/Data.CatList/index.js
  var CatNil = /* @__PURE__ */ function() {
    function CatNil2() {
    }
    ;
    CatNil2.value = new CatNil2();
    return CatNil2;
  }();
  var CatCons = /* @__PURE__ */ function() {
    function CatCons2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    CatCons2.create = function(value0) {
      return function(value1) {
        return new CatCons2(value0, value1);
      };
    };
    return CatCons2;
  }();
  var link = function(v) {
    return function(v1) {
      if (v instanceof CatNil) {
        return v1;
      }
      ;
      if (v1 instanceof CatNil) {
        return v;
      }
      ;
      if (v instanceof CatCons) {
        return new CatCons(v.value0, snoc3(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr5 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl6 = function($copy_v) {
          return function($copy_v1) {
            return function($copy_v2) {
              var $tco_var_v = $copy_v;
              var $tco_var_v1 = $copy_v1;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(v, v1, v2) {
                if (v2 instanceof Nil) {
                  $tco_done = true;
                  return v1;
                }
                ;
                if (v2 instanceof Cons) {
                  $tco_var_v = v;
                  $tco_var_v1 = v(v1)(v2.value0);
                  $copy_v2 = v2.value1;
                  return;
                }
                ;
                throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
              }
              ;
              while (!$tco_done) {
                $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
              }
              ;
              return $tco_result;
            };
          };
        };
        var go2 = function($copy_xs) {
          return function($copy_ys) {
            var $tco_var_xs = $copy_xs;
            var $tco_done1 = false;
            var $tco_result;
            function $tco_loop(xs, ys) {
              var v = uncons5(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl6(function(x) {
                  return function(i2) {
                    return i2(x);
                  };
                })(b2)(ys);
              }
              ;
              if (v instanceof Just) {
                $tco_var_xs = v.value0.value1;
                $copy_ys = new Cons(k(v.value0.value0), ys);
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
            }
            ;
            while (!$tco_done1) {
              $tco_result = $tco_loop($tco_var_xs, $copy_ys);
            }
            ;
            return $tco_result;
          };
        };
        return go2(q2)(Nil.value);
      };
    };
  };
  var uncons6 = function(v) {
    if (v instanceof CatNil) {
      return Nothing.value;
    }
    ;
    if (v instanceof CatCons) {
      return new Just(new Tuple(v.value0, function() {
        var $66 = $$null3(v.value1);
        if ($66) {
          return CatNil.value;
        }
        ;
        return foldr5(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty7 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append8 = link;
  var semigroupCatList = {
    append: append8
  };
  var snoc4 = function(cat) {
    return function(a2) {
      return append8(cat)(new CatCons(a2, empty6));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy8 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append9 = /* @__PURE__ */ append(semigroupCatList);
  var Free = /* @__PURE__ */ function() {
    function Free2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Free2.create = function(value0) {
      return function(value1) {
        return new Free2(value0, value1);
      };
    };
    return Free2;
  }();
  var Return = /* @__PURE__ */ function() {
    function Return2(value0) {
      this.value0 = value0;
    }
    ;
    Return2.create = function(value0) {
      return new Return2(value0);
    };
    return Return2;
  }();
  var Bind = /* @__PURE__ */ function() {
    function Bind2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Bind2.create = function(value0) {
      return function(value1) {
        return new Bind2(value0, value1);
      };
    };
    return Bind2;
  }();
  var toView = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      var runExpF = function(v22) {
        return v22;
      };
      var concatF = function(v22) {
        return function(r) {
          return new Free(v22.value0, append9(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons6(v.value1);
        if (v2 instanceof Nothing) {
          $tco_done = true;
          return new Return(v.value0.value0);
        }
        ;
        if (v2 instanceof Just) {
          $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
          return;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
      }
      ;
      if (v.value0 instanceof Bind) {
        $tco_done = true;
        return new Bind(v.value0.value0, function(a2) {
          return concatF(v.value0.value1(a2))(v.value1);
        });
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
    }
    ;
    while (!$tco_done) {
      $tco_result = $tco_loop($copy_v);
    }
    ;
    return $tco_result;
  };
  var fromView = function(f) {
    return new Free(f, empty7);
  };
  var freeMonad = {
    Applicative0: function() {
      return freeApplicative;
    },
    Bind1: function() {
      return freeBind;
    }
  };
  var freeFunctor = {
    map: function(k) {
      return function(f) {
        return bindFlipped(freeBind)(function() {
          var $189 = pure(freeApplicative);
          return function($190) {
            return $189(k($190));
          };
        }())(f);
      };
    }
  };
  var freeBind = {
    bind: function(v) {
      return function(k) {
        return new Free(v.value0, snoc4(v.value1)(k));
      };
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var freeApplicative = {
    pure: function($191) {
      return fromView(Return.create($191));
    },
    Apply0: function() {
      return $lazy_freeApply(0);
    }
  };
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy8("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure5 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure5($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map113 = map(Monad0.Bind1().Apply0().Functor0());
    var pure13 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map113(Done.create)(pure13(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map113(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM4(go2);
    };
  };

  // output/Halogen.Query.ChildQuery/index.js
  var unChildQueryBox = unsafeCoerce2;

  // output/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a2) {
    return function(b2) {
      return a2 === b2;
    };
  }

  // output/Unsafe.Reference/index.js
  var unsafeRefEq = reallyUnsafeRefEq;

  // output/Halogen.Subscription/index.js
  var $$void5 = /* @__PURE__ */ $$void(functorEffect);
  var bind5 = /* @__PURE__ */ bind(bindEffect);
  var append10 = /* @__PURE__ */ append(semigroupArray);
  var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
  var unsubscribe = function(v) {
    return v;
  };
  var subscribe = function(v) {
    return function(k) {
      return v(function($76) {
        return $$void5(k($76));
      });
    };
  };
  var notify = function(v) {
    return function(a2) {
      return v(a2);
    };
  };
  var create3 = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_(function(v) {
            return append10(v)([k]);
          })(subscribers)();
          return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind5(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var SubscriptionId = function(x) {
    return x;
  };
  var ForkId = function(x) {
    return x;
  };
  var State = /* @__PURE__ */ function() {
    function State2(value0) {
      this.value0 = value0;
    }
    ;
    State2.create = function(value0) {
      return new State2(value0);
    };
    return State2;
  }();
  var Subscribe = /* @__PURE__ */ function() {
    function Subscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Subscribe2.create = function(value0) {
      return function(value1) {
        return new Subscribe2(value0, value1);
      };
    };
    return Subscribe2;
  }();
  var Unsubscribe = /* @__PURE__ */ function() {
    function Unsubscribe2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Unsubscribe2.create = function(value0) {
      return function(value1) {
        return new Unsubscribe2(value0, value1);
      };
    };
    return Unsubscribe2;
  }();
  var Lift2 = /* @__PURE__ */ function() {
    function Lift3(value0) {
      this.value0 = value0;
    }
    ;
    Lift3.create = function(value0) {
      return new Lift3(value0);
    };
    return Lift3;
  }();
  var ChildQuery2 = /* @__PURE__ */ function() {
    function ChildQuery3(value0) {
      this.value0 = value0;
    }
    ;
    ChildQuery3.create = function(value0) {
      return new ChildQuery3(value0);
    };
    return ChildQuery3;
  }();
  var Raise = /* @__PURE__ */ function() {
    function Raise2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Raise2.create = function(value0) {
      return function(value1) {
        return new Raise2(value0, value1);
      };
    };
    return Raise2;
  }();
  var Par = /* @__PURE__ */ function() {
    function Par2(value0) {
      this.value0 = value0;
    }
    ;
    Par2.create = function(value0) {
      return new Par2(value0);
    };
    return Par2;
  }();
  var Fork = /* @__PURE__ */ function() {
    function Fork2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Fork2.create = function(value0) {
      return function(value1) {
        return new Fork2(value0, value1);
      };
    };
    return Fork2;
  }();
  var Join = /* @__PURE__ */ function() {
    function Join2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Join2.create = function(value0) {
      return function(value1) {
        return new Join2(value0, value1);
      };
    };
    return Join2;
  }();
  var Kill = /* @__PURE__ */ function() {
    function Kill2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Kill2.create = function(value0) {
      return function(value1) {
        return new Kill2(value0, value1);
      };
    };
    return Kill2;
  }();
  var GetRef = /* @__PURE__ */ function() {
    function GetRef2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    GetRef2.create = function(value0) {
      return function(value1) {
        return new GetRef2(value0, value1);
      };
    };
    return GetRef2;
  }();
  var HalogenM = function(x) {
    return x;
  };
  var ordSubscriptionId = ordInt;
  var ordForkId = ordInt;
  var monadHalogenM = freeMonad;
  var monadStateHalogenM = {
    state: function($181) {
      return HalogenM(liftF(State.create($181)));
    },
    Monad0: function() {
      return monadHalogenM;
    }
  };
  var functorHalogenM = freeFunctor;
  var applicativeHalogenM = freeApplicative;

  // output/Halogen.Query.HalogenQ/index.js
  var Initialize = /* @__PURE__ */ function() {
    function Initialize2(value0) {
      this.value0 = value0;
    }
    ;
    Initialize2.create = function(value0) {
      return new Initialize2(value0);
    };
    return Initialize2;
  }();
  var Finalize = /* @__PURE__ */ function() {
    function Finalize2(value0) {
      this.value0 = value0;
    }
    ;
    Finalize2.create = function(value0) {
      return new Finalize2(value0);
    };
    return Finalize2;
  }();
  var Receive = /* @__PURE__ */ function() {
    function Receive2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Receive2.create = function(value0) {
      return function(value1) {
        return new Receive2(value0, value1);
      };
    };
    return Receive2;
  }();
  var Action2 = /* @__PURE__ */ function() {
    function Action3(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Action3.create = function(value0) {
      return function(value1) {
        return new Action3(value0, value1);
      };
    };
    return Action3;
  }();
  var Query = /* @__PURE__ */ function() {
    function Query2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Query2.create = function(value0) {
      return function(value1) {
        return new Query2(value0, value1);
      };
    };
    return Query2;
  }();

  // output/Halogen.VDom.Thunk/index.js
  var $runtime_lazy9 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var unsafeEqThunk = function(v, v1) {
    return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
  };
  var runThunk = function(v) {
    return v.value2(v.value3);
  };
  var buildThunk = function(toVDom) {
    var haltThunk = function(state3) {
      return halt(state3.vdom);
    };
    var $lazy_patchThunk = $runtime_lazy9("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step2(state3.vdom, toVDom(runThunk(t2)));
        return mkStep(new Step(extract2(vdom), {
          vdom,
          thunk: t2
        }, $lazy_patchThunk(115), haltThunk));
      };
    });
    var patchThunk = $lazy_patchThunk(108);
    var renderThunk = function(spec) {
      return function(t) {
        var vdom = buildVDom(spec)(toVDom(runThunk(t)));
        return mkStep(new Step(extract2(vdom), {
          thunk: t,
          vdom
        }, patchThunk, haltThunk));
      };
    };
    return renderThunk;
  };

  // output/Halogen.Component/index.js
  var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
  var map28 = /* @__PURE__ */ map(functorHalogenM);
  var pure6 = /* @__PURE__ */ pure(applicativeHalogenM);
  var ComponentSlot = /* @__PURE__ */ function() {
    function ComponentSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ComponentSlot2.create = function(value0) {
      return new ComponentSlot2(value0);
    };
    return ComponentSlot2;
  }();
  var ThunkSlot = /* @__PURE__ */ function() {
    function ThunkSlot2(value0) {
      this.value0 = value0;
    }
    ;
    ThunkSlot2.create = function(value0) {
      return new ThunkSlot2(value0);
    };
    return ThunkSlot2;
  }();
  var unComponentSlot = unsafeCoerce2;
  var unComponent = unsafeCoerce2;
  var mkEval = function(args) {
    return function(v) {
      if (v instanceof Initialize) {
        return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
      }
      ;
      if (v instanceof Finalize) {
        return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
      }
      ;
      if (v instanceof Receive) {
        return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
      }
      ;
      if (v instanceof Action2) {
        return voidLeft2(args.handleAction(v.value0))(v.value1);
      }
      ;
      if (v instanceof Query) {
        return unCoyoneda(function(g) {
          var $45 = map28(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure6(unit)),
      handleQuery: $$const(pure6(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var form = /* @__PURE__ */ element2("form");
  var form_ = /* @__PURE__ */ form([]);
  var input2 = function(props) {
    return element2("input")(props)([]);
  };
  var label4 = /* @__PURE__ */ element2("label");
  var label_ = /* @__PURE__ */ label4([]);
  var pre = /* @__PURE__ */ element2("pre");
  var pre_ = /* @__PURE__ */ pre([]);
  var samp = /* @__PURE__ */ element2("samp");
  var samp_ = /* @__PURE__ */ samp([]);
  var div3 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div3([]);
  var button = /* @__PURE__ */ element2("button");

  // output/Halogen.HTML.Properties/index.js
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var type_17 = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value12 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };

  // output/Halogen.HTML/index.js
  var fromPlainHTML = unsafeCoerce2;

  // output/Control.Monad.Except/index.js
  var unwrap5 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap5(runExceptT($3));
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value14) {
    return value14 == null ? f : s(value14[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail2 = fail(dictMonad);
    var pure10 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value14) {
        return unsafeReadPropImpl(fail2(new TypeMismatch2("object", typeOf(value14))), pure10, k, value14);
      };
    };
  };
  var readProp = function(dictMonad) {
    return unsafeReadProp(dictMonad);
  };

  // output/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }

  // output/Web.Event.Event/index.js
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click2 = "click";

  // output/Halogen.HTML.Events/index.js
  var map30 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped4 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map30(Action.create)(f(ev));
      });
    };
  };
  var handler2 = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return new Just(new Action(f(ev)));
      });
    };
  };
  var onClick = /* @__PURE__ */ function() {
    var $15 = handler2(click2);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop3) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped4(reader)(readProp2(prop3))(unsafeToForeign(a2));
          };
          return handler$prime(key)(composeKleisli2(currentTarget)(function(e) {
            return either($$const(Nothing.value))(function($85) {
              return Just.create(f($85));
            })(runExcept(go2(e)));
          }));
        };
      };
    };
  };
  var onValueInput = /* @__PURE__ */ addForeignPropHandler(input)("value")(readString2);

  // output/Control.Monad.Fork.Class/index.js
  var monadForkAff = {
    suspend: suspendAff,
    fork: forkAff,
    join: joinFiber,
    Monad0: function() {
      return monadAff;
    },
    Functor1: function() {
      return functorFiber;
    }
  };
  var fork = function(dict) {
    return dict.fork;
  };

  // output/Effect.Console/foreign.js
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output/Halogen.Aff.Driver.State/index.js
  var unRenderStateX = unsafeCoerce2;
  var unDriverStateX = unsafeCoerce2;
  var renderStateX_ = function(dictApplicative) {
    var traverse_7 = traverse_(dictApplicative)(foldableMaybe);
    return function(f) {
      return unDriverStateX(function(st) {
        return traverse_7(f)(st.rendering);
      });
    };
  };
  var mkRenderStateX = unsafeCoerce2;
  var renderStateX = function(dictFunctor) {
    return function(f) {
      return unDriverStateX(function(st) {
        return mkRenderStateX(f(st.rendering));
      });
    };
  };
  var mkDriverStateXRef = unsafeCoerce2;
  var mapDriverState = function(f) {
    return function(v) {
      return f(v);
    };
  };
  var initDriverState = function(component2) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty5)();
            var childrenOut = $$new(empty5)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty3))();
            var forks = $$new(empty3)();
            var ds = {
              component: component2,
              state: component2.initialState(input3),
              refs: empty3,
              children: empty5,
              childrenIn,
              childrenOut,
              selfRef,
              handlerRef,
              pendingQueries,
              pendingOuts,
              pendingHandlers,
              rendering: Nothing.value,
              fresh: fresh2,
              subscriptions,
              forks,
              lifecycleHandlers: lchs
            };
            write(ds)(selfRef)();
            return mkDriverStateXRef(selfRef);
          };
        };
      };
    };
  };

  // output/Halogen.Aff.Driver.Eval/index.js
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var lookup4 = /* @__PURE__ */ lookup2(ordSubscriptionId);
  var bind12 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard3(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var pure7 = /* @__PURE__ */ pure(applicativeAff);
  var map31 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel3 = /* @__PURE__ */ parallel(parallelAff);
  var map112 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map210 = /* @__PURE__ */ map(functorMaybe);
  var insert6 = /* @__PURE__ */ insert3(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete4 = /* @__PURE__ */ $$delete2(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert1 = /* @__PURE__ */ insert3(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup1 = /* @__PURE__ */ lookup2(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup2(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do2() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped6(lookup4(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard1(liftEffect4(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind12(liftEffect4(f))(function(result) {
          return bind12(liftEffect4(read(lchs)))(function(v) {
            return discard1(traverse_22(fork3)(v.finalizers))(function() {
              return discard1(parSequence_2(v.initializers))(function() {
                return pure7(result);
              });
            });
          });
        });
      });
    };
  };
  var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
  var fresh = function(f) {
    return function(ref2) {
      return bind12(liftEffect4(read(ref2)))(function(v) {
        return liftEffect4(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render6) {
    return function(ref2) {
      return function(q2) {
        return bind12(liftEffect4(read(ref2)))(function(v) {
          return evalM(render6)(ref2)(v["component"]["eval"](new Query(map31(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render6) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind12(liftEffect4(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel3(bind12(liftEffect4(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render6)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map112(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure7(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect4(write({
                    component: v2.component,
                    state: v3.value1,
                    refs: v2.refs,
                    children: v2.children,
                    childrenIn: v2.childrenIn,
                    childrenOut: v2.childrenOut,
                    selfRef: v2.selfRef,
                    handlerRef: v2.handlerRef,
                    pendingQueries: v2.pendingQueries,
                    pendingOuts: v2.pendingOuts,
                    pendingHandlers: v2.pendingHandlers,
                    rendering: v2.rendering,
                    fresh: v2.fresh,
                    subscriptions: v2.subscriptions,
                    forks: v2.forks,
                    lifecycleHandlers: v2.lifecycleHandlers
                  })(ref2)))(function() {
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render6(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure7(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind12(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind12(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render6)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind12(liftEffect4(read(ref2)))(function(v2) {
                    return discard1(liftEffect4(modify_(map210(insert6(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure7(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure7(v1.value1);
              });
            }
            ;
            if (v1 instanceof Lift2) {
              return v1.value0;
            }
            ;
            if (v1 instanceof ChildQuery2) {
              return evalChildQuery(ref2)(v1.value0);
            }
            ;
            if (v1 instanceof Raise) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                  return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $119 = evalM(render6)(ref2);
                return function($120) {
                  return parallel3($119($120));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind12(fresh(ForkId)(ref2))(function(fid) {
                return bind12(liftEffect4(read(ref2)))(function(v2) {
                  return bind12(liftEffect4($$new(false)))(function(doneRef) {
                    return bind12(fork3($$finally(liftEffect4(function __do2() {
                      modify_($$delete4(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render6)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect4(unlessM2(read(doneRef))(modify_(insert1(fid)(fiber))(v2.forks))))(function() {
                        return pure7(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(joinFiber)(lookup1(v1.value0)(forkMap)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return bind12(liftEffect4(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup1(v1.value0)(forkMap)))(function() {
                    return pure7(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind12(liftEffect4(read(ref2)))(function(v2) {
                return pure7(v1.value1(lookup22(v1.value0)(v2.refs)));
              });
            }
            ;
            throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
          };
        };
        return foldFree2(go2(initRef))(v);
      };
    };
  };
  var evalF = function(render6) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect4(flip(modify_)(ref2)(mapDriverState(function(st) {
            return {
              component: st.component,
              state: st.state,
              refs: alter2($$const(v.value1))(v.value0)(st.refs),
              children: st.children,
              childrenIn: st.childrenIn,
              childrenOut: st.childrenOut,
              selfRef: st.selfRef,
              handlerRef: st.handlerRef,
              pendingQueries: st.pendingQueries,
              pendingOuts: st.pendingOuts,
              pendingHandlers: st.pendingHandlers,
              rendering: st.rendering,
              fresh: st.fresh,
              subscriptions: st.subscriptions,
              forks: st.forks,
              lifecycleHandlers: st.lifecycleHandlers
            };
          })));
        }
        ;
        if (v instanceof Action) {
          return bind12(liftEffect4(read(ref2)))(function(v1) {
            return evalM(render6)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind6 = /* @__PURE__ */ bind(bindEffect);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork(monadForkAff);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard22 = /* @__PURE__ */ discard4(bindAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure8 = /* @__PURE__ */ pure(applicativeEffect);
  var map33 = /* @__PURE__ */ map(functorEffect);
  var pure12 = /* @__PURE__ */ pure(applicativeAff);
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void6 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
  var newLifecycleHandlers = /* @__PURE__ */ function() {
    return $$new({
      initializers: Nil.value,
      finalizers: Nil.value
    });
  }();
  var handlePending = function(ref2) {
    return function __do2() {
      var queue = read(ref2)();
      write(Nothing.value)(ref2)();
      return for_2(queue)(function() {
        var $59 = traverse_5(fork4);
        return function($60) {
          return handleAff($59(reverse3($60)));
        };
      }())();
    };
  };
  var cleanupSubscriptionsAndForks = function(v) {
    return function __do2() {
      bindFlipped7(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
      write(Nothing.value)(v.subscriptions)();
      bindFlipped7(traverse_33(function() {
        var $61 = killFiber(error("finalized"));
        return function($62) {
          return handleAff($61($62));
        };
      }()))(read(v.forks))();
      return write(empty3)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component2) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render6)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse3(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect5(function __do2() {
                        handlePending(st.pendingQueries)();
                        return handlePending(st.pendingOuts)();
                      });
                    });
                  }), preInits),
                  finalizers: handlers.finalizers
                };
              })(lchs);
            });
          };
        };
        var runComponent = function(lchs) {
          return function(handler3) {
            return function(j) {
              return unComponent(function(c) {
                return function __do2() {
                  var lchs$prime = newLifecycleHandlers();
                  var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                  var pre2 = read(lchs)();
                  write({
                    initializers: Nil.value,
                    finalizers: pre2.finalizers
                  })(lchs)();
                  bindFlipped7(unDriverStateX(function() {
                    var $63 = render6(lchs);
                    return function($64) {
                      return $63(function(v) {
                        return v.selfRef;
                      }($64));
                    };
                  }()))(read($$var2))();
                  bindFlipped7(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                  return $$var2;
                };
              });
            };
          };
        };
        var renderChild = function(lchs) {
          return function(handler3) {
            return function(childrenInRef) {
              return function(childrenOutRef) {
                return unComponentSlot(function(slot) {
                  return function __do2() {
                    var childrenIn = map33(slot.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $65 = maybe(pure12(unit))(handler3);
                              return function($66) {
                                return $65(slot.output($66));
                              };
                            }())();
                            return handleAff(evalM(render6)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $67 = maybe(pure12(unit))(handler3);
                          return function($68) {
                            return $67(slot.output($68));
                          };
                        }())(slot.input)(slot.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map33(function($69) {
                      return isJust(slot.get($69));
                    })(read(childrenOutRef))();
                    when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_(slot.set($$var2))(childrenOutRef)();
                    return bind6(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure8(renderSpec2.renderChild(v.value0));
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                    }))();
                  };
                });
              };
            };
          };
        };
        var render6 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map33(isNothing)(read(v.pendingHandlers))();
              when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty5)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $70 = queueOrRun(v.pendingHandlers);
                var $71 = evalF(render6)(v.selfRef);
                return function($72) {
                  return $70($$void6($71($72)));
                };
              }();
              var childHandler = function() {
                var $73 = queueOrRun(v.pendingQueries);
                return function($74) {
                  return $73(handler3(Action.create($74)));
                };
              }();
              var rendering = renderSpec2.render(function($75) {
                return handleAff(handler3($75));
              })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
              var children2 = read(v.childrenOut)();
              var childrenIn = read(v.childrenIn)();
              foreachSlot2(childrenIn)(function(v1) {
                return function __do3() {
                  var childDS = read(v1)();
                  renderStateX_2(renderSpec2.removeChild)(childDS)();
                  return finalize(lchs)(childDS)();
                };
              })();
              flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
                return {
                  component: ds$prime.component,
                  state: ds$prime.state,
                  refs: ds$prime.refs,
                  children: children2,
                  childrenIn: ds$prime.childrenIn,
                  childrenOut: ds$prime.childrenOut,
                  selfRef: ds$prime.selfRef,
                  handlerRef: ds$prime.handlerRef,
                  pendingQueries: ds$prime.pendingQueries,
                  pendingOuts: ds$prime.pendingOuts,
                  pendingHandlers: ds$prime.pendingHandlers,
                  rendering: new Just(rendering),
                  fresh: ds$prime.fresh,
                  subscriptions: ds$prime.subscriptions,
                  forks: ds$prime.forks,
                  lifecycleHandlers: ds$prime.lifecycleHandlers
                };
              }))();
              return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
                return function __do3() {
                  var handlers = read(v.pendingHandlers)();
                  write(new Just(Nil.value))(v.pendingHandlers)();
                  traverse_23(function() {
                    var $76 = traverse_5(fork4);
                    return function($77) {
                      return handleAff($76(reverse3($77)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $52 = maybe(false)($$null)(mmore);
                  if ($52) {
                    return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                  }
                  ;
                  return new Loop(unit);
                };
              }))();
            };
          };
        };
        var finalize = function(lchs) {
          return unDriverStateX(function(st) {
            return function __do2() {
              cleanupSubscriptionsAndForks(st)();
              var f = evalM(render6)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_(function(handlers) {
                return {
                  initializers: handlers.initializers,
                  finalizers: new Cons(f, handlers.finalizers)
                };
              })(lchs)();
              return foreachSlot2(st.children)(function(v) {
                return function __do3() {
                  var dsx = read(v)();
                  return finalize(lchs)(dsx)();
                };
              })();
            };
          });
        };
        var evalDriver = function(disposed) {
          return function(ref2) {
            return function(q2) {
              return bind13(liftEffect5(read(disposed)))(function(v) {
                if (v) {
                  return pure12(Nothing.value);
                }
                ;
                return evalQ(render6)(ref2)(q2);
              });
            };
          };
        };
        var dispose = function(disposed) {
          return function(lchs) {
            return function(dsx) {
              return handleLifecycle(lchs)(function __do2() {
                var v = read(disposed)();
                if (v) {
                  return unit;
                }
                ;
                write(true)(disposed)();
                finalize(lchs)(dsx)();
                return unDriverStateX(function(v1) {
                  return function __do3() {
                    var v2 = liftEffect1(read(v1.selfRef))();
                    return for_2(v2.rendering)(renderSpec2.dispose)();
                  };
                })(dsx)();
              });
            };
          };
        };
        return bind13(liftEffect5(newLifecycleHandlers))(function(lchs) {
          return bind13(liftEffect5($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create3();
              var dsx = bindFlipped7(read)(runComponent(lchs)(function() {
                var $78 = notify(sio.listener);
                return function($79) {
                  return liftEffect5($78($79));
                };
              }())(i2)(component2))();
              return unDriverStateX(function(st) {
                return pure8({
                  query: evalDriver(disposed)(st.selfRef),
                  messages: sio.emitter,
                  dispose: dispose(disposed)(lchs)(dsx)
                });
              })(dsx)();
            });
          });
        });
      };
    };
  };

  // output/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name15) {
    return function(node) {
      return function() {
        return node[name15];
      };
    };
  };
  var baseURI = getEffProp2("baseURI");
  var _ownerDocument = getEffProp2("ownerDocument");
  var _parentNode = getEffProp2("parentNode");
  var _parentElement = getEffProp2("parentElement");
  var childNodes = getEffProp2("childNodes");
  var _firstChild = getEffProp2("firstChild");
  var _lastChild = getEffProp2("lastChild");
  var _previousSibling = getEffProp2("previousSibling");
  var _nextSibling = getEffProp2("nextSibling");
  var _nodeValue = getEffProp2("nodeValue");
  var textContent = getEffProp2("textContent");
  function insertBefore(node1) {
    return function(node2) {
      return function(parent2) {
        return function() {
          parent2.insertBefore(node1, node2);
        };
      };
    };
  }
  function appendChild(node) {
    return function(parent2) {
      return function() {
        parent2.appendChild(node);
      };
    };
  }
  function removeChild2(node) {
    return function(parent2) {
      return function() {
        parent2.removeChild(node);
      };
    };
  }

  // output/Web.DOM.Node/index.js
  var map34 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map34(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map34(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy10 = function(name15, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var pure9 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap6 = /* @__PURE__ */ unwrap();
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var bind14 = /* @__PURE__ */ bind(bindAff);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map35 = /* @__PURE__ */ map(functorEffect);
  var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindEffect);
  var substInParent = function(v) {
    return function(v1) {
      return function(v2) {
        if (v1 instanceof Just && v2 instanceof Just) {
          return $$void7(insertBefore(v)(v1.value0)(v2.value0));
        }
        ;
        if (v1 instanceof Nothing && v2 instanceof Just) {
          return $$void7(appendChild(v)(v2.value0));
        }
        ;
        return pure9(unit);
      };
    };
  };
  var removeChild3 = function(v) {
    return function __do2() {
      var npn = parentNode2(v.node)();
      return traverse_6(function(pn) {
        return removeChild2(v.node)(pn);
      })(npn)();
    };
  };
  var mkSpec = function(handler3) {
    return function(renderChildRef) {
      return function(document5) {
        var getNode = unRenderStateX(function(v) {
          return v.node;
        });
        var done = function(st) {
          if (st instanceof Just) {
            return halt(st.value0);
          }
          ;
          return unit;
        };
        var buildWidget2 = function(spec) {
          var buildThunk2 = buildThunk(unwrap6)(spec);
          var $lazy_patch = $runtime_lazy10("patch", "Halogen.VDom.Driver", function() {
            return function(st, slot) {
              if (st instanceof Just) {
                if (slot instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot.value0);
                }
                ;
                if (slot instanceof ThunkSlot) {
                  var step$prime = step2(st.value0, slot.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot);
            };
          });
          var $lazy_render = $runtime_lazy10("render", "Halogen.VDom.Driver", function() {
            return function(slot) {
              if (slot instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step3 = buildThunk2(slot.value0);
                return mkStep(new Step(extract2(step3), new Just(step3), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
            };
          });
          var $lazy_renderComponentSlot = $runtime_lazy10("renderComponentSlot", "Halogen.VDom.Driver", function() {
            return function(cs) {
              var renderChild = read(renderChildRef)();
              var rsx = renderChild(cs)();
              var node = getNode(rsx);
              return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
            };
          });
          var patch = $lazy_patch(91);
          var render6 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render6;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document5
        };
      };
    };
  };
  var renderSpec = function(document5) {
    return function(container) {
      var render6 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document5);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void7(appendChild(node)(toNode(container)))();
                  return {
                    machine,
                    node,
                    renderChildRef
                  };
                };
              }
              ;
              if (v1 instanceof Just) {
                return function __do2() {
                  write(child)(v1.value0.renderChildRef)();
                  var parent2 = parentNode2(v1.value0.node)();
                  var nextSib = nextSibling(v1.value0.node)();
                  var machine$prime = step2(v1.value0.machine, v);
                  var newNode = extract2(machine$prime);
                  when3(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                  return {
                    machine: machine$prime,
                    node: newNode,
                    renderChildRef: v1.value0.renderChildRef
                  };
                };
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
            };
          };
        };
      };
      return {
        render: render6,
        renderChild: identity7,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component2) {
    return function(i2) {
      return function(element3) {
        return bind14(liftEffect6(map35(toDocument)(bindFlipped8(document4)(windowImpl))))(function(document5) {
          return runUI(renderSpec(document5)(element3))(component2)(i2);
        });
      };
    };
  };

  // output/Sandbox.Main/index.js
  var show7 = /* @__PURE__ */ show(showInt);
  var value13 = /* @__PURE__ */ value12(isPropString);
  var type_19 = /* @__PURE__ */ type_17(isPropButtonType);
  var show13 = /* @__PURE__ */ show(showJsonValue);
  var modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
  var runProgram2 = /* @__PURE__ */ runProgram(/* @__PURE__ */ documentSet(documentViolation))(/* @__PURE__ */ encodeSet2(ordViolation)(encodeJsonViolation));
  var RunProgram = /* @__PURE__ */ function() {
    function RunProgram2() {
    }
    ;
    RunProgram2.value = new RunProgram2();
    return RunProgram2;
  }();
  var UpdateJson = /* @__PURE__ */ function() {
    function UpdateJson2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateJson2.create = function(value0) {
      return new UpdateJson2(value0);
    };
    return UpdateJson2;
  }();
  var UpdateSchema = /* @__PURE__ */ function() {
    function UpdateSchema2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateSchema2.create = function(value0) {
      return new UpdateSchema2(value0);
    };
    return UpdateSchema2;
  }();
  var renderProgramOutput = function(v) {
    return div_([div_([label_([text6("Exit code:")]), text6(show7(v.exitCode))]), div_([label_([text6("Standard output:")]), pre_([samp_([text6(v.stdout)])])]), div_([label_([text6("Standard error:")]), pre_([samp_([text6(v.stderr)])])])]);
  };
  var renderProgramInput = function(v) {
    return form_([div_([label_([text6("JSON schema:")]), input2([onValueInput(UpdateSchema.create), value13(v.schemaText)])]), div_([label_([text6("JSON value:")]), input2([onValueInput(UpdateJson.create), value13(v.jsonText)])]), button([onClick(function(v1) {
      return RunProgram.value;
    }), type_19(ButtonButton.value)])([text6("execute program")])]);
  };
  var render5 = function(v) {
    return div_([renderProgramInput(v.programInput), maybe(text6(""))(function($53) {
      return fromPlainHTML(renderProgramOutput($53));
    })(v.programOutput)]);
  };
  var initialSchema = /* @__PURE__ */ function() {
    return new ObjectSchema({
      exclusiveMaximum: defaultKeywords.exclusiveMaximum,
      exclusiveMinimum: defaultKeywords.exclusiveMinimum,
      items: defaultKeywords.items,
      maximum: defaultKeywords.maximum,
      minimum: defaultKeywords.minimum,
      multipleOf: defaultKeywords.multipleOf,
      not: defaultKeywords.not,
      required: defaultKeywords.required,
      typeKeyword: new Just(singleton8(JsonObject.value)),
      uniqueItems: defaultKeywords.uniqueItems
    });
  }();
  var initialJson = /* @__PURE__ */ jsonSingletonObject("foo")(/* @__PURE__ */ id(1));
  var initialState = function(v) {
    return {
      outputFormat: Json2.value,
      programInput: {
        jsonText: stringify(initialJson),
        schemaText: show13(print(initialSchema))
      },
      programOutput: Nothing.value
    };
  };
  var handleAction = function(v) {
    if (v instanceof RunProgram) {
      return modify_3(function(st) {
        var $36 = {};
        for (var $37 in st) {
          if ({}.hasOwnProperty.call(st, $37)) {
            $36[$37] = st[$37];
          }
          ;
        }
        ;
        $36.programOutput = new Just(runProgram2(st.outputFormat)(compute)(st.programInput));
        return $36;
      });
    }
    ;
    if (v instanceof UpdateJson) {
      return modify_3(function(st) {
        var $42 = {};
        for (var $43 in st) {
          if ({}.hasOwnProperty.call(st, $43)) {
            $42[$43] = st[$43];
          }
          ;
        }
        ;
        $42.programInput = function() {
          var $39 = {};
          for (var $40 in st.programInput) {
            if ({}.hasOwnProperty.call(st.programInput, $40)) {
              $39[$40] = st["programInput"][$40];
            }
            ;
          }
          ;
          $39.jsonText = v.value0;
          return $39;
        }();
        return $42;
      });
    }
    ;
    if (v instanceof UpdateSchema) {
      return modify_3(function(st) {
        var $49 = {};
        for (var $50 in st) {
          if ({}.hasOwnProperty.call(st, $50)) {
            $49[$50] = st[$50];
          }
          ;
        }
        ;
        $49.programInput = function() {
          var $46 = {};
          for (var $47 in st.programInput) {
            if ({}.hasOwnProperty.call(st.programInput, $47)) {
              $46[$47] = st["programInput"][$47];
            }
            ;
          }
          ;
          $46.schemaText = v.value0;
          return $46;
        }();
        return $49;
      });
    }
    ;
    throw new Error("Failed pattern match at Sandbox.Main (line 114, column 16 - line 127, column 72): " + [v.constructor.name]);
  };
  var component = /* @__PURE__ */ function() {
    return mkComponent({
      initialState,
      render: render5,
      "eval": mkEval({
        handleAction,
        handleQuery: defaultEval.handleQuery,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      })
    });
  }();
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(component)(unit)(body2);
  }));

  // <stdin>
  main2();
})();
