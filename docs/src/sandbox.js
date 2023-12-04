(() => {
  // output/CLI/index.js
  var Compat = /* @__PURE__ */ function() {
    function Compat2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Compat2.create = function(value0) {
      return function(value1) {
        return new Compat2(value0, value1);
      };
    };
    return Compat2;
  }();
  var Diff = /* @__PURE__ */ function() {
    function Diff2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Diff2.create = function(value0) {
      return function(value1) {
        return new Diff2(value0, value1);
      };
    };
    return Diff2;
  }();
  var Validate = /* @__PURE__ */ function() {
    function Validate2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Validate2.create = function(value0) {
      return function(value1) {
        return new Validate2(value0, value1);
      };
    };
    return Validate2;
  }();

  // output/Data.Unit/foreign.js
  var unit = void 0;

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

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq9) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq9 : gt;
          };
        };
      };
    };
  };
  var ordBooleanImpl = unsafeCompareImpl;
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;
  var ordCharImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
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
  var eqRowNil = {
    eqRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return true;
        };
      };
    }
  };
  var eqRecord = function(dict) {
    return dict.eqRecord;
  };
  var eqRec = function() {
    return function(dictEqRecord) {
      return {
        eq: eqRecord(dictEqRecord)($$Proxy.value)
      };
    };
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
  var eqBoolean = {
    eq: eqBooleanImpl
  };
  var eq1 = function(dict) {
    return dict.eq1;
  };
  var eq = function(dict) {
    return dict.eq;
  };
  var eq2 = /* @__PURE__ */ eq(eqBoolean);
  var eqRowCons = function(dictEqRecord) {
    var eqRecord1 = eqRecord(dictEqRecord);
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        return function(dictEq) {
          var eq36 = eq(dictEq);
          return {
            eqRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var tail2 = eqRecord1($$Proxy.value)(ra)(rb);
                  var key = reflectSymbol2($$Proxy.value);
                  var get6 = unsafeGet(key);
                  return eq36(get6(ra))(get6(rb)) && tail2;
                };
              };
            }
          };
        };
      };
    };
  };
  var notEq = function(dictEq) {
    var eq36 = eq(dictEq);
    return function(x) {
      return function(y) {
        return eq2(eq36(x)(y))(false);
      };
    };
  };

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
  var eqRec2 = /* @__PURE__ */ eqRec();
  var notEq2 = /* @__PURE__ */ notEq(eqOrdering);
  var ordUnit = {
    compare: function(v) {
      return function(v1) {
        return EQ.value;
      };
    },
    Eq0: function() {
      return eqUnit;
    }
  };
  var ordString = /* @__PURE__ */ function() {
    return {
      compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqString;
      }
    };
  }();
  var ordRecordNil = {
    compareRecord: function(v) {
      return function(v1) {
        return function(v2) {
          return EQ.value;
        };
      };
    },
    EqRecord0: function() {
      return eqRowNil;
    }
  };
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
  var ordBoolean = /* @__PURE__ */ function() {
    return {
      compare: ordBooleanImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqBoolean;
      }
    };
  }();
  var compareRecord = function(dict) {
    return dict.compareRecord;
  };
  var ordRecord = function() {
    return function(dictOrdRecord) {
      var eqRec1 = eqRec2(dictOrdRecord.EqRecord0());
      return {
        compare: compareRecord(dictOrdRecord)($$Proxy.value),
        Eq0: function() {
          return eqRec1;
        }
      };
    };
  };
  var compare1 = function(dict) {
    return dict.compare1;
  };
  var compare = function(dict) {
    return dict.compare;
  };
  var ordRecordCons = function(dictOrdRecord) {
    var compareRecord1 = compareRecord(dictOrdRecord);
    var eqRowCons2 = eqRowCons(dictOrdRecord.EqRecord0())();
    return function() {
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        var eqRowCons1 = eqRowCons2(dictIsSymbol);
        return function(dictOrd) {
          var compare35 = compare(dictOrd);
          var eqRowCons22 = eqRowCons1(dictOrd.Eq0());
          return {
            compareRecord: function(v) {
              return function(ra) {
                return function(rb) {
                  var key = reflectSymbol2($$Proxy.value);
                  var left = compare35(unsafeGet(key)(ra))(unsafeGet(key)(rb));
                  var $95 = notEq2(left)(EQ.value);
                  if ($95) {
                    return left;
                  }
                  ;
                  return compareRecord1($$Proxy.value)(ra)(rb);
                };
              };
            },
            EqRecord0: function() {
              return eqRowCons22;
            }
          };
        };
      };
    };
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
  var compose = function(dict) {
    return dict.compose;
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

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };
  var mapFlipped = function(dictFunctor) {
    var map117 = map(dictFunctor);
    return function(fa) {
      return function(f) {
        return map117(f)(fa);
      };
    };
  };
  var $$void = function(dictFunctor) {
    return map(dictFunctor)($$const(unit));
  };
  var voidLeft = function(dictFunctor) {
    var map117 = map(dictFunctor);
    return function(f) {
      return function(x) {
        return map117($$const(x))(f);
      };
    };
  };
  var functorFn = {
    map: /* @__PURE__ */ compose(semigroupoidFn)
  };
  var functorArray = {
    map: arrayMap
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
  var showBoolean = {
    show: function(v) {
      if (v) {
        return "true";
      }
      ;
      if (!v) {
        return "false";
      }
      ;
      throw new Error("Failed pattern match at Data.Show (line 29, column 1 - line 31, column 23): " + [v.constructor.name]);
    }
  };
  var show = function(dict) {
    return dict.show;
  };

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
  var eqTuple = function(dictEq) {
    var eq9 = eq(dictEq);
    return function(dictEq1) {
      var eq17 = eq(dictEq1);
      return {
        eq: function(x) {
          return function(y) {
            return eq9(x.value0)(y.value0) && eq17(x.value1)(y.value1);
          };
        }
      };
    };
  };
  var ordTuple = function(dictOrd) {
    var compare11 = compare(dictOrd);
    var eqTuple1 = eqTuple(dictOrd.Eq0());
    return function(dictOrd1) {
      var compare18 = compare(dictOrd1);
      var eqTuple2 = eqTuple1(dictOrd1.Eq0());
      return {
        compare: function(x) {
          return function(y) {
            var v = compare11(x.value0)(y.value0);
            if (v instanceof LT) {
              return LT.value;
            }
            ;
            if (v instanceof GT) {
              return GT.value;
            }
            ;
            return compare18(x.value1)(y.value1);
          };
        },
        Eq0: function() {
          return eqTuple2;
        }
      };
    };
  };

  // output/Control.Monad.State.Class/index.js
  var state = function(dict) {
    return dict.state;
  };
  var put = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(s) {
      return state1(function(v) {
        return new Tuple(unit, s);
      });
    };
  };
  var modify_ = function(dictMonadState) {
    var state1 = state(dictMonadState);
    return function(f) {
      return state1(function(s) {
        return new Tuple(unit, f(s));
      });
    };
  };
  var get = function(dictMonadState) {
    return state(dictMonadState)(function(s) {
      return new Tuple(s, s);
    });
  };

  // output/Control.Apply/foreign.js
  var arrayApply = function(fs) {
    return function(xs) {
      var l = fs.length;
      var k = xs.length;
      var result = new Array(l * k);
      var n = 0;
      for (var i2 = 0; i2 < l; i2++) {
        var f = fs[i2];
        for (var j = 0; j < k; j++) {
          result[n++] = f(xs[j]);
        }
      }
      return result;
    };
  };

  // output/Control.Apply/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var applyArray = {
    apply: arrayApply,
    Functor0: function() {
      return functorArray;
    }
  };
  var apply = function(dict) {
    return dict.apply;
  };
  var applySecond = function(dictApply) {
    var apply1 = apply(dictApply);
    var map39 = map(dictApply.Functor0());
    return function(a2) {
      return function(b2) {
        return apply1(map39($$const(identity2))(a2))(b2);
      };
    };
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var unless = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (!v) {
          return v1;
        }
        ;
        if (v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var when = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(v) {
      return function(v1) {
        if (v) {
          return v1;
        }
        ;
        if (!v) {
          return pure16(unit);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure16 = pure(dictApplicative);
    return function(f) {
      return function(a2) {
        return apply2(pure16(f))(a2);
      };
    };
  };
  var applicativeArray = {
    pure: function(x) {
      return [x];
    },
    Apply0: function() {
      return applyArray;
    }
  };

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
    var bind16 = bind(dictBind);
    return function(f) {
      return function(g) {
        return function(a2) {
          return bind16(f(a2))(g);
        };
      };
    };
  };
  var discardUnit = {
    discard: function(dictBind) {
      return bind(dictBind);
    }
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
  var eqMaybe = function(dictEq) {
    var eq9 = eq(dictEq);
    return {
      eq: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return true;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return eq9(x.value0)(y.value0);
          }
          ;
          return false;
        };
      }
    };
  };
  var ordMaybe = function(dictOrd) {
    var compare11 = compare(dictOrd);
    var eqMaybe1 = eqMaybe(dictOrd.Eq0());
    return {
      compare: function(x) {
        return function(y) {
          if (x instanceof Nothing && y instanceof Nothing) {
            return EQ.value;
          }
          ;
          if (x instanceof Nothing) {
            return LT.value;
          }
          ;
          if (y instanceof Nothing) {
            return GT.value;
          }
          ;
          if (x instanceof Just && y instanceof Just) {
            return compare11(x.value0)(y.value0);
          }
          ;
          throw new Error("Failed pattern match at Data.Maybe (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
        };
      },
      Eq0: function() {
        return eqMaybe1;
      }
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
  var empty = function(dict) {
    return dict.empty;
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
    var pure16 = pure(dictApplicative);
    return function(dictFoldable) {
      var foldr22 = foldr(dictFoldable);
      return function(f) {
        return foldr22(function($454) {
          return applySecond2(f($454));
        })(pure16(unit));
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
      var append19 = append(dictMonoid.Semigroup0());
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
                acc: append19(v.acc)(append19(sep)(v1))
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
      var append19 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldr22(function(x) {
          return function(acc) {
            return append19(f(x))(acc);
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

  // output/Data.FoldableWithIndex/index.js
  var foldr8 = /* @__PURE__ */ foldr(foldableArray);
  var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
  var foldl8 = /* @__PURE__ */ foldl(foldableArray);
  var foldrWithIndex = function(dict) {
    return dict.foldrWithIndex;
  };
  var foldlWithIndex = function(dict) {
    return dict.foldlWithIndex;
  };
  var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
    var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
    return function(dictMonoid) {
      var append19 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldrWithIndex1(function(i2) {
          return function(x) {
            return function(acc) {
              return append19(f(i2)(x))(acc);
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
      var map39 = map(dictFunctor);
      return function(dictSemigroup) {
        var append19 = append(dictSemigroup);
        return function(f) {
          var $162 = foldl11(append19);
          var $163 = map39(f);
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

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = /* @__PURE__ */ function() {
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
      return function(map39) {
        return function(pure16) {
          return function(f) {
            return function(array) {
              function go2(bot, top6) {
                switch (top6 - bot) {
                  case 0:
                    return pure16([]);
                  case 1:
                    return map39(array1)(f(array[bot]));
                  case 2:
                    return apply2(map39(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map39(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top6 - bot) / 4) * 2;
                    return apply2(map39(concat2)(go2(bot, pivot)))(go2(pivot, top6));
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
              var value15 = b2;
              while (true) {
                var maybe2 = f(value15);
                if (isNothing2(maybe2))
                  return result;
                var tuple = fromJust5(maybe2);
                result.push(fst2(tuple));
                value15 = snd2(tuple);
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
              var value15 = b2;
              while (true) {
                var tuple = f(value15);
                result.push(fst2(tuple));
                var maybe2 = snd2(tuple);
                if (isNothing2(maybe2))
                  return result;
                value15 = fromJust5(maybe2);
              }
            };
          };
        };
      };
    };
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

  // output/Data.NonEmpty/index.js
  var map4 = /* @__PURE__ */ map(functorTuple);
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
          return uncurry(NonEmpty.create)(map4(unfoldr3(map1(f)))(f(b2)));
        };
      }
    };
  };
  var singleton2 = function(dictPlus) {
    var empty8 = empty(dictPlus);
    return function(a2) {
      return new NonEmpty(a2, empty8);
    };
  };
  var semigroupNonEmpty = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(dictSemigroup) {
      var append19 = append(dictSemigroup);
      return {
        append: function(v) {
          return function(v1) {
            return new NonEmpty(v.value0, append19(v.value1)(append19(pure16(v1.value0))(v1.value1)));
          };
        }
      };
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
    var foldMap5 = foldMap(dictFoldable);
    var foldl7 = foldl(dictFoldable);
    var foldr6 = foldr(dictFoldable);
    return {
      foldMap: function(dictMonoid) {
        var append19 = append(dictMonoid.Semigroup0());
        var foldMap15 = foldMap5(dictMonoid);
        return function(f) {
          return function(v) {
            return append19(f(v.value0))(foldMap15(f)(v.value1));
          };
        };
      },
      foldl: function(f) {
        return function(b2) {
          return function(v) {
            return foldl7(f)(f(b2)(v.value0))(v.value1);
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
    var foldl7 = foldl(dictFoldable);
    var foldr6 = foldr(dictFoldable);
    var foldableNonEmpty1 = foldableNonEmpty(dictFoldable);
    return {
      foldMap1: function(dictSemigroup) {
        var append19 = append(dictSemigroup);
        return function(f) {
          return function(v) {
            return foldl7(function(s) {
              return function(a1) {
                return append19(s)(f(a1));
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
          return foldl7(f)(v.value0)(v.value1);
        };
      },
      Foldable0: function() {
        return foldableNonEmpty1;
      }
    };
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
      var append24 = append(dictMonoid.Semigroup0());
      var mempty2 = mempty(dictMonoid);
      return function(f) {
        return foldl(foldableList)(function(acc) {
          var $286 = append24(acc);
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
      var eq9 = eq(dictEq);
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
                    $copy_v2 = v2 && eq9(v1.value0)(v.value0);
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
      var compare11 = compare(dictOrd);
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
                  var v2 = compare11(v.value0)(v1.value0);
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

  // output/Data.Map.Internal/index.js
  var $runtime_lazy = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var map5 = /* @__PURE__ */ map(functorMaybe);
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
  var singleton3 = function(k) {
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
          return singleton3(k)(v);
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
  var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy("unsafeSplit", "Data.Map.Internal", function() {
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
  var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy("unsafeSplitLast", "Data.Map.Internal", function() {
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
  var $lazy_unsafeDifference = /* @__PURE__ */ $runtime_lazy("unsafeDifference", "Data.Map.Internal", function() {
    return function(comp, l, r) {
      if (l instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (r instanceof Leaf) {
        return l;
      }
      ;
      if (r instanceof Node) {
        var v = unsafeSplit(comp, r.value2, l);
        var l$prime = $lazy_unsafeDifference(819)(comp, v.value1, r.value4);
        var r$prime = $lazy_unsafeDifference(820)(comp, v.value2, r.value5);
        return unsafeJoinNodes(l$prime, r$prime);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 814, column 39 - line 821, column 33): " + [l.constructor.name, r.constructor.name]);
    };
  });
  var unsafeDifference = /* @__PURE__ */ $lazy_unsafeDifference(813);
  var $lazy_unsafeUnionWith = /* @__PURE__ */ $runtime_lazy("unsafeUnionWith", "Data.Map.Internal", function() {
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
    var compare11 = compare(dictOrd);
    return function(app) {
      return function(m1) {
        return function(m2) {
          return unsafeUnionWith(compare11, app, m1, m2);
        };
      };
    };
  };
  var union = function(dictOrd) {
    return unionWith(dictOrd)($$const);
  };
  var pop = function(dictOrd) {
    var compare11 = compare(dictOrd);
    return function(k) {
      return function(m) {
        var v = unsafeSplit(compare11, k, m);
        return map5(function(a2) {
          return new Tuple(a2, unsafeJoinNodes(v.value1, v.value2));
        })(v.value0);
      };
    };
  };
  var member = function(dictOrd) {
    var compare11 = compare(dictOrd);
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
            var v1 = compare11(k)(v.value2);
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
  var lookup = function(dictOrd) {
    var compare11 = compare(dictOrd);
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
            var v1 = compare11(k)(v.value2);
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
    var eq17 = eq(dictEq);
    return function(dictEq1) {
      var eq26 = eq(dictEq1);
      return {
        eq: /* @__PURE__ */ function() {
          var go2 = function($copy_a) {
            return function($copy_b) {
              var $tco_var_a = $copy_a;
              var $tco_done = false;
              var $tco_result;
              function $tco_loop(a2, b2) {
                var v = stepAsc(a2);
                if (v instanceof IterNext) {
                  var v2 = stepAsc(b2);
                  if (v2 instanceof IterNext && (eq17(v.value0)(v2.value0) && eq26(v.value1)(v2.value1))) {
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
    var compare11 = compare(dictOrd);
    return function(app) {
      return function(k) {
        return function(v) {
          var go2 = function(v1) {
            if (v1 instanceof Leaf) {
              return singleton3(k)(v);
            }
            ;
            if (v1 instanceof Node) {
              var v2 = compare11(k)(v1.value2);
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
  var insert = function(dictOrd) {
    var compare11 = compare(dictOrd);
    return function(k) {
      return function(v) {
        var go2 = function(v1) {
          if (v1 instanceof Leaf) {
            return singleton3(k)(v);
          }
          ;
          if (v1 instanceof Node) {
            var v2 = compare11(k)(v1.value2);
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
        var $lazy_go = $runtime_lazy("go", "Data.Map.Internal", function() {
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
        var $lazy_go = $runtime_lazy("go", "Data.Map.Internal", function() {
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
      var append19 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append19(go2(v.value4))(append19(f(v.value3))(go2(v.value5)));
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
        var $lazy_go = $runtime_lazy("go", "Data.Map.Internal", function() {
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
        var $lazy_go = $runtime_lazy("go", "Data.Map.Internal", function() {
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
      var append19 = append(dictMonoid.Semigroup0());
      return function(f) {
        var go2 = function(v) {
          if (v instanceof Leaf) {
            return mempty2;
          }
          ;
          if (v instanceof Node) {
            return append19(go2(v.value4))(append19(f(v.value2)(v.value3))(go2(v.value5)));
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
  var keys = /* @__PURE__ */ function() {
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
  var filter = function(dictOrd) {
    var $769 = filterWithKey(dictOrd);
    return function($770) {
      return $769($$const($770));
    };
  };
  var eqMap = function(dictEq) {
    var eqMapIter1 = eqMapIter(dictEq);
    return function(dictEq1) {
      var eq17 = eq(eqMapIter1(dictEq1));
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
                return eq17(toMapIter(xs))(toMapIter(ys));
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
  var empty2 = /* @__PURE__ */ function() {
    return Leaf.value;
  }();
  var fromFoldable = function(dictOrd) {
    var insert13 = insert(dictOrd);
    return function(dictFoldable) {
      return foldl(dictFoldable)(function(m) {
        return function(v) {
          return insert13(v.value0)(v.value1)(m);
        };
      })(empty2);
    };
  };
  var difference = function(dictOrd) {
    var compare11 = compare(dictOrd);
    return function(m1) {
      return function(m2) {
        return unsafeDifference(compare11, m1, m2);
      };
    };
  };
  var $$delete = function(dictOrd) {
    var compare11 = compare(dictOrd);
    return function(k) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return Leaf.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare11(k)(v.value2);
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
    var compare11 = compare(dictOrd);
    return function(f) {
      return function(k) {
        return function(m) {
          var v = unsafeSplit(compare11, k, m);
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
  var $$null = function(s) {
    return s === "";
  };

  // output/DOM.HTML.Indexed.InputType/index.js
  var InputButton = /* @__PURE__ */ function() {
    function InputButton2() {
    }
    ;
    InputButton2.value = new InputButton2();
    return InputButton2;
  }();
  var InputCheckbox = /* @__PURE__ */ function() {
    function InputCheckbox2() {
    }
    ;
    InputCheckbox2.value = new InputCheckbox2();
    return InputCheckbox2;
  }();
  var InputColor = /* @__PURE__ */ function() {
    function InputColor2() {
    }
    ;
    InputColor2.value = new InputColor2();
    return InputColor2;
  }();
  var InputDate = /* @__PURE__ */ function() {
    function InputDate2() {
    }
    ;
    InputDate2.value = new InputDate2();
    return InputDate2;
  }();
  var InputDatetimeLocal = /* @__PURE__ */ function() {
    function InputDatetimeLocal2() {
    }
    ;
    InputDatetimeLocal2.value = new InputDatetimeLocal2();
    return InputDatetimeLocal2;
  }();
  var InputEmail = /* @__PURE__ */ function() {
    function InputEmail2() {
    }
    ;
    InputEmail2.value = new InputEmail2();
    return InputEmail2;
  }();
  var InputFile = /* @__PURE__ */ function() {
    function InputFile2() {
    }
    ;
    InputFile2.value = new InputFile2();
    return InputFile2;
  }();
  var InputHidden = /* @__PURE__ */ function() {
    function InputHidden2() {
    }
    ;
    InputHidden2.value = new InputHidden2();
    return InputHidden2;
  }();
  var InputImage = /* @__PURE__ */ function() {
    function InputImage2() {
    }
    ;
    InputImage2.value = new InputImage2();
    return InputImage2;
  }();
  var InputMonth = /* @__PURE__ */ function() {
    function InputMonth2() {
    }
    ;
    InputMonth2.value = new InputMonth2();
    return InputMonth2;
  }();
  var InputNumber = /* @__PURE__ */ function() {
    function InputNumber2() {
    }
    ;
    InputNumber2.value = new InputNumber2();
    return InputNumber2;
  }();
  var InputPassword = /* @__PURE__ */ function() {
    function InputPassword2() {
    }
    ;
    InputPassword2.value = new InputPassword2();
    return InputPassword2;
  }();
  var InputRadio = /* @__PURE__ */ function() {
    function InputRadio2() {
    }
    ;
    InputRadio2.value = new InputRadio2();
    return InputRadio2;
  }();
  var InputRange = /* @__PURE__ */ function() {
    function InputRange2() {
    }
    ;
    InputRange2.value = new InputRange2();
    return InputRange2;
  }();
  var InputReset = /* @__PURE__ */ function() {
    function InputReset2() {
    }
    ;
    InputReset2.value = new InputReset2();
    return InputReset2;
  }();
  var InputSearch = /* @__PURE__ */ function() {
    function InputSearch2() {
    }
    ;
    InputSearch2.value = new InputSearch2();
    return InputSearch2;
  }();
  var InputSubmit = /* @__PURE__ */ function() {
    function InputSubmit2() {
    }
    ;
    InputSubmit2.value = new InputSubmit2();
    return InputSubmit2;
  }();
  var InputTel = /* @__PURE__ */ function() {
    function InputTel2() {
    }
    ;
    InputTel2.value = new InputTel2();
    return InputTel2;
  }();
  var InputText = /* @__PURE__ */ function() {
    function InputText2() {
    }
    ;
    InputText2.value = new InputText2();
    return InputText2;
  }();
  var InputTime = /* @__PURE__ */ function() {
    function InputTime2() {
    }
    ;
    InputTime2.value = new InputTime2();
    return InputTime2;
  }();
  var InputUrl = /* @__PURE__ */ function() {
    function InputUrl2() {
    }
    ;
    InputUrl2.value = new InputUrl2();
    return InputUrl2;
  }();
  var InputWeek = /* @__PURE__ */ function() {
    function InputWeek2() {
    }
    ;
    InputWeek2.value = new InputWeek2();
    return InputWeek2;
  }();
  var renderInputType = function(v) {
    if (v instanceof InputButton) {
      return "button";
    }
    ;
    if (v instanceof InputCheckbox) {
      return "checkbox";
    }
    ;
    if (v instanceof InputColor) {
      return "color";
    }
    ;
    if (v instanceof InputDate) {
      return "date";
    }
    ;
    if (v instanceof InputDatetimeLocal) {
      return "datetime-local";
    }
    ;
    if (v instanceof InputEmail) {
      return "email";
    }
    ;
    if (v instanceof InputFile) {
      return "file";
    }
    ;
    if (v instanceof InputHidden) {
      return "hidden";
    }
    ;
    if (v instanceof InputImage) {
      return "image";
    }
    ;
    if (v instanceof InputMonth) {
      return "month";
    }
    ;
    if (v instanceof InputNumber) {
      return "number";
    }
    ;
    if (v instanceof InputPassword) {
      return "password";
    }
    ;
    if (v instanceof InputRadio) {
      return "radio";
    }
    ;
    if (v instanceof InputRange) {
      return "range";
    }
    ;
    if (v instanceof InputReset) {
      return "reset";
    }
    ;
    if (v instanceof InputSearch) {
      return "search";
    }
    ;
    if (v instanceof InputSubmit) {
      return "submit";
    }
    ;
    if (v instanceof InputTel) {
      return "tel";
    }
    ;
    if (v instanceof InputText) {
      return "text";
    }
    ;
    if (v instanceof InputTime) {
      return "time";
    }
    ;
    if (v instanceof InputUrl) {
      return "url";
    }
    ;
    if (v instanceof InputWeek) {
      return "week";
    }
    ;
    throw new Error("Failed pattern match at DOM.HTML.Indexed.InputType (line 33, column 19 - line 55, column 22): " + [v.constructor.name]);
  };

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

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value15) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value15);
  };
  var replicatePolyfill = function(count, value15) {
    var result = [];
    var n = 0;
    for (var i2 = 0; i2 < count; i2++) {
      result[n++] = value15;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ function() {
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

  // output/Control.Monad/index.js
  var unlessM = function(dictMonad) {
    var bind16 = bind(dictMonad.Bind1());
    var unless2 = unless(dictMonad.Applicative0());
    return function(mb) {
      return function(m) {
        return bind16(mb)(function(b2) {
          return unless2(b2)(m);
        });
      };
    };
  };
  var ap = function(dictMonad) {
    var bind16 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a2) {
        return bind16(f)(function(f$prime) {
          return bind16(a2)(function(a$prime) {
            return pure16(f$prime(a$prime));
          });
        });
      };
    };
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
  var $runtime_lazy2 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy2("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy2("applyEffect", "Effect", function() {
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
  var modify_2 = function(f) {
    return function(s) {
      return $$void2(modify(f)(s));
    };
  };

  // output/Control.Monad.Rec.Class/index.js
  var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
  var map6 = /* @__PURE__ */ map(functorEffect);
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
          return map6(fromDone)(read(r))();
        };
      };
    },
    Monad0: function() {
      return monadEffect;
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

  // output/Control.Monad.ST.Internal/index.js
  var $runtime_lazy3 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
  var $lazy_applyST = /* @__PURE__ */ $runtime_lazy3("applyST", "Control.Monad.ST.Internal", function() {
    return {
      apply: ap(monadST),
      Functor0: function() {
        return functorST;
      }
    };
  });

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

  // output/Data.Array/index.js
  var fromJust4 = /* @__PURE__ */ fromJust();
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var singleton4 = function(a2) {
    return [a2];
  };
  var replicate = /* @__PURE__ */ runFn2(replicateImpl);
  var fromFoldable2 = function(dictFoldable) {
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
  var step = function(v, a2) {
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
  var map7 = /* @__PURE__ */ map(functorArray);
  var map12 = /* @__PURE__ */ map(functorTuple);
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
      if (v2 instanceof Text) {
        return new Text(v2.value0);
      }
      ;
      if (v2 instanceof Elem) {
        return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map7(go2)(v2.value3));
      }
      ;
      if (v2 instanceof Keyed) {
        return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map7(map12(go2))(v2.value3));
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
  function createElement(ns, name16, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name16);
    } else {
      return doc.createElement(name16);
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
  function addEventListener(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

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

  // output/Halogen.VDom.Util/index.js
  var unsafeLookup = unsafeGetAny;
  var unsafeFreeze2 = unsafeCoerce2;
  var pokeMutMap = unsafeSetAny;
  var newMutMap = newImpl;

  // output/Web.DOM.Element/foreign.js
  var getProp = function(name16) {
    return function(doctype) {
      return doctype[name16];
    };
  };
  var _namespaceURI = getProp("namespaceURI");
  var _prefix = getProp("prefix");
  var localName = getProp("localName");
  var tagName = getProp("tagName");

  // output/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
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

  // output/Web.DOM.ParentNode/index.js
  var map8 = /* @__PURE__ */ map(functorEffect);
  var querySelector = function(qs) {
    var $2 = map8(toMaybe);
    var $3 = _querySelector(qs);
    return function($4) {
      return $2($3($4));
    };
  };

  // output/Web.DOM.Element/index.js
  var toNode = unsafeCoerce2;

  // output/Halogen.VDom.DOM/index.js
  var $runtime_lazy4 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var haltWidget = function(v) {
    return halt(v.widget);
  };
  var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy4("patchWidget", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Widget) {
        var res = step(state3.widget, vdom.value0);
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
  var $lazy_patchText = /* @__PURE__ */ $runtime_lazy4("patchText", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchText(82)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Text) {
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
  var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy4("patchElem", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        var v1 = length(state3.children);
        if (v1 === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
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
          var res = step(s, v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var onThat = function(ix, v2) {
          var res = state3.build(v2);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
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
  var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy4("patchKeyed", "Halogen.VDom.DOM", function() {
    return function(state3, vdom) {
      if (vdom instanceof Grafted) {
        return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
      }
      ;
      if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
        var v = length(vdom.value3);
        if (state3.length === 0 && v === 0) {
          var attrs2 = step(state3.attrs, vdom.value2);
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
          var res = step(s, v3.value1);
          insertChildIx(ix$prime, extract2(res), state3.node);
          return res;
        };
        var onThat = function(v2, ix, v3) {
          var res = state3.build(v3.value1);
          insertChildIx(ix, extract2(res), state3.node);
          return res;
        };
        var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
        var attrs2 = step(state3.attrs, vdom.value2);
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
    var node = toNode(el);
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
    var node = toNode(el);
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
    var $lazy_build = $runtime_lazy4("build", "Halogen.VDom.DOM", function() {
      return function(v) {
        if (v instanceof Text) {
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
  function typeOf(value15) {
    return typeof value15;
  }
  function tagOf(value15) {
    return Object.prototype.toString.call(value15).slice(8, -1);
  }
  var isArray = Array.isArray || function(value15) {
    return Object.prototype.toString.call(value15) === "[object Array]";
  };

  // output/Effect.Exception/foreign.js
  function error(msg) {
    return new Error(msg);
  }
  function message(e) {
    return e.message;
  }
  function throwException(e) {
    return function() {
      throw e;
    };
  }
  function catchException(c) {
    return function(t) {
      return function() {
        try {
          return t();
        } catch (e) {
          if (e instanceof Error || Object.prototype.toString.call(e) === "[object Error]") {
            return c(e)();
          } else {
            return c(new Error(e.toString()))();
          }
        }
      };
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
  var monadThrowEffect = {
    throwError: throwException,
    Monad0: function() {
      return monadEffect;
    }
  };
  var monadErrorEffect = {
    catchError: /* @__PURE__ */ flip(catchException),
    MonadThrow0: function() {
      return monadThrowEffect;
    }
  };
  var liftEither = function(dictMonadThrow) {
    return either(throwError(dictMonadThrow))(pure(dictMonadThrow.Monad0().Applicative0()));
  };
  var catchError = function(dict) {
    return dict.catchError;
  };
  var $$try = function(dictMonadError) {
    var catchError1 = catchError(dictMonadError);
    var Monad0 = dictMonadError.MonadThrow0().Monad0();
    var map39 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    return function(a2) {
      return catchError1(map39(Right.create)(a2))(function($52) {
        return pure16(Left.create($52));
      });
    };
  };

  // output/Control.Monad.Reader.Class/index.js
  var ask = function(dict) {
    return dict.ask;
  };
  var asks = function(dictMonadAsk) {
    var map39 = map(dictMonadAsk.Monad0().Bind1().Apply0().Functor0());
    var ask1 = ask(dictMonadAsk);
    return function(f) {
      return map39(f)(ask1);
    };
  };

  // output/Control.Monad.Trans.Class/index.js
  var lift = function(dict) {
    return dict.lift;
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
  var map9 = /* @__PURE__ */ map(functorEither);
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
    var map117 = map(dictFunctor);
    return {
      map: function(f) {
        return mapExceptT(map117(map9(f)));
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
    var bind16 = bind(dictMonad.Bind1());
    var pure16 = pure(dictMonad.Applicative0());
    return {
      bind: function(v) {
        return function(k) {
          return bind16(v)(either(function($187) {
            return pure16(Left.create($187));
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

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
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
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
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

  // output/Data.List/index.js
  var map10 = /* @__PURE__ */ map(functorMaybe);
  var foldr3 = /* @__PURE__ */ foldr(foldableList);
  var uncons = function(v) {
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
  var toUnfoldable = function(dictUnfoldable) {
    return unfoldr(dictUnfoldable)(function(xs) {
      return map10(function(rec) {
        return new Tuple(rec.head, rec.tail);
      })(uncons(xs));
    });
  };
  var snoc = function(xs) {
    return function(x) {
      return foldr3(Cons.create)(new Cons(x, Nil.value))(xs);
    };
  };
  var reverse2 = /* @__PURE__ */ function() {
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
  var unsnoc = function(lst) {
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
    return map10(function(h) {
      return {
        init: reverse2(h.revInit),
        last: h.last
      };
    })(go2(lst)(Nil.value));
  };
  var $$null2 = function(v) {
    if (v instanceof Nil) {
      return true;
    }
    ;
    return false;
  };
  var fromFoldable3 = function(dictFoldable) {
    return foldr(dictFoldable)(Cons.create)(Nil.value);
  };
  var dropWhile = function(p2) {
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
  var wrappedOperation = function(name16) {
    return function(f) {
      return function(v) {
        var v1 = f(new Cons(v.value0, v.value1));
        if (v1 instanceof Cons) {
          return new NonEmpty(v1.value0, v1.value1);
        }
        ;
        if (v1 instanceof Nil) {
          return unsafeCrashWith("Impossible: empty list in NonEmptyList " + name16);
        }
        ;
        throw new Error("Failed pattern match at Data.List.NonEmpty (line 92, column 3 - line 94, column 81): " + [v1.constructor.name]);
      };
    };
  };
  var unsnoc2 = function(v) {
    var v1 = unsnoc(v.value1);
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
  var uncons2 = function(v) {
    return {
      head: v.value0,
      tail: v.value1
    };
  };
  var toList2 = function(v) {
    return new Cons(v.value0, v.value1);
  };
  var singleton5 = /* @__PURE__ */ function() {
    var $200 = singleton2(plusList);
    return function($201) {
      return NonEmptyList($200($201));
    };
  }();
  var snoc$prime = function(v) {
    return function(v1) {
      if (v instanceof Cons) {
        return new NonEmpty(v.value0, snoc(v.value1)(v1));
      }
      ;
      if (v instanceof Nil) {
        return singleton5(v1);
      }
      ;
      throw new Error("Failed pattern match at Data.List.NonEmpty (line 140, column 1 - line 140, column 51): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var reverse3 = /* @__PURE__ */ wrappedOperation("reverse")(reverse2);
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
  var cons$prime = function(x) {
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
    var fromFoldable111 = fromFoldable3(dictFoldable);
    return function(v) {
      return function(ys) {
        return new NonEmpty(v.value0, append12(v.value1)(fromFoldable111(ys)));
      };
    };
  };

  // output/Data.String.CodeUnits/foreign.js
  var singleton6 = function(c) {
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
  var take2 = function(n) {
    return function(s) {
      return s.substr(0, n);
    };
  };
  var drop2 = function(n) {
    return function(s) {
      return s.substring(n);
    };
  };
  var splitAt = function(i2) {
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
      var v1 = splitAt(length3(v))(str);
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

  // output/Foreign/index.js
  var pure2 = /* @__PURE__ */ pure(applicativeEither);
  var TypeMismatch = /* @__PURE__ */ function() {
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
      return $153(singleton5($154));
    };
  };
  var unsafeReadTagged = function(dictMonad) {
    var pure16 = pure(applicativeExceptT(dictMonad));
    var fail1 = fail(dictMonad);
    return function(tag) {
      return function(value15) {
        if (tagOf(value15) === tag) {
          return pure16(unsafeFromForeign(value15));
        }
        ;
        if (otherwise) {
          return fail1(new TypeMismatch(tag, tagOf(value15)));
        }
        ;
        throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value15.constructor.name]);
      };
    };
  };
  var readNumber = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("Number");
  };
  var readInt = function(dictMonad) {
    var map39 = map(dictMonad.Bind1().Apply0().Functor0());
    var readNumber1 = readNumber(dictMonad);
    return function(value15) {
      var error4 = new Left(singleton5(new TypeMismatch("Int", tagOf(value15))));
      var fromNumber2 = function() {
        var $155 = maybe(error4)(pure2);
        return function($156) {
          return $155(fromNumber($156));
        };
      }();
      return mapExceptT(map39(either($$const(error4))(fromNumber2)))(readNumber1(value15));
    };
  };
  var readString = function(dictMonad) {
    return unsafeReadTagged(dictMonad)("String");
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
  var empty3 = {};
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
  var keys2 = Object.keys || toArrayWithKey(function(k) {
    return function() {
      return k;
    };
  });

  // output/Foreign.Object/index.js
  var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindST);
  var $$void3 = /* @__PURE__ */ $$void(functorST);
  var thawST = _copyST;
  var singleton7 = function(k) {
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
  var lookup2 = /* @__PURE__ */ function() {
    return runFn4(_lookup)(Nothing.value)(Just.create);
  }();
  var insert2 = function(k) {
    return function(v) {
      return mutate(poke2(k)(v));
    };
  };
  var fromFoldable4 = function(dictFoldable) {
    var fromFoldable111 = fromFoldable2(dictFoldable);
    return function(l) {
      return runST(function __do2() {
        var s = newImpl();
        foreach(fromFoldable111(l))(function(v) {
          return $$void3(poke2(v.value0)(v.value1)(s));
        })();
        return s;
      });
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
  function addEventListener2(type) {
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
  function removeEventListener2(type) {
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

  // output/Halogen.VDom.DOM.Prop/index.js
  var $runtime_lazy5 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
  var Property = /* @__PURE__ */ function() {
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
    if (v instanceof Property) {
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
  var propFromInt = unsafeCoerce2;
  var propFromBoolean = unsafeCoerce2;
  var buildProp = function(emit) {
    return function(el) {
      var removeProp = function(prevEvents) {
        return function(v, v1) {
          if (v1 instanceof Attribute) {
            return removeAttribute(toNullable(v1.value0), v1.value1, el);
          }
          ;
          if (v1 instanceof Property) {
            return removeProperty(v1.value0, el);
          }
          ;
          if (v1 instanceof Handler) {
            var handler3 = unsafeLookup(v1.value0, prevEvents);
            return removeEventListener(v1.value0, fst(handler3), el);
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
        var v = lookup2("ref")(state3.props);
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
          if (v11 instanceof Property && v2 instanceof Property) {
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
          if (v2 instanceof Property) {
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
            addEventListener(v2.value0, listener, el);
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
      var $lazy_patchProp = $runtime_lazy5("patchProp", "Halogen.VDom.DOM.Prop", function() {
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
  var widget = function($28) {
    return HTML(Widget.create($28));
  };
  var toPropValue = function(dict) {
    return dict.toPropValue;
  };
  var text = function($29) {
    return HTML(Text.create($29));
  };
  var prop = function(dictIsProp) {
    var toPropValue1 = toPropValue(dictIsProp);
    return function(v) {
      var $31 = Property.create(v);
      return function($32) {
        return $31(toPropValue1($32));
      };
    };
  };
  var isPropString = {
    toPropValue: propFromString
  };
  var isPropInt = {
    toPropValue: propFromInt
  };
  var isPropInputType = {
    toPropValue: function($45) {
      return propFromString(renderInputType($45));
    }
  };
  var isPropButtonType = {
    toPropValue: function($50) {
      return propFromString(renderButtonType($50));
    }
  };
  var isPropBoolean = {
    toPropValue: propFromBoolean
  };
  var handler = /* @__PURE__ */ function() {
    return Handler.create;
  }();
  var element = function(ns) {
    return function(name16) {
      return function(props) {
        return function(children2) {
          return new Elem(ns, name16, props, children2);
        };
      };
    };
  };
  var attr = function(ns) {
    return function(v) {
      return Attribute.create(ns)(v);
    };
  };

  // output/Halogen.HTML.Elements/index.js
  var element2 = /* @__PURE__ */ function() {
    return element(Nothing.value);
  }();
  var fieldset = /* @__PURE__ */ element2("fieldset");
  var fieldset_ = /* @__PURE__ */ fieldset([]);
  var form = /* @__PURE__ */ element2("form");
  var form_ = /* @__PURE__ */ form([]);
  var input = function(props) {
    return element2("input")(props)([]);
  };
  var label = /* @__PURE__ */ element2("label");
  var label_ = /* @__PURE__ */ label([]);
  var legend = /* @__PURE__ */ element2("legend");
  var legend_ = /* @__PURE__ */ legend([]);
  var option = /* @__PURE__ */ element2("option");
  var pre = /* @__PURE__ */ element2("pre");
  var pre_ = /* @__PURE__ */ pre([]);
  var samp = /* @__PURE__ */ element2("samp");
  var samp_ = /* @__PURE__ */ samp([]);
  var select = /* @__PURE__ */ element2("select");
  var textarea = function(es) {
    return element2("textarea")(es)([]);
  };
  var div2 = /* @__PURE__ */ element2("div");
  var div_ = /* @__PURE__ */ div2([]);
  var button = /* @__PURE__ */ element2("button");

  // output/Control.Monad.Except/index.js
  var unwrap2 = /* @__PURE__ */ unwrap();
  var runExcept = function($3) {
    return unwrap2(runExceptT($3));
  };

  // output/Effect.Unsafe/foreign.js
  var unsafePerformEffect = function(f) {
    return f();
  };

  // output/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value15) {
    return value15 == null ? f : s(value15[key]);
  }

  // output/Foreign.Index/index.js
  var unsafeReadProp = function(dictMonad) {
    var fail2 = fail(dictMonad);
    var pure16 = pure(applicativeExceptT(dictMonad));
    return function(k) {
      return function(value15) {
        return unsafeReadPropImpl(fail2(new TypeMismatch("object", typeOf(value15))), pure16, k, value15);
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

  // output/Web.Event.Event/index.js
  var currentTarget = function($5) {
    return toMaybe(_currentTarget($5));
  };

  // output/Web.HTML.Event.EventTypes/index.js
  var domcontentloaded = "DOMContentLoaded";
  var change = "change";

  // output/Web.UIEvent.MouseEvent.EventTypes/index.js
  var click = "click";

  // output/Halogen.HTML.Events/index.js
  var map11 = /* @__PURE__ */ map(functorMaybe);
  var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindMaybe);
  var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
  var readProp2 = /* @__PURE__ */ readProp(monadIdentity);
  var readString2 = /* @__PURE__ */ readString(monadIdentity);
  var mouseHandler = unsafeCoerce2;
  var handler$prime = function(et) {
    return function(f) {
      return handler(et)(function(ev) {
        return map11(Action.create)(f(ev));
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
    var $15 = handler2(click);
    return function($16) {
      return $15(mouseHandler($16));
    };
  }();
  var addForeignPropHandler = function(key) {
    return function(prop4) {
      return function(reader) {
        return function(f) {
          var go2 = function(a2) {
            return composeKleisliFlipped2(reader)(readProp2(prop4))(unsafeToForeign(a2));
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
  var onSelectedIndexChange = /* @__PURE__ */ addForeignPropHandler(change)("selectedIndex")(/* @__PURE__ */ readInt(monadIdentity));
  var onValueChange = /* @__PURE__ */ addForeignPropHandler(change)("value")(readString2);

  // output/Halogen.HTML.Properties/index.js
  var prop2 = function(dictIsProp) {
    return prop(dictIsProp);
  };
  var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
  var prop22 = /* @__PURE__ */ prop2(isPropString);
  var prop3 = /* @__PURE__ */ prop2(isPropInt);
  var rows = /* @__PURE__ */ prop3("rows");
  var selected = /* @__PURE__ */ prop1("selected");
  var type_3 = function(dictIsProp) {
    return prop2(dictIsProp)("type");
  };
  var value2 = function(dictIsProp) {
    return prop2(dictIsProp)("value");
  };
  var name3 = /* @__PURE__ */ prop22("name");
  var id2 = /* @__PURE__ */ prop22("id");
  var $$for = /* @__PURE__ */ prop22("htmlFor");
  var checked2 = /* @__PURE__ */ prop1("checked");
  var attr2 = /* @__PURE__ */ function() {
    return attr(Nothing.value);
  }();
  var style = /* @__PURE__ */ attr2("style");

  // output/Control.Applicative.Free/index.js
  var identity5 = /* @__PURE__ */ identity(categoryFn);
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
    var pure16 = pure(dictApplicative);
    return function(fStack) {
      return function(valStack) {
        return function(nat) {
          return function(func) {
            return function(count) {
              if (func instanceof Pure) {
                return new Tuple(new Cons({
                  func: pure16(func.value0),
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
    var pure16 = pure(dictApplicative);
    var goLeft1 = goLeft(dictApplicative);
    return function(nat) {
      return function(z) {
        var go2 = function($copy_v) {
          var $tco_done = false;
          var $tco_result;
          function $tco_loop(v) {
            if (v.value1.value0 instanceof Pure) {
              var v1 = goApply1(v.value0)(v.value1.value1)(pure16(v.value1.value0.value0));
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
        return go2(new Tuple(Nil.value, singleton5(z)));
      };
    };
  };
  var retractFreeAp = function(dictApplicative) {
    return foldFreeAp(dictApplicative)(identity5);
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
  var uncons3 = function($copy_v) {
    var $tco_done = false;
    var $tco_result;
    function $tco_loop(v) {
      if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
        $tco_done = true;
        return Nothing.value;
      }
      ;
      if (v.value0 instanceof Nil) {
        $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
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
  var snoc2 = function(v) {
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
  var empty4 = /* @__PURE__ */ function() {
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
        return new CatCons(v.value0, snoc2(v.value1)(v1));
      }
      ;
      throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var foldr4 = function(k) {
    return function(b2) {
      return function(q2) {
        var foldl7 = function($copy_v) {
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
              var v = uncons3(xs);
              if (v instanceof Nothing) {
                $tco_done1 = true;
                return foldl7(function(x) {
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
  var uncons4 = function(v) {
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
        return foldr4(link)(CatNil.value)(v.value1);
      }()));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
  };
  var empty5 = /* @__PURE__ */ function() {
    return CatNil.value;
  }();
  var append3 = link;
  var semigroupCatList = {
    append: append3
  };
  var snoc3 = function(cat) {
    return function(a2) {
      return append3(cat)(new CatCons(a2, empty4));
    };
  };

  // output/Control.Monad.Free/index.js
  var $runtime_lazy6 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var append4 = /* @__PURE__ */ append(semigroupCatList);
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
          return new Free(v22.value0, append4(v22.value1)(r));
        };
      };
      if (v.value0 instanceof Return) {
        var v2 = uncons4(v.value1);
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
    return new Free(f, empty5);
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
        return new Free(v.value0, snoc3(v.value1)(k));
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
  var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy6("freeApply", "Control.Monad.Free", function() {
    return {
      apply: ap(freeMonad),
      Functor0: function() {
        return freeFunctor;
      }
    };
  });
  var pure3 = /* @__PURE__ */ pure(freeApplicative);
  var liftF = function(f) {
    return fromView(new Bind(f, function($192) {
      return pure3($192);
    }));
  };
  var foldFree = function(dictMonadRec) {
    var Monad0 = dictMonadRec.Monad0();
    var map117 = map(Monad0.Bind1().Apply0().Functor0());
    var pure16 = pure(Monad0.Applicative0());
    var tailRecM4 = tailRecM(dictMonadRec);
    return function(k) {
      var go2 = function(f) {
        var v = toView(f);
        if (v instanceof Return) {
          return map117(Done.create)(pure16(v.value0));
        }
        ;
        if (v instanceof Bind) {
          return map117(function($199) {
            return Loop.create(v.value1($199));
          })(k(v.value0));
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
      };
      return tailRecM4(go2);
    };
  };

  // output/Control.Monad.Reader.Trans/index.js
  var ReaderT = function(x) {
    return x;
  };
  var runReaderT = function(v) {
    return v;
  };
  var monadTransReaderT = {
    lift: function(dictMonad) {
      return function($147) {
        return ReaderT($$const($147));
      };
    }
  };
  var lift3 = /* @__PURE__ */ lift(monadTransReaderT);
  var mapReaderT = function(f) {
    return function(v) {
      return function($148) {
        return f(v($148));
      };
    };
  };
  var functorReaderT = function(dictFunctor) {
    return {
      map: function() {
        var $149 = map(dictFunctor);
        return function($150) {
          return mapReaderT($149($150));
        };
      }()
    };
  };
  var applyReaderT = function(dictApply) {
    var apply2 = apply(dictApply);
    var functorReaderT1 = functorReaderT(dictApply.Functor0());
    return {
      apply: function(v) {
        return function(v1) {
          return function(r) {
            return apply2(v(r))(v1(r));
          };
        };
      },
      Functor0: function() {
        return functorReaderT1;
      }
    };
  };
  var bindReaderT = function(dictBind) {
    var bind16 = bind(dictBind);
    var applyReaderT1 = applyReaderT(dictBind.Apply0());
    return {
      bind: function(v) {
        return function(k) {
          return function(r) {
            return bind16(v(r))(function(a2) {
              var v1 = k(a2);
              return v1(r);
            });
          };
        };
      },
      Apply0: function() {
        return applyReaderT1;
      }
    };
  };
  var applicativeReaderT = function(dictApplicative) {
    var applyReaderT1 = applyReaderT(dictApplicative.Apply0());
    return {
      pure: function() {
        var $154 = pure(dictApplicative);
        return function($155) {
          return ReaderT($$const($154($155)));
        };
      }(),
      Apply0: function() {
        return applyReaderT1;
      }
    };
  };
  var monadReaderT = function(dictMonad) {
    var applicativeReaderT1 = applicativeReaderT(dictMonad.Applicative0());
    var bindReaderT1 = bindReaderT(dictMonad.Bind1());
    return {
      Applicative0: function() {
        return applicativeReaderT1;
      },
      Bind1: function() {
        return bindReaderT1;
      }
    };
  };
  var monadAskReaderT = function(dictMonad) {
    var monadReaderT1 = monadReaderT(dictMonad);
    return {
      ask: pure(dictMonad.Applicative0()),
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };
  var monadThrowReaderT = function(dictMonadThrow) {
    var Monad0 = dictMonadThrow.Monad0();
    var monadReaderT1 = monadReaderT(Monad0);
    return {
      throwError: function() {
        var $166 = lift3(Monad0);
        var $167 = throwError(dictMonadThrow);
        return function($168) {
          return $166($167($168));
        };
      }(),
      Monad0: function() {
        return monadReaderT1;
      }
    };
  };
  var monadErrorReaderT = function(dictMonadError) {
    var catchError2 = catchError(dictMonadError);
    var monadThrowReaderT1 = monadThrowReaderT(dictMonadError.MonadThrow0());
    return {
      catchError: function(v) {
        return function(h) {
          return function(r) {
            return catchError2(v(r))(function(e) {
              var v1 = h(e);
              return v1(r);
            });
          };
        };
      },
      MonadThrow0: function() {
        return monadThrowReaderT1;
      }
    };
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
        return Aff.Bind(aff, function(value15) {
          return Aff.Pure(f(value15));
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
  var _sequential = Aff.Seq;

  // output/Control.Parallel.Class/index.js
  var sequential = function(dict) {
    return dict.sequential;
  };
  var parallel = function(dict) {
    return dict.parallel;
  };

  // output/Control.Parallel/index.js
  var identity6 = /* @__PURE__ */ identity(categoryFn);
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
        return parTraverse_2(dictFoldable)(identity6);
      };
    };
  };

  // output/Effect.Aff/index.js
  var $runtime_lazy7 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var pure4 = /* @__PURE__ */ pure(applicativeEffect);
  var $$void4 = /* @__PURE__ */ $$void(functorEffect);
  var map13 = /* @__PURE__ */ map(functorEffect);
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
  var map14 = /* @__PURE__ */ map(functorAff);
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
  var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy7("applyAff", "Effect.Aff", function() {
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
      return map13(effectCanceler)(v.join(k));
    });
  };
  var functorFiber = {
    map: function(f) {
      return function(t) {
        return unsafePerformEffect(makeFiber(map14(f)(joinFiber(t))));
      };
    }
  };
  var killFiber = function(e) {
    return function(v) {
      return bind1(liftEffect2(v.isSuspended))(function(suspended) {
        if (suspended) {
          return liftEffect2($$void4(v.kill(e, $$const(pure4(unit)))));
        }
        ;
        return makeAff(function(k) {
          return map13(effectCanceler)(v.kill(e, k));
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

  // output/Effect.Aff.Class/index.js
  var monadAffAff = {
    liftAff: /* @__PURE__ */ identity(categoryFn),
    MonadEffect0: function() {
      return monadEffectAff;
    }
  };
  var liftAff = function(dict) {
    return dict.liftAff;
  };

  // output/Halogen.Data.OrdBox/index.js
  var OrdBox = /* @__PURE__ */ function() {
    function OrdBox2(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    OrdBox2.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new OrdBox2(value0, value1, value22);
        };
      };
    };
    return OrdBox2;
  }();
  var mkOrdBox = function(dictOrd) {
    return OrdBox.create(eq(dictOrd.Eq0()))(compare(dictOrd));
  };
  var eqOrdBox = {
    eq: function(v) {
      return function(v1) {
        return v.value0(v.value2)(v1.value2);
      };
    }
  };
  var ordOrdBox = {
    compare: function(v) {
      return function(v1) {
        return v.value1(v.value2)(v1.value2);
      };
    },
    Eq0: function() {
      return eqOrdBox;
    }
  };

  // output/Halogen.Data.Slot/index.js
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordString)(ordOrdBox);
  var pop1 = /* @__PURE__ */ pop(ordTuple2);
  var lookup1 = /* @__PURE__ */ lookup(ordTuple2);
  var insert1 = /* @__PURE__ */ insert(ordTuple2);
  var pop2 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return pop1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var lookup3 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(v) {
              return lookup1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(v);
            };
          };
        };
      };
    };
  };
  var insert3 = function() {
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      return function(dictOrd) {
        var mkOrdBox2 = mkOrdBox(dictOrd);
        return function(sym) {
          return function(key) {
            return function(val) {
              return function(v) {
                return insert1(new Tuple(reflectSymbol2(sym), mkOrdBox2(key)))(val)(v);
              };
            };
          };
        };
      };
    };
  };
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
  var empty6 = empty2;

  // output/Halogen.Query.ChildQuery/index.js
  var ChildQuery = /* @__PURE__ */ function() {
    function ChildQuery3(value0, value1, value22) {
      this.value0 = value0;
      this.value1 = value1;
      this.value2 = value22;
    }
    ;
    ChildQuery3.create = function(value0) {
      return function(value1) {
        return function(value22) {
          return new ChildQuery3(value0, value1, value22);
        };
      };
    };
    return ChildQuery3;
  }();
  var unChildQueryBox = unsafeCoerce2;
  var mkChildQueryBox = unsafeCoerce2;

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
  var bind2 = /* @__PURE__ */ bind(bindEffect);
  var append5 = /* @__PURE__ */ append(semigroupArray);
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
  var create = function __do() {
    var subscribers = $$new([])();
    return {
      emitter: function(k) {
        return function __do2() {
          modify_2(function(v) {
            return append5(v)([k]);
          })(subscribers)();
          return modify_2(deleteBy(unsafeRefEq)(k))(subscribers);
        };
      },
      listener: function(a2) {
        return bind2(read(subscribers))(traverse_1(function(k) {
          return k(a2);
        }));
      }
    };
  };

  // output/Halogen.Query.HalogenM/index.js
  var identity7 = /* @__PURE__ */ identity(categoryFn);
  var lookup4 = /* @__PURE__ */ lookup3();
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
  var raise = function(o) {
    return liftF(new Raise(o, unit));
  };
  var query = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup4(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(q2) {
              return liftF(new ChildQuery2(mkChildQueryBox(new ChildQuery(function(dictApplicative) {
                var pure16 = pure(dictApplicative);
                return function(k) {
                  var $177 = maybe(pure16(Nothing.value))(k);
                  var $178 = lookup23(label5)(p2);
                  return function($179) {
                    return $177($178($179));
                  };
                };
              }, q2, identity7))));
            };
          };
        };
      };
    };
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
  var monadThrowHalogenM = function(dictMonadThrow) {
    return {
      throwError: function() {
        var $184 = throwError(dictMonadThrow);
        return function($185) {
          return HalogenM(liftF(Lift2.create($184($185))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var monadEffectHalogenM = function(dictMonadEffect) {
    return {
      liftEffect: function() {
        var $186 = liftEffect(dictMonadEffect);
        return function($187) {
          return HalogenM(liftF(Lift2.create($186($187))));
        };
      }(),
      Monad0: function() {
        return monadHalogenM;
      }
    };
  };
  var monadAffHalogenM = function(dictMonadAff) {
    var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    return {
      liftAff: function() {
        var $188 = liftAff(dictMonadAff);
        return function($189) {
          return HalogenM(liftF(Lift2.create($188($189))));
        };
      }(),
      MonadEffect0: function() {
        return monadEffectHalogenM1;
      }
    };
  };
  var functorHalogenM = freeFunctor;
  var bindHalogenM = freeBind;
  var applicativeHalogenM = freeApplicative;

  // output/CLI.Halogen.Command/index.js
  var append6 = /* @__PURE__ */ append(semigroupArray);
  var value3 = /* @__PURE__ */ value2(isPropString);
  var map15 = /* @__PURE__ */ map(functorHalogenM);
  var get2 = /* @__PURE__ */ get(monadStateHalogenM);
  var modify_3 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var insert4 = /* @__PURE__ */ insert(ordString);
  var GetInput = /* @__PURE__ */ function() {
    function GetInput3(value0) {
      this.value0 = value0;
    }
    ;
    GetInput3.create = function(value0) {
      return new GetInput3(value0);
    };
    return GetInput3;
  }();
  var UpdateOption = /* @__PURE__ */ function() {
    function UpdateOption2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateOption2.create = function(value0) {
      return new UpdateOption2(value0);
    };
    return UpdateOption2;
  }();
  var UpdateFileSystem = /* @__PURE__ */ function() {
    function UpdateFileSystem2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    UpdateFileSystem2.create = function(value0) {
      return function(value1) {
        return new UpdateFileSystem2(value0, value1);
      };
    };
    return UpdateFileSystem2;
  }();
  var renderFilesystemFieldset = /* @__PURE__ */ function() {
    var $20 = foldlWithIndex(foldableWithIndexMap)(function(filePath) {
      return function(acc) {
        return function(fileContents) {
          return append6(acc)([label([$$for(filePath)])([text(filePath)]), textarea([id2(filePath), rows(10), value3(fileContents)])]);
        };
      };
    })([legend_([text("Filesystem")])]);
    return function($21) {
      return fieldset_($20($21));
    };
  }();
  var renderField = function(name16) {
    return function(value1) {
      return function(toAction) {
        return div_([label([$$for(name16)])([text(name16)]), input([id2(name16), name3(name16), value3(value1), onValueChange(function($22) {
          return UpdateOption.create(toAction($22));
        })])]);
      };
    };
  };
  var handleQuery = function(v) {
    return map15(function($23) {
      return Just.create(v.value0($23));
    })(get2);
  };
  var handleAction = function(handleOptionAction4) {
    return function(v) {
      if (v instanceof UpdateFileSystem) {
        return modify_3(function(st) {
          var $14 = {};
          for (var $15 in st) {
            if ({}.hasOwnProperty.call(st, $15)) {
              $14[$15] = st[$15];
            }
            ;
          }
          ;
          $14.filesystem = insert4(v.value0)(v.value1)(st.filesystem);
          return $14;
        });
      }
      ;
      if (v instanceof UpdateOption) {
        return handleOptionAction4(v.value0);
      }
      ;
      throw new Error("Failed pattern match at CLI.Halogen.Command (line 55, column 35 - line 62, column 36): " + [v.constructor.name]);
    };
  };

  // output/Data.Argonaut.Core/foreign.js
  function id3(x) {
    return x;
  }
  var jsonNull = null;
  function stringifyWithIndent(i2) {
    return function(j) {
      return JSON.stringify(j, null, i2);
    };
  }
  function isArray2(a2) {
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
    } else if (isArray2(a2)) {
      if (isArray2(b2)) {
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
      else if (isArray2(b2))
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

  // output/Data.Argonaut.Core/index.js
  var eq3 = /* @__PURE__ */ eq(eqOrdering);
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
      return id3(singleton7(key)(val));
    };
  };
  var jsonEmptyString = /* @__PURE__ */ id3("");
  var jsonEmptyObject = /* @__PURE__ */ id3(empty3);
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
        return eq3(compare(ordJson)(j1)(j2))(EQ.value);
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
  var toNumber2 = /* @__PURE__ */ toJsonType(caseJsonNumber);
  var caseJsonNull = function(d) {
    return function(f) {
      return function(j) {
        return _caseJson(f, $$const(d), $$const(d), $$const(d), $$const(d), $$const(d), j);
      };
    };
  };
  var isNull2 = /* @__PURE__ */ isJsonType(caseJsonNull);
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

  // output/Data.Array.NonEmpty/index.js
  var unsafeFromArray = NonEmptyArray;
  var toArray2 = function(v) {
    return v;
  };
  var singleton8 = function($110) {
    return unsafeFromArray(singleton4($110));
  };
  var fromFoldable1 = function(dictFoldable1) {
    var $117 = fromFoldable2(dictFoldable1.Foldable0());
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
  var fromFoldable5 = function(dictFoldable) {
    var $119 = fromFoldable2(dictFoldable);
    return function($120) {
      return fromArray($119($120));
    };
  };
  var cons$prime2 = function(x) {
    return function(xs) {
      return unsafeFromArray(cons(x)(xs));
    };
  };
  var fromNonEmpty = function(v) {
    return cons$prime2(v.value0)(v.value1);
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
  var cons3 = function(x) {
    return unsafeAdapt(cons(x));
  };
  var reverse4 = /* @__PURE__ */ unsafeAdapt(reverse);

  // output/Data.Set/index.js
  var coerce3 = /* @__PURE__ */ coerce();
  var foldMap2 = /* @__PURE__ */ foldMap(foldableList);
  var foldl3 = /* @__PURE__ */ foldl(foldableList);
  var foldr5 = /* @__PURE__ */ foldr(foldableList);
  var $$Set = function(x) {
    return x;
  };
  var union3 = function(dictOrd) {
    return coerce3(union(dictOrd));
  };
  var toMap = function(v) {
    return v;
  };
  var toList3 = function(v) {
    return keys(v);
  };
  var toUnfoldable3 = function(dictUnfoldable) {
    var $96 = toUnfoldable(dictUnfoldable);
    return function($97) {
      return $96(toList3($97));
    };
  };
  var singleton9 = function(a2) {
    return singleton3(a2)(unit);
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
  var insert6 = function(dictOrd) {
    var insert13 = insert(dictOrd);
    return function(a2) {
      return function(v) {
        return insert13(a2)(unit)(v);
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
        var $102 = foldr5(f)(x);
        return function($103) {
          return $102(toList3($103));
        };
      };
    }
  };
  var eqSet = function(dictEq) {
    var eq9 = eq(eqMap(dictEq)(eqUnit));
    return {
      eq: function(v) {
        return function(v1) {
          return eq9(v)(v1);
        };
      }
    };
  };
  var ordSet = function(dictOrd) {
    var compare11 = compare(ordList(dictOrd));
    var eqSet1 = eqSet(dictOrd.Eq0());
    return {
      compare: function(s1) {
        return function(s2) {
          return compare11(toList3(s1))(toList3(s2));
        };
      },
      Eq0: function() {
        return eqSet1;
      }
    };
  };
  var empty7 = empty2;
  var fromFoldable6 = function(dictFoldable) {
    var foldl22 = foldl(dictFoldable);
    return function(dictOrd) {
      var insert13 = insert6(dictOrd);
      return foldl22(function(m) {
        return function(a2) {
          return insert13(a2)(m);
        };
      })(empty7);
    };
  };
  var monoidSet = function(dictOrd) {
    var semigroupSet1 = semigroupSet(dictOrd);
    return {
      mempty: empty7,
      Semigroup0: function() {
        return semigroupSet1;
      }
    };
  };
  var difference3 = function(dictOrd) {
    return coerce3(difference(dictOrd));
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

  // output/Data.String.CodePoints/index.js
  var fromEnum2 = /* @__PURE__ */ fromEnum(boundedEnumChar);
  var map16 = /* @__PURE__ */ map(functorMaybe);
  var unfoldr2 = /* @__PURE__ */ unfoldr(unfoldableArray);
  var div3 = /* @__PURE__ */ div(euclideanRingInt);
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
  var uncons6 = function(s) {
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
        tail: drop2(2)(s)
      });
    }
    ;
    return new Just({
      head: cu0,
      tail: drop2(1)(s)
    });
  };
  var unconsButWithTuple = function(s) {
    return map16(function(v) {
      return new Tuple(v.head, v.tail);
    })(uncons6(s));
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
  var length6 = function($74) {
    return length(toCodePointArray($74));
  };
  var fromCharCode2 = /* @__PURE__ */ function() {
    var $75 = toEnumWithDefaults(boundedEnumChar)(bottom(boundedChar))(top(boundedChar));
    return function($76) {
      return singleton6($75($76));
    };
  }();
  var singletonFallback = function(v) {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    ;
    var lead = div3(v - 65536 | 0)(1024) + 55296 | 0;
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
      var v2 = uncons6(v1);
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
        return map16(function(k) {
          return length6(take2(k)(s));
        })(lastIndexOf$prime(p2)(i$prime)(s));
      };
    };
  };
  var splitAt3 = function(i2) {
    return function(s) {
      var before = take4(i2)(s);
      return {
        before,
        after: drop2(length3(before))(s)
      };
    };
  };

  // output/Data.String.NonEmpty.Internal/index.js
  var show2 = /* @__PURE__ */ show(showString);
  var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(bindMaybe);
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
    return composeKleisliFlipped3(fromString)(liftS(stripPrefix(pat)));
  };
  var eqNonEmptyString = eqString;
  var appendString = function(v) {
    return function(s2) {
      return v + s2;
    };
  };

  // output/Data.Argonaut.Encode.Encoders/index.js
  var map17 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable5 = /* @__PURE__ */ toUnfoldable(unfoldableArray);
  var toUnfoldable22 = /* @__PURE__ */ toUnfoldable3(unfoldableList);
  var extend2 = function(encoder) {
    return function(v) {
      var $40 = caseJsonObject(jsonSingletonObject(v.value0)(v.value1))(function() {
        var $42 = insert2(v.value0)(v.value1);
        return function($43) {
          return id3($42($43));
        };
      }());
      return function($41) {
        return $40(encoder($41));
      };
    };
  };
  var encodeString = id3;
  var encodeNumber = id3;
  var encodeMaybe = function(encoder) {
    return function(v) {
      if (v instanceof Nothing) {
        return jsonNull;
      }
      ;
      if (v instanceof Just) {
        return encoder(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Argonaut.Encode.Encoders (line 31, column 23 - line 33, column 22): " + [v.constructor.name]);
    };
  };
  var encodeList = function(encoder) {
    var $45 = map17(encoder);
    return function($46) {
      return id3($45(toUnfoldable5($46)));
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
  var encodeBoolean = id3;
  var assoc = function(encoder) {
    return function(k) {
      var $64 = Tuple.create(k);
      return function($65) {
        return $64(encoder($65));
      };
    };
  };

  // output/Record/index.js
  var get3 = function(dictIsSymbol) {
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
        return empty3;
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
          return id3(gEncodeJson1(rec)($$Proxy.value));
        }
      };
    };
  };
  var encodeJsonJson = {
    encodeJson: /* @__PURE__ */ identity(categoryFn)
  };
  var encodeJsonJString = {
    encodeJson: encodeString
  };
  var encodeJsonJNumber = {
    encodeJson: encodeNumber
  };
  var encodeJsonJBoolean = {
    encodeJson: encodeBoolean
  };
  var encodeJson = function(dict) {
    return dict.encodeJson;
  };
  var encodeJsonList = function(dictEncodeJson) {
    return {
      encodeJson: encodeList(encodeJson(dictEncodeJson))
    };
  };
  var encodeJsonMaybe = function(dictEncodeJson) {
    return {
      encodeJson: encodeMaybe(encodeJson(dictEncodeJson))
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
    var encodeJson15 = encodeJson(dictEncodeJson);
    return function(dictGEncodeJson) {
      var gEncodeJson1 = gEncodeJson(dictGEncodeJson);
      return function(dictIsSymbol) {
        var reflectSymbol2 = reflectSymbol(dictIsSymbol);
        var get6 = get3(dictIsSymbol)();
        return function() {
          return {
            gEncodeJson: function(row) {
              return function(v) {
                return insert2(reflectSymbol2($$Proxy.value))(encodeJson15(get6($$Proxy.value)(row)))(gEncodeJson1(row)($$Proxy.value));
              };
            }
          };
        };
      };
    };
  };

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
  var $runtime_lazy8 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
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
    var $lazy_patchThunk = $runtime_lazy8("patchThunk", "Halogen.VDom.Thunk", function() {
      return function(state3, t2) {
        var $48 = unsafeEqThunk(state3.thunk, t2);
        if ($48) {
          return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
        }
        ;
        var vdom = step(state3.vdom, toVDom(runThunk(t2)));
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
  var map18 = /* @__PURE__ */ map(functorHalogenM);
  var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
  var lookup5 = /* @__PURE__ */ lookup3();
  var pop3 = /* @__PURE__ */ pop2();
  var insert7 = /* @__PURE__ */ insert3();
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
          var $45 = map18(maybe(v.value1(unit))(g));
          return function($46) {
            return $45(args.handleQuery($46));
          };
        })(v.value0);
      }
      ;
      throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
    };
  };
  var mkComponentSlot = unsafeCoerce2;
  var mkComponent = unsafeCoerce2;
  var defaultEval = /* @__PURE__ */ function() {
    return {
      handleAction: $$const(pure5(unit)),
      handleQuery: $$const(pure5(Nothing.value)),
      receive: $$const(Nothing.value),
      initialize: Nothing.value,
      finalize: Nothing.value
    };
  }();
  var componentSlot = function() {
    return function(dictIsSymbol) {
      var lookup13 = lookup5(dictIsSymbol);
      var pop12 = pop3(dictIsSymbol);
      var insert13 = insert7(dictIsSymbol);
      return function(dictOrd) {
        var lookup23 = lookup13(dictOrd);
        var pop22 = pop12(dictOrd);
        var insert22 = insert13(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(comp) {
              return function(input3) {
                return function(output2) {
                  return mkComponentSlot({
                    get: lookup23(label5)(p2),
                    pop: pop22(label5)(p2),
                    set: insert22(label5)(p2),
                    component: comp,
                    input: input3,
                    output: output2
                  });
                };
              };
            };
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
  var length7 = function($30) {
    return length6(fromNonEmptyString($30));
  };
  var splitAt4 = function(i2) {
    return function(nes21) {
      var v = splitAt3(i2)(fromNonEmptyString(nes21));
      return {
        before: fromString(v.before),
        after: fromString(v.after)
      };
    };
  };

  // output/Data.Markdown/index.js
  var map19 = /* @__PURE__ */ map(functorNonEmptyArray);
  var append7 = /* @__PURE__ */ append(semigroupNonEmptyList);
  var append13 = /* @__PURE__ */ append(semigroupString);
  var fromFoldable7 = /* @__PURE__ */ fromFoldable3(foldableArray);
  var appendFoldable2 = /* @__PURE__ */ appendFoldable(foldableList);
  var unfoldr12 = /* @__PURE__ */ unfoldr1(unfoldable1NonEmptyList);
  var map110 = /* @__PURE__ */ map(functorNonEmptyList);
  var foldl4 = /* @__PURE__ */ foldl(foldableNonEmptyArray);
  var defer3 = /* @__PURE__ */ defer(lazyFn);
  var map22 = /* @__PURE__ */ map(functorArray);
  var fromFoldable12 = /* @__PURE__ */ fromFoldable2(foldableNonEmptyList);
  var foldMap12 = /* @__PURE__ */ foldMap1(foldable1NonEmptyArray)(semigroupNonEmptyList);
  var map32 = /* @__PURE__ */ map(functorList);
  var top3 = /* @__PURE__ */ top(boundedInt);
  var append22 = /* @__PURE__ */ append(semigroupList);
  var fromFoldable22 = /* @__PURE__ */ fromFoldable2(foldableList);
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
    var fromFoldable113 = fromFoldable1(dictFoldable1);
    var $674 = List.create(false);
    var $675 = map19(fromFoldable113);
    return function($676) {
      return $674($675(fromFoldable113($676)));
    };
  };
  var text2 = /* @__PURE__ */ function() {
    return Text2.create;
  }();
  var renderRule = /* @__PURE__ */ append7(/* @__PURE__ */ singleton5(""))(/* @__PURE__ */ singleton5("---"));
  var renderLink = function(name16) {
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
      return "[" + (toString2(name16) + ("](" + (urlString + ")")));
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
      var top12 = singleton5("```" + renderCodeLanguage(codeLanguage));
      var codeLines = reverse2(fromFoldable7(code2));
      var bottom5 = append7(singleton5(""))(singleton5("```"));
      return append7(appendFoldable2(bottom5)(codeLines))(top12);
    };
  };
  var paragraph = function(dictFoldable1) {
    var $677 = fromFoldable1(dictFoldable1);
    return function($678) {
      return Paragraph.create($677($678));
    };
  };
  var orderedList = function(dictFoldable1) {
    var fromFoldable113 = fromFoldable1(dictFoldable1);
    var $679 = List.create(true);
    var $680 = map19(fromFoldable113);
    return function($681) {
      return $679($680(fromFoldable113($681)));
    };
  };
  var mapLast = function(f) {
    return function($683) {
      return function(v) {
        var v1 = fromList(v.tail);
        if (v1 instanceof Just) {
          return append7(singleton5(v.head))(mapLast(f)(v1.value0));
        }
        ;
        if (v1 instanceof Nothing) {
          return singleton5(f(v.head));
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 81, column 39 - line 85, column 32): " + [v1.constructor.name]);
      }(uncons2($683));
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
        return cons$prime(f(v.head))(v.tail);
      }(uncons2($685));
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
      var $623 = length7(s) <= maxLineLength;
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
          }(uncons2(renderEmphasis(options2)(node.value0)));
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
          return append7(singleton5(""))(mapFirst(function(v2) {
            return v2 + "\\";
          })(acc));
        }
        ;
        if (node instanceof Text2) {
          var v = uncons2(formatText(options2.maxLineLength)(node.value0));
          var v1 = fromList(v.tail);
          if (v1 instanceof Nothing) {
            return mapFirst(function(v2) {
              return v2 + toString2(v.head);
            })(acc);
          }
          ;
          if (v1 instanceof Just) {
            return append7(map110(toString2)(reverse3(v1.value0)))(mapFirst(function(v2) {
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
    return foldl4(f)(singleton5(""));
  };
  var renderEmphasis = function(options2) {
    return defer3(function(v) {
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
        var formattedLine = joinWith("")(map22(replaceAll("\\")("<br/>"))(fromFoldable12(reverse3(renderedLines))));
        return append7(singleton5(""))(singleton5(prefix + formattedLine));
      };
    };
  };
  var renderParagraph = function(options2) {
    return function(nodes) {
      var renderedLines = renderPhrasingContentNodes(options2)(nodes);
      return append7(singleton5(""))(renderedLines);
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
        var indent = joinWith("")(replicate(length6(itemPrefix))(" "));
        var renderListItem = function(nodes) {
          var renderedLines = foldMap12(renderFlowContentNode(options2))(reverse4(nodes));
          var v = unsnoc2(renderedLines);
          var formattedOtherLines = map32(function(s) {
            var $650 = $$null(s);
            if ($650) {
              return s;
            }
            ;
            return indent + s;
          })(dropWhile($$null)(v.init));
          var formattedFirstLine = itemPrefix + v.last;
          return snoc$prime(new Cons("", formattedOtherLines))(formattedFirstLine);
        };
        return foldMap12(renderListItem)(reverse4(children2));
      };
    };
  };
  var renderFlowContentNode = function(options2) {
    return defer3(function(v) {
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
      var renderedLines = foldMap12(renderFlowContentNode(options2))(reverse4(children2));
      var formatLine = function(s) {
        var $662 = $$null(s);
        if ($662) {
          return ">";
        }
        ;
        return "> " + s;
      };
      var v = unsnoc2(renderedLines);
      var formattedOtherLines = map32(formatLine)(dropWhile($$null)(v.init));
      var formattedFirstLine = formatLine(v.last);
      return snoc$prime(new Cons("", formattedOtherLines))(formattedFirstLine);
    };
  };
  var document = function(dictFoldable) {
    var $718 = map22(FlowContent.create);
    var $719 = fromFoldable2(dictFoldable);
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
        }(uncons($728));
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
          var renderedLines = toList2(renderPhrasingContentNodes(options2)(singleton8(v.value0)));
          var mergedLines = appendWith$prime(flip(append13))(renderedLines)(acc);
          return mergedLines;
        }
        ;
        throw new Error("Failed pattern match at Data.Markdown (line 191, column 11 - line 212, column 20): " + [v.constructor.name]);
      };
    };
    var $729 = joinWith("\n");
    var $730 = dropWhile($$null);
    var $731 = foldl12(f)(Nil.value);
    return function($732) {
      return function(v) {
        return v + "\n";
      }($729(fromFoldable22(reverse2($730($731(fromFoldable7($732)))))));
    };
  };

  // output/Data.Set.NonEmpty/index.js
  var coerce4 = /* @__PURE__ */ coerce();
  var map111 = /* @__PURE__ */ map(functorTuple);
  var foldMap13 = /* @__PURE__ */ foldMap1(foldable1NonEmptyList);
  var foldr12 = /* @__PURE__ */ foldr1(foldable1NonEmptyList);
  var foldl13 = /* @__PURE__ */ foldl1(foldable1NonEmptyList);
  var unionSet = function(dictOrd) {
    return coerce4(append(semigroupSet(dictOrd)));
  };
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
      return map111(stepNext)(v);
    });
    return function($83) {
      return $82(stepHead(toMapIter(toMap(coerce4($83)))));
    };
  };
  var toUnfoldable12 = /* @__PURE__ */ toUnfoldable1(unfoldable1NonEmptyList);
  var toSet = function(v) {
    return v;
  };
  var singleton11 = /* @__PURE__ */ coerce4(singleton9);
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
  var singleton12 = /* @__PURE__ */ singleton2(plusArray);
  var paragraph2 = /* @__PURE__ */ paragraph(foldable1NonEmptyArray);
  var nes3 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "\u2205";
    }
  }));
  var orderedList2 = /* @__PURE__ */ orderedList(foldable1NonEmptyArray);
  var unorderedList2 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var map21 = /* @__PURE__ */ map(functorNonEmptyArray);
  var document2 = function(dict) {
    return dict.document;
  };
  var documentFoldable = function(dictDocument) {
    var document12 = document2(dictDocument);
    return function(dictFoldable) {
      var fromFoldable111 = fromFoldable5(dictFoldable);
      return function(isOrdered) {
        var $60 = maybe(paragraph2(singleton8(text2(nes3($$Proxy.value)))))(function() {
          var $62 = function() {
            if (isOrdered) {
              return orderedList2;
            }
            ;
            return unorderedList2;
          }();
          var $63 = map21(function($65) {
            return fromNonEmpty(document12($65));
          });
          return function($64) {
            return $62($63($64));
          };
        }());
        return function($61) {
          return singleton12($60(fromFoldable111($61)));
        };
      };
    };
  };
  var documentSet = function(dictDocument) {
    return {
      document: documentFoldable(dictDocument)(foldableSet)(false)
    };
  };
  var documentNonEmptySet = function(dictDocument) {
    return {
      document: function() {
        var $68 = document2(documentSet(dictDocument));
        return function($69) {
          return $68(toSet($69));
        };
      }()
    };
  };

  // output/JsonValue/index.js
  var ordJsonValue = ordJson;
  var encodeJsonJsonValue = encodeJsonJson;

  // output/JsonSchema/index.js
  var eq13 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqNumber));
  var eq22 = /* @__PURE__ */ eq(/* @__PURE__ */ eqSet(eqString));
  var compare2 = /* @__PURE__ */ compare(ordBoolean);
  var compare13 = /* @__PURE__ */ compare(/* @__PURE__ */ ordMaybe(ordNumber));
  var compare22 = /* @__PURE__ */ compare(/* @__PURE__ */ ordSet(ordString));
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
  var map23 = /* @__PURE__ */ map(functorArray);
  var toUnfoldable6 = /* @__PURE__ */ toUnfoldable3(unfoldableArray);
  var append8 = /* @__PURE__ */ append(semigroupArray);
  var fromFoldable8 = /* @__PURE__ */ fromFoldable4(foldableArray);
  var map112 = /* @__PURE__ */ map(functorTuple);
  var unwrap3 = /* @__PURE__ */ unwrap();
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
  var eq32 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(/* @__PURE__ */ eqSet(eqJsonValueType)));
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
  var compare3 = /* @__PURE__ */ compare(/* @__PURE__ */ ordMaybe(/* @__PURE__ */ ordSet(ordJsonValueType)));
  var eqJsonSchema = {
    eq: function(x) {
      return function(y) {
        if (x instanceof BooleanSchema && y instanceof BooleanSchema) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof ObjectSchema && y instanceof ObjectSchema) {
          return eq13(x.value0.exclusiveMaximum)(y.value0.exclusiveMaximum) && eq13(x.value0.exclusiveMinimum)(y.value0.exclusiveMinimum) && eq(eqMaybe(eqJsonSchema))(x.value0.items)(y.value0.items) && eq13(x.value0.maximum)(y.value0.maximum) && eq13(x.value0.minimum)(y.value0.minimum) && eq13(x.value0.multipleOf)(y.value0.multipleOf) && eq(eqMaybe(eqJsonSchema))(x.value0.not)(y.value0.not) && eq22(x.value0.required)(y.value0.required) && eq32(x.value0.typeKeyword)(y.value0.typeKeyword) && x.value0.uniqueItems === y.value0.uniqueItems;
        }
        ;
        return false;
      };
    }
  };
  var ordJsonSchema = {
    compare: function(x) {
      return function(y) {
        if (x instanceof BooleanSchema && y instanceof BooleanSchema) {
          return compare2(x.value0)(y.value0);
        }
        ;
        if (x instanceof BooleanSchema) {
          return LT.value;
        }
        ;
        if (y instanceof BooleanSchema) {
          return GT.value;
        }
        ;
        if (x instanceof ObjectSchema && y instanceof ObjectSchema) {
          var v = compare13(x.value0.exclusiveMaximum)(y.value0.exclusiveMaximum);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          var v1 = compare13(x.value0.exclusiveMinimum)(y.value0.exclusiveMinimum);
          if (v1 instanceof LT) {
            return LT.value;
          }
          ;
          if (v1 instanceof GT) {
            return GT.value;
          }
          ;
          var v2 = compare(ordMaybe(ordJsonSchema))(x.value0.items)(y.value0.items);
          if (v2 instanceof LT) {
            return LT.value;
          }
          ;
          if (v2 instanceof GT) {
            return GT.value;
          }
          ;
          var v3 = compare13(x.value0.maximum)(y.value0.maximum);
          if (v3 instanceof LT) {
            return LT.value;
          }
          ;
          if (v3 instanceof GT) {
            return GT.value;
          }
          ;
          var v4 = compare13(x.value0.minimum)(y.value0.minimum);
          if (v4 instanceof LT) {
            return LT.value;
          }
          ;
          if (v4 instanceof GT) {
            return GT.value;
          }
          ;
          var v5 = compare13(x.value0.multipleOf)(y.value0.multipleOf);
          if (v5 instanceof LT) {
            return LT.value;
          }
          ;
          if (v5 instanceof GT) {
            return GT.value;
          }
          ;
          var v6 = compare(ordMaybe(ordJsonSchema))(x.value0.not)(y.value0.not);
          if (v6 instanceof LT) {
            return LT.value;
          }
          ;
          if (v6 instanceof GT) {
            return GT.value;
          }
          ;
          var v7 = compare22(x.value0.required)(y.value0.required);
          if (v7 instanceof LT) {
            return LT.value;
          }
          ;
          if (v7 instanceof GT) {
            return GT.value;
          }
          ;
          var v8 = compare3(x.value0.typeKeyword)(y.value0.typeKeyword);
          if (v8 instanceof LT) {
            return LT.value;
          }
          ;
          if (v8 instanceof GT) {
            return GT.value;
          }
          ;
          return compare2(x.value0.uniqueItems)(y.value0.uniqueItems);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqJsonSchema;
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
      return id3(toString2(renderJsonValueType($239)));
    }
  };
  var printRequiredKeywordSpec = /* @__PURE__ */ function() {
    var $240 = map23(id3);
    return function($241) {
      return wrap2(id3($240(toUnfoldable6($241))));
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
  var printOptionalNumber = function(name16) {
    return function(v) {
      if (v instanceof Just) {
        return [new Tuple(name16, wrap2(id3(v.value0)))];
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
      return [new Tuple("multipleOf", wrap2(id3(v.value0)))];
    }
    ;
    if (v instanceof Nothing) {
      return [];
    }
    ;
    throw new Error("Failed pattern match at JsonSchema (line 158, column 19 - line 161, column 15): " + [v.constructor.name]);
  };
  var printJsonValueType = function($242) {
    return id3(function(v) {
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
    var $243 = map23(printJsonValueType);
    return function($244) {
      return wrap2(id3($243(toUnfoldable6($244))));
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
      required: empty7,
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
    return [new Tuple("uniqueItems", wrap2(id3(bool)))];
  };
  var printObjectSchema = function(keywords) {
    var printNot = function(mbSchema) {
      return maybe([])(function($245) {
        return singleton4(function(v) {
          return new Tuple("not", v);
        }(print2($245)));
      })(mbSchema);
    };
    var printItems = function(mbSchema) {
      return maybe([])(function($246) {
        return singleton4(function(v) {
          return new Tuple("items", v);
        }(print2($246)));
      })(mbSchema);
    };
    var printKeywords = append8(printOptionalNumber("exclusiveMaximum")(keywords.exclusiveMaximum))(append8(printOptionalNumber("exclusiveMinimum")(keywords.exclusiveMinimum))(append8(printItems(keywords.items))(append8(printOptionalNumber("maximum")(keywords.maximum))(append8(printOptionalNumber("minimum")(keywords.minimum))(append8(printMultipleOf(keywords.multipleOf))(append8(printNot(keywords.not))(append8(printRequired(keywords.required))(append8(printTypeKeyword(keywords.typeKeyword))(printUniqueItems(keywords.uniqueItems))))))))));
    return wrap2(id3(fromFoldable8(map23(map112(unwrap3))(printKeywords))));
  };
  var print2 = function(v) {
    if (v instanceof BooleanSchema) {
      return wrap2(id3(v.value0));
    }
    ;
    if (v instanceof ObjectSchema) {
      return printObjectSchema(v.value0);
    }
    ;
    throw new Error("Failed pattern match at JsonSchema (line 114, column 9 - line 118, column 31): " + [v.constructor.name]);
  };
  var encodeJsonJsonSchema = {
    encodeJson: /* @__PURE__ */ function() {
      var $249 = encodeJson(encodeJsonJsonValue);
      return function($250) {
        return $249(print2($250));
      };
    }()
  };

  // output/CLI.Halogen.CompatOptions/index.js
  var fromFoldable9 = /* @__PURE__ */ fromFoldable(ordString)(foldableArray);
  var encodeJson2 = /* @__PURE__ */ encodeJson(encodeJsonJsonValue);
  var modify_4 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var UpdateLeftSchemaFilePath = /* @__PURE__ */ function() {
    function UpdateLeftSchemaFilePath3(value0) {
      this.value0 = value0;
    }
    ;
    UpdateLeftSchemaFilePath3.create = function(value0) {
      return new UpdateLeftSchemaFilePath3(value0);
    };
    return UpdateLeftSchemaFilePath3;
  }();
  var UpdateRightSchemaFilePath = /* @__PURE__ */ function() {
    function UpdateRightSchemaFilePath3(value0) {
      this.value0 = value0;
    }
    ;
    UpdateRightSchemaFilePath3.create = function(value0) {
      return new UpdateRightSchemaFilePath3(value0);
    };
    return UpdateRightSchemaFilePath3;
  }();
  var render3 = function(v) {
    return div_([fieldset_([legend_([text("Command Options")]), renderField("left schema file path")(v.options.leftSchemaFilePath)(UpdateLeftSchemaFilePath.create), renderField("right schema file path")(v.options.rightSchemaFilePath)(UpdateRightSchemaFilePath.create)]), renderFilesystemFieldset(v.filesystem)]);
  };
  var initialState = function(v) {
    return {
      filesystem: fromFoldable9([new Tuple("schemata/left.json", stringifyWithIndent(2)(encodeJson2(print2(new ObjectSchema({
        exclusiveMaximum: defaultKeywords.exclusiveMaximum,
        exclusiveMinimum: defaultKeywords.exclusiveMinimum,
        items: defaultKeywords.items,
        maximum: defaultKeywords.maximum,
        minimum: defaultKeywords.minimum,
        multipleOf: defaultKeywords.multipleOf,
        not: defaultKeywords.not,
        required: defaultKeywords.required,
        typeKeyword: new Just(singleton9(JsonInteger.value)),
        uniqueItems: defaultKeywords.uniqueItems
      }))))), new Tuple("schemata/right.json", stringifyWithIndent(2)(encodeJson2(print2(new ObjectSchema({
        exclusiveMaximum: defaultKeywords.exclusiveMaximum,
        exclusiveMinimum: defaultKeywords.exclusiveMinimum,
        items: defaultKeywords.items,
        maximum: defaultKeywords.maximum,
        minimum: defaultKeywords.minimum,
        multipleOf: defaultKeywords.multipleOf,
        not: defaultKeywords.not,
        required: defaultKeywords.required,
        typeKeyword: new Just(singleton9(JsonString.value)),
        uniqueItems: defaultKeywords.uniqueItems
      })))))]),
      options: {
        leftSchemaFilePath: "schemata/left.json",
        rightSchemaFilePath: "schemata/right.json"
      }
    };
  };
  var handleOptionAction = function(v) {
    if (v instanceof UpdateLeftSchemaFilePath) {
      return modify_4(function(st) {
        var $21 = {};
        for (var $22 in st) {
          if ({}.hasOwnProperty.call(st, $22)) {
            $21[$22] = st[$22];
          }
          ;
        }
        ;
        $21.options = function() {
          var $18 = {};
          for (var $19 in st.options) {
            if ({}.hasOwnProperty.call(st.options, $19)) {
              $18[$19] = st["options"][$19];
            }
            ;
          }
          ;
          $18.leftSchemaFilePath = v.value0;
          return $18;
        }();
        return $21;
      });
    }
    ;
    if (v instanceof UpdateRightSchemaFilePath) {
      return modify_4(function(st) {
        var $28 = {};
        for (var $29 in st) {
          if ({}.hasOwnProperty.call(st, $29)) {
            $28[$29] = st[$29];
          }
          ;
        }
        ;
        $28.options = function() {
          var $25 = {};
          for (var $26 in st.options) {
            if ({}.hasOwnProperty.call(st.options, $26)) {
              $25[$26] = st["options"][$26];
            }
            ;
          }
          ;
          $25.rightSchemaFilePath = v.value0;
          return $25;
        }();
        return $28;
      });
    }
    ;
    throw new Error("Failed pattern match at CLI.Halogen.CompatOptions (line 75, column 22 - line 85, column 8): " + [v.constructor.name]);
  };
  var component = /* @__PURE__ */ function() {
    return mkComponent({
      "eval": mkEval({
        handleAction: handleAction(handleOptionAction),
        handleQuery,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      }),
      initialState,
      render: render3
    });
  }();

  // output/CLI.Halogen.DiffOptions/index.js
  var fromFoldable10 = /* @__PURE__ */ fromFoldable(ordString)(foldableArray);
  var encodeJson3 = /* @__PURE__ */ encodeJson(encodeJsonJsonValue);
  var modify_5 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var UpdateLeftSchemaFilePath2 = /* @__PURE__ */ function() {
    function UpdateLeftSchemaFilePath3(value0) {
      this.value0 = value0;
    }
    ;
    UpdateLeftSchemaFilePath3.create = function(value0) {
      return new UpdateLeftSchemaFilePath3(value0);
    };
    return UpdateLeftSchemaFilePath3;
  }();
  var UpdateRightSchemaFilePath2 = /* @__PURE__ */ function() {
    function UpdateRightSchemaFilePath3(value0) {
      this.value0 = value0;
    }
    ;
    UpdateRightSchemaFilePath3.create = function(value0) {
      return new UpdateRightSchemaFilePath3(value0);
    };
    return UpdateRightSchemaFilePath3;
  }();
  var render4 = function(v) {
    return div_([fieldset_([legend_([text("Command Options")]), renderField("left schema file path")(v.options.leftSchemaFilePath)(UpdateLeftSchemaFilePath2.create), renderField("right schema file path")(v.options.rightSchemaFilePath)(UpdateRightSchemaFilePath2.create)]), renderFilesystemFieldset(v.filesystem)]);
  };
  var initialState2 = function(v) {
    return {
      filesystem: fromFoldable10([new Tuple("schemata/left.json", stringifyWithIndent(2)(encodeJson3(print2(new ObjectSchema({
        exclusiveMaximum: defaultKeywords.exclusiveMaximum,
        exclusiveMinimum: defaultKeywords.exclusiveMinimum,
        items: defaultKeywords.items,
        maximum: defaultKeywords.maximum,
        minimum: defaultKeywords.minimum,
        multipleOf: defaultKeywords.multipleOf,
        not: defaultKeywords.not,
        required: defaultKeywords.required,
        typeKeyword: new Just(singleton9(JsonInteger.value)),
        uniqueItems: defaultKeywords.uniqueItems
      }))))), new Tuple("schemata/right.json", stringifyWithIndent(2)(encodeJson3(print2(new ObjectSchema({
        exclusiveMaximum: defaultKeywords.exclusiveMaximum,
        exclusiveMinimum: defaultKeywords.exclusiveMinimum,
        items: defaultKeywords.items,
        maximum: defaultKeywords.maximum,
        minimum: defaultKeywords.minimum,
        multipleOf: defaultKeywords.multipleOf,
        not: defaultKeywords.not,
        required: defaultKeywords.required,
        typeKeyword: new Just(singleton9(JsonString.value)),
        uniqueItems: defaultKeywords.uniqueItems
      })))))]),
      options: {
        leftSchemaFilePath: "schemata/left.json",
        rightSchemaFilePath: "schemata/right.json"
      }
    };
  };
  var handleOptionAction2 = function(v) {
    if (v instanceof UpdateLeftSchemaFilePath2) {
      return modify_5(function(st) {
        var $21 = {};
        for (var $22 in st) {
          if ({}.hasOwnProperty.call(st, $22)) {
            $21[$22] = st[$22];
          }
          ;
        }
        ;
        $21.options = function() {
          var $18 = {};
          for (var $19 in st.options) {
            if ({}.hasOwnProperty.call(st.options, $19)) {
              $18[$19] = st["options"][$19];
            }
            ;
          }
          ;
          $18.leftSchemaFilePath = v.value0;
          return $18;
        }();
        return $21;
      });
    }
    ;
    if (v instanceof UpdateRightSchemaFilePath2) {
      return modify_5(function(st) {
        var $28 = {};
        for (var $29 in st) {
          if ({}.hasOwnProperty.call(st, $29)) {
            $28[$29] = st[$29];
          }
          ;
        }
        ;
        $28.options = function() {
          var $25 = {};
          for (var $26 in st.options) {
            if ({}.hasOwnProperty.call(st.options, $26)) {
              $25[$26] = st["options"][$26];
            }
            ;
          }
          ;
          $25.rightSchemaFilePath = v.value0;
          return $25;
        }();
        return $28;
      });
    }
    ;
    throw new Error("Failed pattern match at CLI.Halogen.DiffOptions (line 75, column 22 - line 85, column 8): " + [v.constructor.name]);
  };
  var component2 = /* @__PURE__ */ function() {
    return mkComponent({
      "eval": mkEval({
        handleAction: handleAction(handleOptionAction2),
        handleQuery,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      }),
      initialState: initialState2,
      render: render4
    });
  }();

  // output/CLI.Program/index.js
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
  var eqOutputFormat = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Json2 && y instanceof Json2) {
          return true;
        }
        ;
        if (x instanceof Markdown && y instanceof Markdown) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var unexpectedError = function(message2) {
    return {
      exitCode: 2,
      stderr: message2,
      stdout: ""
    };
  };
  var successfulOutput = function(renderOutput) {
    return function(output2) {
      return {
        exitCode: 0,
        stderr: "",
        stdout: renderOutput(output2)
      };
    };
  };
  var readFileContent = function(dict) {
    return dict.readFileContent;
  };
  var expectedError = function(renderOutput) {
    return function(output2) {
      return {
        exitCode: 1,
        stderr: "",
        stdout: renderOutput(output2)
      };
    };
  };

  // output/CLI.Halogen.OutputFormat/index.js
  var type_4 = /* @__PURE__ */ type_3(isPropInputType);
  var value4 = /* @__PURE__ */ value2(isPropString);
  var eq4 = /* @__PURE__ */ eq(eqOutputFormat);
  var bind3 = /* @__PURE__ */ bind(bindHalogenM);
  var get4 = /* @__PURE__ */ get(monadStateHalogenM);
  var pure6 = /* @__PURE__ */ pure(applicativeHalogenM);
  var modify_6 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var GetOutputFormat = /* @__PURE__ */ function() {
    function GetOutputFormat2(value0) {
      this.value0 = value0;
    }
    ;
    GetOutputFormat2.create = function(value0) {
      return new GetOutputFormat2(value0);
    };
    return GetOutputFormat2;
  }();
  var OutputFormatUpdated = /* @__PURE__ */ function() {
    function OutputFormatUpdated2(value0) {
      this.value0 = value0;
    }
    ;
    OutputFormatUpdated2.create = function(value0) {
      return new OutputFormatUpdated2(value0);
    };
    return OutputFormatUpdated2;
  }();
  var showOutputFormat = function(v) {
    if (v instanceof Json2) {
      return "JSON";
    }
    ;
    if (v instanceof Markdown) {
      return "Markdown";
    }
    ;
    throw new Error("Failed pattern match at CLI.Halogen.OutputFormat (line 66, column 20 - line 70, column 15): " + [v.constructor.name]);
  };
  var render5 = function(v) {
    var renderOutputFormatFieldFor = function(format) {
      return div_([label([$$for(showOutputFormat(format))])([text(showOutputFormat(format))]), input([type_4(InputRadio.value), name3("outputFormat"), id2(showOutputFormat(format)), value4(showOutputFormat(format)), checked2(eq4(format)(v.outputFormat)), onClick($$const(new OutputFormatUpdated(format)))])]);
    };
    return fieldset_([legend_([text("Output Format")]), renderOutputFormatFieldFor(Json2.value), renderOutputFormatFieldFor(Markdown.value)]);
  };
  var handleQuery2 = function(v) {
    return bind3(get4)(function(state3) {
      return pure6(new Just(v.value0(state3.outputFormat)));
    });
  };
  var handleAction2 = function(v) {
    return modify_6(function(st) {
      var $19 = {};
      for (var $20 in st) {
        if ({}.hasOwnProperty.call(st, $20)) {
          $19[$20] = st[$20];
        }
        ;
      }
      ;
      $19.outputFormat = v.value0;
      return $19;
    });
  };
  var component3 = /* @__PURE__ */ function() {
    return mkComponent({
      "eval": mkEval({
        handleAction: handleAction2,
        handleQuery: handleQuery2,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      }),
      initialState: $$const({
        outputFormat: Json2.value
      }),
      render: render5
    });
  }();

  // output/CLI.Halogen.ValidateOptions/index.js
  var fromFoldable11 = /* @__PURE__ */ fromFoldable(ordString)(foldableArray);
  var encodeJson4 = /* @__PURE__ */ encodeJson(encodeJsonJson);
  var encodeJson1 = /* @__PURE__ */ encodeJson(encodeJsonJsonValue);
  var modify_7 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var UpdateJsonFilePath = /* @__PURE__ */ function() {
    function UpdateJsonFilePath2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateJsonFilePath2.create = function(value0) {
      return new UpdateJsonFilePath2(value0);
    };
    return UpdateJsonFilePath2;
  }();
  var UpdateSchemaFilePath = /* @__PURE__ */ function() {
    function UpdateSchemaFilePath2(value0) {
      this.value0 = value0;
    }
    ;
    UpdateSchemaFilePath2.create = function(value0) {
      return new UpdateSchemaFilePath2(value0);
    };
    return UpdateSchemaFilePath2;
  }();
  var render6 = function(v) {
    return div_([fieldset_([legend_([text("Command Options")]), renderField("schema file path")(v.options.schemaFilePath)(UpdateSchemaFilePath.create), renderField("json file path")(v.options.jsonFilePath)(UpdateJsonFilePath.create)]), renderFilesystemFieldset(v.filesystem)]);
  };
  var initialState3 = function(v) {
    return {
      filesystem: fromFoldable11([new Tuple("value.json", stringifyWithIndent(2)(encodeJson4(id3(123)))), new Tuple("schema.json", stringifyWithIndent(2)(encodeJson1(print2(new ObjectSchema({
        exclusiveMaximum: defaultKeywords.exclusiveMaximum,
        exclusiveMinimum: defaultKeywords.exclusiveMinimum,
        items: defaultKeywords.items,
        maximum: defaultKeywords.maximum,
        minimum: defaultKeywords.minimum,
        multipleOf: defaultKeywords.multipleOf,
        not: defaultKeywords.not,
        required: defaultKeywords.required,
        typeKeyword: new Just(singleton9(JsonString.value)),
        uniqueItems: defaultKeywords.uniqueItems
      })))))]),
      options: {
        jsonFilePath: "value.json",
        schemaFilePath: "schema.json"
      }
    };
  };
  var handleOptionAction3 = function(v) {
    if (v instanceof UpdateJsonFilePath) {
      return modify_7(function(st) {
        var $21 = {};
        for (var $22 in st) {
          if ({}.hasOwnProperty.call(st, $22)) {
            $21[$22] = st[$22];
          }
          ;
        }
        ;
        $21.options = function() {
          var $18 = {};
          for (var $19 in st.options) {
            if ({}.hasOwnProperty.call(st.options, $19)) {
              $18[$19] = st["options"][$19];
            }
            ;
          }
          ;
          $18.jsonFilePath = v.value0;
          return $18;
        }();
        return $21;
      });
    }
    ;
    if (v instanceof UpdateSchemaFilePath) {
      return modify_7(function(st) {
        var $28 = {};
        for (var $29 in st) {
          if ({}.hasOwnProperty.call(st, $29)) {
            $28[$29] = st[$29];
          }
          ;
        }
        ;
        $28.options = function() {
          var $25 = {};
          for (var $26 in st.options) {
            if ({}.hasOwnProperty.call(st.options, $26)) {
              $25[$26] = st["options"][$26];
            }
            ;
          }
          ;
          $25.schemaFilePath = v.value0;
          return $25;
        }();
        return $28;
      });
    }
    ;
    throw new Error("Failed pattern match at CLI.Halogen.ValidateOptions (line 69, column 22 - line 79, column 8): " + [v.constructor.name]);
  };
  var component4 = /* @__PURE__ */ function() {
    return mkComponent({
      "eval": mkEval({
        handleAction: handleAction(handleOptionAction3),
        handleQuery,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      }),
      initialState: initialState3,
      render: render6
    });
  }();

  // output/Halogen.HTML/index.js
  var componentSlot2 = /* @__PURE__ */ componentSlot();
  var slot_ = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component7) {
              return function(input3) {
                return widget(new ComponentSlot(componentSlot22(label5)(p2)(component7)(input3)($$const(Nothing.value))));
              };
            };
          };
        };
      };
    };
  };
  var slot = function() {
    return function(dictIsSymbol) {
      var componentSlot1 = componentSlot2(dictIsSymbol);
      return function(dictOrd) {
        var componentSlot22 = componentSlot1(dictOrd);
        return function(label5) {
          return function(p2) {
            return function(component7) {
              return function(input3) {
                return function(outputQuery) {
                  return widget(new ComponentSlot(componentSlot22(label5)(p2)(component7)(input3)(function($11) {
                    return Just.create(outputQuery($11));
                  })));
                };
              };
            };
          };
        };
      };
    };
  };
  var fromPlainHTML = unsafeCoerce2;

  // output/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value15) {
    var tag = Object.prototype.toString.call(value15);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value15);
    } else {
      return nothing;
    }
  }

  // output/Web.HTML.HTMLElement/index.js
  var toNode2 = unsafeCoerce2;
  var fromElement = function(x) {
    return _read(Nothing.value, Just.create, x);
  };

  // output/Halogen.Query/index.js
  var query2 = /* @__PURE__ */ query();
  var identity8 = /* @__PURE__ */ identity(categoryFn);
  var request = function() {
    return function(dictIsSymbol) {
      var query1 = query2(dictIsSymbol);
      return function(dictOrd) {
        var query22 = query1(dictOrd);
        return function(slot3) {
          return function(label5) {
            return function(req) {
              return query22(slot3)(label5)(req(identity8));
            };
          };
        };
      };
    };
  };

  // output/CLI.Command/index.js
  var document3 = /* @__PURE__ */ document(/* @__PURE__ */ foldableNonEmpty(foldableArray));
  var commandProgram = function(dictApplicative) {
    var pure16 = pure(dictApplicative);
    return function(dictDocument) {
      var document12 = document2(dictDocument);
      return function(dictEncodeJson) {
        var encodeJson8 = encodeJson(dictEncodeJson);
        return function(dictMonadError) {
          var catchError2 = catchError(dictMonadError);
          var bindFlipped9 = bindFlipped(dictMonadError.MonadThrow0().Monad0().Bind1());
          return function(program5) {
            return function(outputFormat) {
              return function(options2) {
                var renderOutput = function() {
                  if (outputFormat instanceof Json2) {
                    var $16 = stringifyWithIndent(2);
                    return function($17) {
                      return $16(encodeJson8($17));
                    };
                  }
                  ;
                  if (outputFormat instanceof Markdown) {
                    var $18 = render2({
                      maxLineLength: 72
                    });
                    return function($19) {
                      return $18(document3(document12($19)));
                    };
                  }
                  ;
                  throw new Error("Failed pattern match at CLI.Command (line 45, column 18 - line 51, column 30): " + [outputFormat.constructor.name]);
                }();
                var handleProgramOutput = function() {
                  var $20 = either(expectedError(renderOutput))(successfulOutput(renderOutput));
                  return function($21) {
                    return pure16($20($21));
                  };
                }();
                var fallback = function($22) {
                  return pure16(unexpectedError(message($22)));
                };
                return catchError2(bindFlipped9(handleProgramOutput)(program5(options2)))(fallback);
              };
            };
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

  // output/JsonSchema.Codec.Parsing/index.js
  var bind4 = /* @__PURE__ */ bind(bindEither);
  var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeEither);
  var pure7 = /* @__PURE__ */ pure(applicativeEither);
  var fromFoldable13 = /* @__PURE__ */ fromFoldable6(foldableArray);
  var fromFoldable14 = /* @__PURE__ */ fromFoldable13(ordString);
  var map25 = /* @__PURE__ */ map(functorEither);
  var fromFoldable23 = /* @__PURE__ */ fromFoldable13(ordJsonValueType);
  var alt3 = /* @__PURE__ */ alt(altEither);
  var unwrap4 = /* @__PURE__ */ unwrap();
  var map113 = /* @__PURE__ */ map(functorMaybe);
  var wrap3 = /* @__PURE__ */ wrap();
  var parsingErrorMessage = function(reason) {
    return "Invalid schema: " + reason;
  };
  var parseRequiredKeywordSpec = function(specJson) {
    return bind4(note("Property names are not an array.")(toArray(specJson)))(function(propertyNameJsons) {
      return bind4(traverse2(function() {
        var $45 = note("Property name is not a string.");
        return function($46) {
          return $45(toString($46));
        };
      }())(propertyNameJsons))(function(propertyNames) {
        return pure7(fromFoldable14(propertyNames));
      });
    });
  };
  var parseOptionalNumber = function(name16) {
    var $47 = lookup2(name16);
    return function($48) {
      return function(v) {
        if (v instanceof Just) {
          var v1 = toNumber2(v.value0);
          if (v1 instanceof Just) {
            return new Right(new Just(v1.value0));
          }
          ;
          if (v1 instanceof Nothing) {
            return new Left(name16 + " is not a number.");
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
    var $49 = lookup2("multipleOf");
    return function($50) {
      return function(v) {
        if (v instanceof Just) {
          var $28 = isNull2(v.value0);
          if ($28) {
            return new Right(defaultKeywords.multipleOf);
          }
          ;
          var v1 = toNumber2(v.value0);
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
      var $51 = map25(singleton9);
      return function($52) {
        return $51(parseJsonValueType($52));
      };
    }();
    var parseArraySpec = function(json) {
      return bind4(note("Types are not an array.")(toArray(json)))(function(typeJsons) {
        return bind4(traverse2(parseJsonValueType)(typeJsons))(function(types) {
          return pure7(fromFoldable23(types));
        });
      });
    };
    return alt3(parseStringSpec(specJson))(parseArraySpec(specJson));
  };
  var parseBooleanSchema = function(json) {
    return bind4(note(parsingErrorMessage("the JSON value is not a JSON boolean"))(toBoolean(unwrap4(json))))(function(bool) {
      return pure7(new BooleanSchema(bool));
    });
  };
  var parseSchema = function(json) {
    return alt3(parseBooleanSchema(json))(alt3(parseObjectSchema(json))(new Left("the JSON value is neither a boolean nor an object")));
  };
  var parseObjectSchema = function(keywordsJson) {
    return bind4(note(parsingErrorMessage("the JSON value is not a JSON object"))(toObject(unwrap4(keywordsJson))))(function(schemaObject) {
      return bind4(parseOptionalNumber("exclusiveMaximum")(schemaObject))(function(exclusiveMaximum) {
        return bind4(parseOptionalNumber("exclusiveMinimum")(schemaObject))(function(exclusiveMinimum) {
          return bind4(maybe(new Right(defaultKeywords.items))(function() {
            var $53 = map25(Just.create);
            return function($54) {
              return $53(parseSchema($54));
            };
          }())(map113(wrap3)(lookup2("items")(schemaObject))))(function(items2) {
            return bind4(parseOptionalNumber("maximum")(schemaObject))(function(maximum2) {
              return bind4(parseOptionalNumber("minimum")(schemaObject))(function(minimum2) {
                return bind4(parseMultipleOf(schemaObject))(function(multipleOf) {
                  return bind4(maybe(new Right(defaultKeywords.not))(function() {
                    var $55 = map25(Just.create);
                    return function($56) {
                      return $55(parseSchema($56));
                    };
                  }())(map113(wrap3)(lookup2("not")(schemaObject))))(function(not3) {
                    return bind4(maybe(new Right(defaultKeywords.required))(parseRequiredKeywordSpec)(lookup2("required")(schemaObject)))(function(required4) {
                      return bind4(function() {
                        var v = lookup2("type")(schemaObject);
                        if (v instanceof Just) {
                          return map25(Just.create)(parseTypeKeywordSpec(v.value0));
                        }
                        ;
                        if (v instanceof Nothing) {
                          return new Right(defaultKeywords.typeKeyword);
                        }
                        ;
                        throw new Error("Failed pattern match at JsonSchema.Codec.Parsing (line 69, column 17 - line 73, column 47): " + [v.constructor.name]);
                      }())(function(typeKeyword) {
                        return bind4(maybe(new Right(defaultKeywords.uniqueItems))(function(json) {
                          var $44 = isNull2(json);
                          if ($44) {
                            return new Right(defaultKeywords.uniqueItems);
                          }
                          ;
                          return note("Unique items is not a boolean.")(toBoolean(json));
                        })(lookup2("uniqueItems")(schemaObject)))(function(uniqueItems) {
                          return pure7(new ObjectSchema({
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

  // output/JsonSchema.SchemaPath/index.js
  var eq5 = /* @__PURE__ */ eq(eqNonEmptyString);
  var compare4 = /* @__PURE__ */ compare(ordNonEmptyString);
  var show3 = /* @__PURE__ */ show(showNonEmptyString);
  var append9 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes7 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "exclusiveMinimum";
    }
  }));
  var nes12 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "exclusiveMaximum";
    }
  }));
  var nes23 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "items";
    }
  }));
  var nes33 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "maximum";
    }
  }));
  var nes43 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
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
          return eq5(x.value0)(y.value0);
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
        return id3("exclusiveMinimum");
      }
      ;
      if (v instanceof ExclusiveMaximum) {
        return id3("exclusiveMaximum");
      }
      ;
      if (v instanceof Items) {
        return id3("items");
      }
      ;
      if (v instanceof Maximum) {
        return id3("maximum");
      }
      ;
      if (v instanceof Minimum) {
        return id3("minimum");
      }
      ;
      if (v instanceof MultipleOf) {
        return id3("multipleOf");
      }
      ;
      if (v instanceof Properties) {
        return id3(show3(v.value0));
      }
      ;
      if (v instanceof TypeKeyword) {
        return id3("type");
      }
      ;
      if (v instanceof UniqueItems) {
        return id3("uniqueItems");
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.SchemaPath (line 62, column 16 - line 80, column 33): " + [v.constructor.name]);
    }
  };
  var render7 = /* @__PURE__ */ function() {
    var f = function(acc) {
      return function($151) {
        return function(v) {
          return append9(appendString(acc)("/"))(v);
        }(function(v) {
          if (v instanceof ExclusiveMinimum) {
            return nes7($$Proxy.value);
          }
          ;
          if (v instanceof ExclusiveMaximum) {
            return nes12($$Proxy.value);
          }
          ;
          if (v instanceof Items) {
            return nes23($$Proxy.value);
          }
          ;
          if (v instanceof Maximum) {
            return nes33($$Proxy.value);
          }
          ;
          if (v instanceof Minimum) {
            return nes43($$Proxy.value);
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
      return $152(reverse2($153));
    };
  }();

  // output/JsonSchema.Difference/index.js
  var exclusiveMaximumIsSymbol = {
    reflectSymbol: function() {
      return "exclusiveMaximum";
    }
  };
  var exclusiveMinimumIsSymbol = {
    reflectSymbol: function() {
      return "exclusiveMinimum";
    }
  };
  var itemsIsSymbol = {
    reflectSymbol: function() {
      return "items";
    }
  };
  var maximumIsSymbol = {
    reflectSymbol: function() {
      return "maximum";
    }
  };
  var minimumIsSymbol = {
    reflectSymbol: function() {
      return "minimum";
    }
  };
  var multipleOfIsSymbol = {
    reflectSymbol: function() {
      return "multipleOf";
    }
  };
  var notIsSymbol = {
    reflectSymbol: function() {
      return "not";
    }
  };
  var requiredIsSymbol = {
    reflectSymbol: function() {
      return "required";
    }
  };
  var typeKeywordIsSymbol = {
    reflectSymbol: function() {
      return "typeKeyword";
    }
  };
  var uniqueItemsIsSymbol = {
    reflectSymbol: function() {
      return "uniqueItems";
    }
  };
  var differenceTypeIsSymbol = {
    reflectSymbol: function() {
      return "differenceType";
    }
  };
  var pathIsSymbol = {
    reflectSymbol: function() {
      return "path";
    }
  };
  var eq14 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqNumber));
  var eq23 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(eqJsonSchema));
  var eq33 = /* @__PURE__ */ eq(/* @__PURE__ */ eqSet(eqString));
  var eq42 = /* @__PURE__ */ eq(/* @__PURE__ */ eqMaybe(/* @__PURE__ */ eqSet(eqJsonValueType)));
  var compare5 = /* @__PURE__ */ compare(ordBoolean);
  var compare14 = /* @__PURE__ */ compare(/* @__PURE__ */ ordMaybe(ordNumber));
  var compare23 = /* @__PURE__ */ compare(/* @__PURE__ */ ordMaybe(ordJsonSchema));
  var compare32 = /* @__PURE__ */ compare(/* @__PURE__ */ ordSet(ordString));
  var compare42 = /* @__PURE__ */ compare(/* @__PURE__ */ ordMaybe(/* @__PURE__ */ ordSet(ordJsonValueType)));
  var eq52 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqSchemaPathSegment));
  var compare52 = /* @__PURE__ */ compare(/* @__PURE__ */ ordList(ordSchemaPathSegment));
  var show4 = /* @__PURE__ */ show(showBoolean);
  var show12 = /* @__PURE__ */ show(showNumber);
  var extend4 = /* @__PURE__ */ extend3(encodeJsonJson);
  var assoc3 = /* @__PURE__ */ assoc2(encodeJsonJson);
  var gEncodeJsonCons2 = /* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonMaybe(encodeJsonJNumber));
  var gEncodeJsonCons1 = /* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonMaybe(encodeJsonJsonSchema));
  var encodeJsonMaybe2 = /* @__PURE__ */ encodeJsonMaybe(/* @__PURE__ */ encodeSet2(ordJsonValueType)(encodeJsonJsonValueType));
  var encodeJson5 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons1(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons2(/* @__PURE__ */ gEncodeJsonCons1(/* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeSet2(ordString)(encodeJsonJString))(/* @__PURE__ */ gEncodeJsonCons(encodeJsonMaybe2)(/* @__PURE__ */ gEncodeJsonCons(encodeJsonJBoolean)(gEncodeJsonNil)(uniqueItemsIsSymbol)())(typeKeywordIsSymbol)())(requiredIsSymbol)())(notIsSymbol)())(multipleOfIsSymbol)())(minimumIsSymbol)())(maximumIsSymbol)())(itemsIsSymbol)())(exclusiveMinimumIsSymbol)())(exclusiveMaximumIsSymbol)())());
  var encodeJson12 = /* @__PURE__ */ encodeJson(encodeJsonMaybe2);
  var nes8 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "unspecified";
    }
  }));
  var paragraph3 = /* @__PURE__ */ paragraph(foldable1NonEmptyArray);
  var unorderedList3 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var map26 = /* @__PURE__ */ map(functorNonEmptyArray);
  var map114 = /* @__PURE__ */ map(functorFn);
  var nes13 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "none";
    }
  }));
  var singleton13 = /* @__PURE__ */ singleton2(plusArray);
  var nes24 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of boolean schema from allow-all to reject-all";
    }
  }));
  var nes34 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of boolean schema from reject-all to allow-all";
    }
  }));
  var append14 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes44 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of exclusiveMaximum from ";
    }
  }));
  var nes53 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " to ";
    }
  }));
  var nes63 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of exclusiveMinimum from ";
    }
  }));
  var nes73 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of maximum from ";
    }
  }));
  var nes82 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of minimum from ";
    }
  }));
  var nes9 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of multipleOf from ";
    }
  }));
  var nes10 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of boolean schema to object schema";
    }
  }));
  var nes11 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of object schema to boolean schema";
    }
  }));
  var nes122 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "change of accepted JSON value types from";
    }
  }));
  var nes132 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "to";
    }
  }));
  var nes14 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "JSON schema path: ";
    }
  }));
  var fromFoldable15 = /* @__PURE__ */ fromFoldable2(/* @__PURE__ */ foldableNonEmpty(foldableArray));
  var BooleanSchemaChange = /* @__PURE__ */ function() {
    function BooleanSchemaChange2(value0) {
      this.value0 = value0;
    }
    ;
    BooleanSchemaChange2.create = function(value0) {
      return new BooleanSchemaChange2(value0);
    };
    return BooleanSchemaChange2;
  }();
  var ExclusiveMaximumChange = /* @__PURE__ */ function() {
    function ExclusiveMaximumChange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ExclusiveMaximumChange2.create = function(value0) {
      return function(value1) {
        return new ExclusiveMaximumChange2(value0, value1);
      };
    };
    return ExclusiveMaximumChange2;
  }();
  var ExclusiveMinimumChange = /* @__PURE__ */ function() {
    function ExclusiveMinimumChange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    ExclusiveMinimumChange2.create = function(value0) {
      return function(value1) {
        return new ExclusiveMinimumChange2(value0, value1);
      };
    };
    return ExclusiveMinimumChange2;
  }();
  var MaximumChange = /* @__PURE__ */ function() {
    function MaximumChange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MaximumChange2.create = function(value0) {
      return function(value1) {
        return new MaximumChange2(value0, value1);
      };
    };
    return MaximumChange2;
  }();
  var MinimumChange = /* @__PURE__ */ function() {
    function MinimumChange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MinimumChange2.create = function(value0) {
      return function(value1) {
        return new MinimumChange2(value0, value1);
      };
    };
    return MinimumChange2;
  }();
  var MultipleOfChange = /* @__PURE__ */ function() {
    function MultipleOfChange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    MultipleOfChange2.create = function(value0) {
      return function(value1) {
        return new MultipleOfChange2(value0, value1);
      };
    };
    return MultipleOfChange2;
  }();
  var SchemaChangeFromBooleanToObject = /* @__PURE__ */ function() {
    function SchemaChangeFromBooleanToObject2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SchemaChangeFromBooleanToObject2.create = function(value0) {
      return function(value1) {
        return new SchemaChangeFromBooleanToObject2(value0, value1);
      };
    };
    return SchemaChangeFromBooleanToObject2;
  }();
  var SchemaChangeFromObjectToBoolean = /* @__PURE__ */ function() {
    function SchemaChangeFromObjectToBoolean2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    SchemaChangeFromObjectToBoolean2.create = function(value0) {
      return function(value1) {
        return new SchemaChangeFromObjectToBoolean2(value0, value1);
      };
    };
    return SchemaChangeFromObjectToBoolean2;
  }();
  var TypeChange = /* @__PURE__ */ function() {
    function TypeChange2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    TypeChange2.create = function(value0) {
      return function(value1) {
        return new TypeChange2(value0, value1);
      };
    };
    return TypeChange2;
  }();
  var eqDifferenceType = {
    eq: function(x) {
      return function(y) {
        if (x instanceof BooleanSchemaChange && y instanceof BooleanSchemaChange) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof ExclusiveMaximumChange && y instanceof ExclusiveMaximumChange) {
          return eq14(x.value0)(y.value0) && eq14(x.value1)(y.value1);
        }
        ;
        if (x instanceof ExclusiveMinimumChange && y instanceof ExclusiveMinimumChange) {
          return eq14(x.value0)(y.value0) && eq14(x.value1)(y.value1);
        }
        ;
        if (x instanceof MaximumChange && y instanceof MaximumChange) {
          return eq14(x.value0)(y.value0) && eq14(x.value1)(y.value1);
        }
        ;
        if (x instanceof MinimumChange && y instanceof MinimumChange) {
          return eq14(x.value0)(y.value0) && eq14(x.value1)(y.value1);
        }
        ;
        if (x instanceof MultipleOfChange && y instanceof MultipleOfChange) {
          return eq14(x.value0)(y.value0) && eq14(x.value1)(y.value1);
        }
        ;
        if (x instanceof SchemaChangeFromBooleanToObject && y instanceof SchemaChangeFromBooleanToObject) {
          return x.value0 === y.value0 && (eq14(x.value1.exclusiveMaximum)(y.value1.exclusiveMaximum) && eq14(x.value1.exclusiveMinimum)(y.value1.exclusiveMinimum) && eq23(x.value1.items)(y.value1.items) && eq14(x.value1.maximum)(y.value1.maximum) && eq14(x.value1.minimum)(y.value1.minimum) && eq14(x.value1.multipleOf)(y.value1.multipleOf) && eq23(x.value1.not)(y.value1.not) && eq33(x.value1.required)(y.value1.required) && eq42(x.value1.typeKeyword)(y.value1.typeKeyword) && x.value1.uniqueItems === y.value1.uniqueItems);
        }
        ;
        if (x instanceof SchemaChangeFromObjectToBoolean && y instanceof SchemaChangeFromObjectToBoolean) {
          return eq14(x.value0.exclusiveMaximum)(y.value0.exclusiveMaximum) && eq14(x.value0.exclusiveMinimum)(y.value0.exclusiveMinimum) && eq23(x.value0.items)(y.value0.items) && eq14(x.value0.maximum)(y.value0.maximum) && eq14(x.value0.minimum)(y.value0.minimum) && eq14(x.value0.multipleOf)(y.value0.multipleOf) && eq23(x.value0.not)(y.value0.not) && eq33(x.value0.required)(y.value0.required) && eq42(x.value0.typeKeyword)(y.value0.typeKeyword) && x.value0.uniqueItems === y.value0.uniqueItems && x.value1 === y.value1;
        }
        ;
        if (x instanceof TypeChange && y instanceof TypeChange) {
          return eq42(x.value0)(y.value0) && eq42(x.value1)(y.value1);
        }
        ;
        return false;
      };
    }
  };
  var eq6 = /* @__PURE__ */ eq(eqDifferenceType);
  var ordDifferenceType = {
    compare: function(x) {
      return function(y) {
        if (x instanceof BooleanSchemaChange && y instanceof BooleanSchemaChange) {
          return compare5(x.value0)(y.value0);
        }
        ;
        if (x instanceof BooleanSchemaChange) {
          return LT.value;
        }
        ;
        if (y instanceof BooleanSchemaChange) {
          return GT.value;
        }
        ;
        if (x instanceof ExclusiveMaximumChange && y instanceof ExclusiveMaximumChange) {
          var v = compare14(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare14(x.value1)(y.value1);
        }
        ;
        if (x instanceof ExclusiveMaximumChange) {
          return LT.value;
        }
        ;
        if (y instanceof ExclusiveMaximumChange) {
          return GT.value;
        }
        ;
        if (x instanceof ExclusiveMinimumChange && y instanceof ExclusiveMinimumChange) {
          var v = compare14(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare14(x.value1)(y.value1);
        }
        ;
        if (x instanceof ExclusiveMinimumChange) {
          return LT.value;
        }
        ;
        if (y instanceof ExclusiveMinimumChange) {
          return GT.value;
        }
        ;
        if (x instanceof MaximumChange && y instanceof MaximumChange) {
          var v = compare14(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare14(x.value1)(y.value1);
        }
        ;
        if (x instanceof MaximumChange) {
          return LT.value;
        }
        ;
        if (y instanceof MaximumChange) {
          return GT.value;
        }
        ;
        if (x instanceof MinimumChange && y instanceof MinimumChange) {
          var v = compare14(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare14(x.value1)(y.value1);
        }
        ;
        if (x instanceof MinimumChange) {
          return LT.value;
        }
        ;
        if (y instanceof MinimumChange) {
          return GT.value;
        }
        ;
        if (x instanceof MultipleOfChange && y instanceof MultipleOfChange) {
          var v = compare14(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare14(x.value1)(y.value1);
        }
        ;
        if (x instanceof MultipleOfChange) {
          return LT.value;
        }
        ;
        if (y instanceof MultipleOfChange) {
          return GT.value;
        }
        ;
        if (x instanceof SchemaChangeFromBooleanToObject && y instanceof SchemaChangeFromBooleanToObject) {
          var v = compare5(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          var v1 = compare14(x.value1.exclusiveMaximum)(y.value1.exclusiveMaximum);
          if (v1 instanceof LT) {
            return LT.value;
          }
          ;
          if (v1 instanceof GT) {
            return GT.value;
          }
          ;
          var v2 = compare14(x.value1.exclusiveMinimum)(y.value1.exclusiveMinimum);
          if (v2 instanceof LT) {
            return LT.value;
          }
          ;
          if (v2 instanceof GT) {
            return GT.value;
          }
          ;
          var v3 = compare23(x.value1.items)(y.value1.items);
          if (v3 instanceof LT) {
            return LT.value;
          }
          ;
          if (v3 instanceof GT) {
            return GT.value;
          }
          ;
          var v4 = compare14(x.value1.maximum)(y.value1.maximum);
          if (v4 instanceof LT) {
            return LT.value;
          }
          ;
          if (v4 instanceof GT) {
            return GT.value;
          }
          ;
          var v5 = compare14(x.value1.minimum)(y.value1.minimum);
          if (v5 instanceof LT) {
            return LT.value;
          }
          ;
          if (v5 instanceof GT) {
            return GT.value;
          }
          ;
          var v6 = compare14(x.value1.multipleOf)(y.value1.multipleOf);
          if (v6 instanceof LT) {
            return LT.value;
          }
          ;
          if (v6 instanceof GT) {
            return GT.value;
          }
          ;
          var v7 = compare23(x.value1.not)(y.value1.not);
          if (v7 instanceof LT) {
            return LT.value;
          }
          ;
          if (v7 instanceof GT) {
            return GT.value;
          }
          ;
          var v8 = compare32(x.value1.required)(y.value1.required);
          if (v8 instanceof LT) {
            return LT.value;
          }
          ;
          if (v8 instanceof GT) {
            return GT.value;
          }
          ;
          var v9 = compare42(x.value1.typeKeyword)(y.value1.typeKeyword);
          if (v9 instanceof LT) {
            return LT.value;
          }
          ;
          if (v9 instanceof GT) {
            return GT.value;
          }
          ;
          return compare5(x.value1.uniqueItems)(y.value1.uniqueItems);
        }
        ;
        if (x instanceof SchemaChangeFromBooleanToObject) {
          return LT.value;
        }
        ;
        if (y instanceof SchemaChangeFromBooleanToObject) {
          return GT.value;
        }
        ;
        if (x instanceof SchemaChangeFromObjectToBoolean && y instanceof SchemaChangeFromObjectToBoolean) {
          var v = function() {
            var v12 = compare14(x.value0.exclusiveMaximum)(y.value0.exclusiveMaximum);
            if (v12 instanceof LT) {
              return LT.value;
            }
            ;
            if (v12 instanceof GT) {
              return GT.value;
            }
            ;
            var v22 = compare14(x.value0.exclusiveMinimum)(y.value0.exclusiveMinimum);
            if (v22 instanceof LT) {
              return LT.value;
            }
            ;
            if (v22 instanceof GT) {
              return GT.value;
            }
            ;
            var v32 = compare23(x.value0.items)(y.value0.items);
            if (v32 instanceof LT) {
              return LT.value;
            }
            ;
            if (v32 instanceof GT) {
              return GT.value;
            }
            ;
            var v42 = compare14(x.value0.maximum)(y.value0.maximum);
            if (v42 instanceof LT) {
              return LT.value;
            }
            ;
            if (v42 instanceof GT) {
              return GT.value;
            }
            ;
            var v52 = compare14(x.value0.minimum)(y.value0.minimum);
            if (v52 instanceof LT) {
              return LT.value;
            }
            ;
            if (v52 instanceof GT) {
              return GT.value;
            }
            ;
            var v62 = compare14(x.value0.multipleOf)(y.value0.multipleOf);
            if (v62 instanceof LT) {
              return LT.value;
            }
            ;
            if (v62 instanceof GT) {
              return GT.value;
            }
            ;
            var v72 = compare23(x.value0.not)(y.value0.not);
            if (v72 instanceof LT) {
              return LT.value;
            }
            ;
            if (v72 instanceof GT) {
              return GT.value;
            }
            ;
            var v82 = compare32(x.value0.required)(y.value0.required);
            if (v82 instanceof LT) {
              return LT.value;
            }
            ;
            if (v82 instanceof GT) {
              return GT.value;
            }
            ;
            var v92 = compare42(x.value0.typeKeyword)(y.value0.typeKeyword);
            if (v92 instanceof LT) {
              return LT.value;
            }
            ;
            if (v92 instanceof GT) {
              return GT.value;
            }
            ;
            return compare5(x.value0.uniqueItems)(y.value0.uniqueItems);
          }();
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare5(x.value1)(y.value1);
        }
        ;
        if (x instanceof SchemaChangeFromObjectToBoolean) {
          return LT.value;
        }
        ;
        if (y instanceof SchemaChangeFromObjectToBoolean) {
          return GT.value;
        }
        ;
        if (x instanceof TypeChange && y instanceof TypeChange) {
          var v = compare42(x.value0)(y.value0);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare42(x.value1)(y.value1);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Difference (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqDifferenceType;
    }
  };
  var compare6 = /* @__PURE__ */ compare(ordDifferenceType);
  var eqDifference = {
    eq: function(x) {
      return function(y) {
        return eq6(x.differenceType)(y.differenceType) && eq52(x.path)(y.path);
      };
    }
  };
  var ordDifference = {
    compare: function(x) {
      return function(y) {
        var v = compare6(x.differenceType)(y.differenceType);
        if (v instanceof LT) {
          return LT.value;
        }
        ;
        if (v instanceof GT) {
          return GT.value;
        }
        ;
        return compare52(x.path)(y.path);
      };
    },
    Eq0: function() {
      return eqDifference;
    }
  };
  var append23 = /* @__PURE__ */ append(/* @__PURE__ */ semigroupSet(ordDifference));
  var foldMap4 = /* @__PURE__ */ foldMap(foldableArray)(/* @__PURE__ */ monoidSet(ordDifference));
  var fromFoldable16 = /* @__PURE__ */ fromFoldable6(foldableMaybe)(ordDifference);
  var encodeJsonDifferenceType = {
    encodeJson: function(v) {
      if (v instanceof BooleanSchemaChange) {
        return id3("boolean schema changed to " + show4(v.value0));
      }
      ;
      if (v instanceof ExclusiveMaximumChange) {
        return id3("exclusive maximum value change from " + (maybe("not set")(show12)(v.value0) + (" to " + maybe("not set")(show12)(v.value1))));
      }
      ;
      if (v instanceof ExclusiveMinimumChange) {
        return id3("exclusive minimum value change from " + (maybe("not set")(show12)(v.value0) + (" to " + maybe("not set")(show12)(v.value1))));
      }
      ;
      if (v instanceof MaximumChange) {
        return id3("maximum value change from " + (maybe("not set")(show12)(v.value0) + (" to " + maybe("not set")(show12)(v.value1))));
      }
      ;
      if (v instanceof MinimumChange) {
        return id3("minimum value change from " + (maybe("not set")(show12)(v.value0) + (" to " + maybe("not set")(show12)(v.value1))));
      }
      ;
      if (v instanceof MultipleOfChange) {
        return id3("multiple of change from " + (maybe("not set")(show12)(v.value0) + (" to " + maybe("not set")(show12)(v.value1))));
      }
      ;
      if (v instanceof SchemaChangeFromBooleanToObject) {
        return extend4(assoc3("newObjectSchema")(encodeJson5(v.value1)))(extend4(assoc3("oldBooleanSchema")(id3(v.value0)))(jsonEmptyObject));
      }
      ;
      if (v instanceof SchemaChangeFromObjectToBoolean) {
        return extend4(assoc3("newSchema")(id3("boolean schema of " + show4(v.value1))))(extend4(assoc3("oldObjectSchema")(encodeJson5(v.value0)))(jsonEmptyObject));
      }
      ;
      if (v instanceof TypeChange) {
        return extend4(assoc3("newAcceptableTypes")(encodeJson12(v.value1)))(extend4(assoc3("oldAcceptableTypes")(encodeJson12(v.value0)))(jsonEmptyObject));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Difference (line 72, column 16 - line 118, column 29): " + [v.constructor.name]);
    }
  };
  var encodeJsonDifference = /* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons(encodeJsonDifferenceType)(/* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonList(encodeJsonSchemaPathSegme))(gEncodeJsonNil)(pathIsSymbol)())(differenceTypeIsSymbol)())();
  var renderOptionalNumber = /* @__PURE__ */ function() {
    return maybe(nes8($$Proxy.value))(show1(show1Number));
  }();
  var renderJsonValueType2 = function($688) {
    return paragraph3(singleton8(text2(renderJsonValueType($688))));
  };
  var renderJsonValueTypes = /* @__PURE__ */ function() {
    var toFlowContentNode = function() {
      var $689 = fromFoldable5(foldableSet);
      return function($690) {
        return function(v) {
          if (v instanceof Just) {
            return unorderedList3(map26(map114(singleton8)(renderJsonValueType2))(v.value0));
          }
          ;
          if (v instanceof Nothing) {
            return paragraph3(singleton8(text2(nes13($$Proxy.value))));
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.Difference (line 393, column 48 - line 402, column 35): " + [v.constructor.name]);
        }($689($690));
      };
    }();
    return maybe(paragraph3(singleton8(text2(nes8($$Proxy.value)))))(toFlowContentNode);
  }();
  var documentDifferenceType = {
    document: function(v) {
      if (v instanceof BooleanSchemaChange && !v.value0) {
        return singleton13(paragraph3(singleton8(text2(nes24($$Proxy.value)))));
      }
      ;
      if (v instanceof BooleanSchemaChange && v.value0) {
        return singleton13(paragraph3(singleton8(text2(nes34($$Proxy.value)))));
      }
      ;
      if (v instanceof ExclusiveMaximumChange) {
        return singleton13(paragraph3(singleton8(text2(append14(nes44($$Proxy.value))(append14(renderOptionalNumber(v.value0))(append14(nes53($$Proxy.value))(renderOptionalNumber(v.value1))))))));
      }
      ;
      if (v instanceof ExclusiveMinimumChange) {
        return singleton13(paragraph3(singleton8(text2(append14(nes63($$Proxy.value))(append14(renderOptionalNumber(v.value0))(append14(nes53($$Proxy.value))(renderOptionalNumber(v.value1))))))));
      }
      ;
      if (v instanceof MaximumChange) {
        return singleton13(paragraph3(singleton8(text2(append14(nes73($$Proxy.value))(append14(renderOptionalNumber(v.value0))(append14(nes53($$Proxy.value))(renderOptionalNumber(v.value1))))))));
      }
      ;
      if (v instanceof MinimumChange) {
        return singleton13(paragraph3(singleton8(text2(append14(nes82($$Proxy.value))(append14(renderOptionalNumber(v.value0))(append14(nes53($$Proxy.value))(renderOptionalNumber(v.value1))))))));
      }
      ;
      if (v instanceof MultipleOfChange) {
        return singleton13(paragraph3(singleton8(text2(append14(nes9($$Proxy.value))(append14(renderOptionalNumber(v.value0))(append14(nes53($$Proxy.value))(renderOptionalNumber(v.value1))))))));
      }
      ;
      if (v instanceof SchemaChangeFromBooleanToObject) {
        return singleton13(paragraph3(singleton8(text2(nes10($$Proxy.value)))));
      }
      ;
      if (v instanceof SchemaChangeFromObjectToBoolean) {
        return singleton13(paragraph3(singleton8(text2(nes11($$Proxy.value)))));
      }
      ;
      if (v instanceof TypeChange) {
        return new NonEmpty(paragraph3(singleton8(text2(nes122($$Proxy.value)))), [renderJsonValueTypes(v.value0), paragraph3(singleton8(text2(nes132($$Proxy.value)))), renderJsonValueTypes(v.value1)]);
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Difference (line 124, column 14 - line 230, column 10): " + [v.constructor.name]);
    }
  };
  var document4 = /* @__PURE__ */ document2(documentDifferenceType);
  var documentDifference = {
    document: function(v) {
      return new NonEmpty(paragraph3(singleton8(text2(append14(nes14($$Proxy.value))(render7(v.path))))), fromFoldable15(document4(v.differenceType)));
    }
  };
  var calculateTypeKeywordDiff = function(path) {
    return function(previousKeywords) {
      return function(nextKeywords) {
        var $670 = eq42(previousKeywords.typeKeyword)(nextKeywords.typeKeyword);
        if ($670) {
          return empty7;
        }
        ;
        return singleton9({
          differenceType: new TypeChange(previousKeywords.typeKeyword, nextKeywords.typeKeyword),
          path: new Cons(TypeKeyword.value, path)
        });
      };
    };
  };
  var calculateRangeDiff = function(path) {
    return function(previousKeywords) {
      return function(nextKeywords) {
        var minimumDiff = function() {
          var $671 = eq14(previousKeywords.minimum)(nextKeywords.minimum);
          if ($671) {
            return empty7;
          }
          ;
          return singleton9({
            differenceType: new MinimumChange(previousKeywords.minimum, nextKeywords.minimum),
            path: new Cons(Minimum.value, path)
          });
        }();
        var maximumDiff = function() {
          var $672 = eq14(previousKeywords.maximum)(nextKeywords.maximum);
          if ($672) {
            return empty7;
          }
          ;
          return singleton9({
            differenceType: new MaximumChange(previousKeywords.maximum, nextKeywords.maximum),
            path: new Cons(Maximum.value, path)
          });
        }();
        var exclusiveMinimumDiff = function() {
          var $673 = eq14(previousKeywords.exclusiveMinimum)(nextKeywords.exclusiveMinimum);
          if ($673) {
            return empty7;
          }
          ;
          return singleton9({
            differenceType: new ExclusiveMinimumChange(previousKeywords.exclusiveMinimum, nextKeywords.exclusiveMinimum),
            path: new Cons(ExclusiveMinimum.value, path)
          });
        }();
        var exclusiveMaximumDiff = function() {
          var $674 = eq14(previousKeywords.exclusiveMaximum)(nextKeywords.exclusiveMaximum);
          if ($674) {
            return empty7;
          }
          ;
          return singleton9({
            differenceType: new ExclusiveMaximumChange(previousKeywords.exclusiveMaximum, nextKeywords.exclusiveMaximum),
            path: new Cons(ExclusiveMaximum.value, path)
          });
        }();
        return append23(exclusiveMaximumDiff)(append23(exclusiveMinimumDiff)(append23(maximumDiff)(minimumDiff)));
      };
    };
  };
  var calculateMultipleOfDiff = function(path) {
    return function(previousKeywords) {
      return function(nextKeywords) {
        var $675 = eq14(previousKeywords.multipleOf)(nextKeywords.multipleOf);
        if ($675) {
          return empty7;
        }
        ;
        return singleton9({
          differenceType: new MultipleOfChange(previousKeywords.multipleOf, nextKeywords.multipleOf),
          path: new Cons(MultipleOf.value, path)
        });
      };
    };
  };
  var calculate = /* @__PURE__ */ function() {
    var calculateObjectSchemataDiff = function(path) {
      return function(previousKeywords) {
        return function(nextKeywords) {
          return foldMap4(function(f) {
            return f(path)(previousKeywords)(nextKeywords);
          })([calculateMultipleOfDiff, calculateRangeDiff, calculateTypeKeywordDiff]);
        };
      };
    };
    var calculateObjectAndBooleanSchemataDiff = function(path) {
      return function(keywords) {
        return function(bool) {
          return {
            differenceType: new SchemaChangeFromObjectToBoolean(keywords, bool),
            path
          };
        };
      };
    };
    var calculateBooleanSchemataDiff = function(path) {
      return function(v) {
        return function(v1) {
          if (!v && v1) {
            return new Just({
              differenceType: new BooleanSchemaChange(true),
              path
            });
          }
          ;
          if (v && !v1) {
            return new Just({
              differenceType: new BooleanSchemaChange(false),
              path
            });
          }
          ;
          return Nothing.value;
        };
      };
    };
    var calculateBooleanAndObjectSchemataDiff = function(path) {
      return function(bool) {
        return function(keywords) {
          return {
            differenceType: new SchemaChangeFromBooleanToObject(bool, keywords),
            path
          };
        };
      };
    };
    var go2 = function(path) {
      return function(previousSchema) {
        return function(nextSchema) {
          if (previousSchema instanceof BooleanSchema && nextSchema instanceof BooleanSchema) {
            return fromFoldable16(calculateBooleanSchemataDiff(path)(previousSchema.value0)(nextSchema.value0));
          }
          ;
          if (previousSchema instanceof BooleanSchema && nextSchema instanceof ObjectSchema) {
            return singleton9(calculateBooleanAndObjectSchemataDiff(path)(previousSchema.value0)(nextSchema.value0));
          }
          ;
          if (previousSchema instanceof ObjectSchema && nextSchema instanceof BooleanSchema) {
            return singleton9(calculateObjectAndBooleanSchemataDiff(path)(previousSchema.value0)(nextSchema.value0));
          }
          ;
          if (previousSchema instanceof ObjectSchema && nextSchema instanceof ObjectSchema) {
            return calculateObjectSchemataDiff(path)(previousSchema.value0)(nextSchema.value0);
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.Difference (line 236, column 39 - line 253, column 69): " + [previousSchema.constructor.name, nextSchema.constructor.name]);
        };
      };
    };
    return go2(Nil.value);
  }();

  // output/JsonSchema.Range/index.js
  var compare7 = /* @__PURE__ */ compare(ordNumber);
  var show5 = /* @__PURE__ */ show(showNumber);
  var nes15 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "]";
    }
  }));
  var nes16 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return ")";
    }
  }));
  var nes25 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "[";
    }
  }));
  var nes35 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "(";
    }
  }));
  var append15 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes45 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
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
          return compare7(x.value0)(y.value0);
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
          return compare7(x.value0)(y.value0);
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
        return id3(show5(v.value0) + " (inclusively)");
      }
      ;
      if (v instanceof Open) {
        return id3(show5(v.value0) + " (exclusively)");
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Range (line 42, column 16 - line 46, column 48): " + [v.constructor.name]);
    }
  };
  var renderRange = function(range3) {
    var renderTo = function() {
      if (range3.to instanceof Closed) {
        return prependString(show5(range3.to.value0))(nes15($$Proxy.value));
      }
      ;
      if (range3.to instanceof Open) {
        return prependString(show5(range3.to.value0))(nes16($$Proxy.value));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Range (line 29, column 14 - line 33, column 71): " + [range3.to.constructor.name]);
    }();
    var renderFrom = function() {
      if (range3.from instanceof Closed) {
        return appendString(nes25($$Proxy.value))(show5(range3.from.value0));
      }
      ;
      if (range3.from instanceof Open) {
        return appendString(nes35($$Proxy.value))(show5(range3.from.value0));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Range (line 22, column 16 - line 26, column 70): " + [range3.from.constructor.name]);
    }();
    return inlineCode(append15(renderFrom)(append15(nes45($$Proxy.value))(renderTo)));
  };

  // output/Utils/index.js
  var isInteger = function(x) {
    return toNumber(trunc2(x)) === x;
  };

  // output/JsonSchema.Compatibility/index.js
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
  var newIsSymbol = {
    reflectSymbol: function() {
      return "new";
    }
  };
  var oldIsSymbol = {
    reflectSymbol: function() {
      return "old";
    }
  };
  var incompatibilityTypeIsSymbol = {
    reflectSymbol: function() {
      return "incompatibilityType";
    }
  };
  var pathIsSymbol2 = {
    reflectSymbol: function() {
      return "path";
    }
  };
  var eq7 = /* @__PURE__ */ eq(eqBoundary);
  var compare8 = /* @__PURE__ */ compare(ordBoundary);
  var eq24 = /* @__PURE__ */ eq(/* @__PURE__ */ eqNonEmptySet(eqJsonValueType));
  var compare15 = /* @__PURE__ */ compare(ordNumber);
  var compare24 = /* @__PURE__ */ compare(/* @__PURE__ */ ordNonEmptySet(ordJsonValueType));
  var ordRecord2 = /* @__PURE__ */ ordRecord();
  var ordRecordCons2 = /* @__PURE__ */ ordRecordCons(/* @__PURE__ */ ordRecordCons(ordRecordNil)()(pathIsSymbol2)(/* @__PURE__ */ ordList(ordSchemaPathSegment)))()(incompatibilityTypeIsSymbol);
  var extend5 = /* @__PURE__ */ extend3(encodeJsonJson);
  var assoc4 = /* @__PURE__ */ assoc2(encodeJsonJson);
  var gEncodeJsonCons3 = /* @__PURE__ */ gEncodeJsonCons(encodeJsonBoundary);
  var encodeJson6 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons3(/* @__PURE__ */ gEncodeJsonCons3(gEncodeJsonNil)(toIsSymbol)())(fromIsSymbol)())());
  var gEncodeJsonCons12 = /* @__PURE__ */ gEncodeJsonCons(encodeJsonJNumber);
  var encodeJson13 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons12(/* @__PURE__ */ gEncodeJsonCons12(gEncodeJsonNil)(oldIsSymbol)())(newIsSymbol)())());
  var encodeJson22 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeSet2(ordJsonValueType)(encodeJsonJsonValueType));
  var gEncodeJsonCons22 = /* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonList(encodeJsonSchemaPathSegme))(gEncodeJsonNil)(pathIsSymbol2)();
  var nes17 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " and ";
    }
  }));
  var singleton14 = /* @__PURE__ */ singleton2(plusArray);
  var paragraph4 = /* @__PURE__ */ paragraph(foldable1NonEmptyArray);
  var nes18 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "numerical values must be multiples of ";
    }
  }));
  var show13 = /* @__PURE__ */ show1(show1Number);
  var nes26 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " now";
    }
  }));
  var nes36 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "the old multiple constraint of ";
    }
  }));
  var nes46 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " is not a factor of the new multiple constraint of ";
    }
  }));
  var nes54 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "the range of allowed values has been reduced by ";
    }
  }));
  var nes64 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "the set of allowed JSON value types has been reduced by ";
    }
  }));
  var join1With2 = /* @__PURE__ */ join1With(foldable1NonEmptyArray);
  var map27 = /* @__PURE__ */ map(functorNonEmptyArray);
  var fromFoldable17 = /* @__PURE__ */ fromFoldable1(foldable1NonEmptySet);
  var append10 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes74 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "schema path: ";
    }
  }));
  var fromFoldable18 = /* @__PURE__ */ fromFoldable2(/* @__PURE__ */ foldableNonEmpty(foldableArray));
  var nes83 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "numerical values must not be multiples of ";
    }
  }));
  var nes92 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "anymore";
    }
  }));
  var nes102 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "the new multiple constraint of ";
    }
  }));
  var nes112 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " is not a factor of the old multiple constraint of ";
    }
  }));
  var nes123 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "the range of allowed values has been extended by ";
    }
  }));
  var nes133 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "the set of allowed JSON value types has been extended by ";
    }
  }));
  var nes142 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Reasons for breaking the forward compatibility:";
    }
  }));
  var unorderedList4 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var fromFoldable112 = /* @__PURE__ */ fromFoldable1(/* @__PURE__ */ foldable1NonEmpty(foldableArray));
  var nes152 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Reasons for breaking the backward compatibility:";
    }
  }));
  var nes162 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "\u2713";
    }
  }));
  var append16 = /* @__PURE__ */ append(/* @__PURE__ */ semigroupNonEmpty(applicativeArray)(semigroupArray));
  var member3 = /* @__PURE__ */ member2(ordJsonValueType);
  var insert8 = /* @__PURE__ */ insert6(ordJsonValueType);
  var fromFoldable24 = /* @__PURE__ */ fromFoldable6(foldableArray)(ordJsonValueType);
  var difference4 = /* @__PURE__ */ difference3(ordJsonValueType);
  var foldl5 = /* @__PURE__ */ foldl(foldableSet);
  var top4 = /* @__PURE__ */ top(boundedNumber);
  var bottom3 = /* @__PURE__ */ bottom(boundedNumber);
  var Lower = /* @__PURE__ */ function() {
    function Lower2(value0) {
      this.value0 = value0;
    }
    ;
    Lower2.create = function(value0) {
      return new Lower2(value0);
    };
    return Lower2;
  }();
  var LowerAndUpper = /* @__PURE__ */ function() {
    function LowerAndUpper2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    LowerAndUpper2.create = function(value0) {
      return function(value1) {
        return new LowerAndUpper2(value0, value1);
      };
    };
    return LowerAndUpper2;
  }();
  var Upper = /* @__PURE__ */ function() {
    function Upper2(value0) {
      this.value0 = value0;
    }
    ;
    Upper2.create = function(value0) {
      return new Upper2(value0);
    };
    return Upper2;
  }();
  var MultipleWithdrawn = /* @__PURE__ */ function() {
    function MultipleWithdrawn2(value0) {
      this.value0 = value0;
    }
    ;
    MultipleWithdrawn2.create = function(value0) {
      return new MultipleWithdrawn2(value0);
    };
    return MultipleWithdrawn2;
  }();
  var NewMultipleIsNotFactorOfOldMultiple = /* @__PURE__ */ function() {
    function NewMultipleIsNotFactorOfOldMultiple2(value0) {
      this.value0 = value0;
    }
    ;
    NewMultipleIsNotFactorOfOldMultiple2.create = function(value0) {
      return new NewMultipleIsNotFactorOfOldMultiple2(value0);
    };
    return NewMultipleIsNotFactorOfOldMultiple2;
  }();
  var RangeOfAllowedNumbersExtended = /* @__PURE__ */ function() {
    function RangeOfAllowedNumbersExtended2(value0) {
      this.value0 = value0;
    }
    ;
    RangeOfAllowedNumbersExtended2.create = function(value0) {
      return new RangeOfAllowedNumbersExtended2(value0);
    };
    return RangeOfAllowedNumbersExtended2;
  }();
  var SetOfAllowedTypesExtended = /* @__PURE__ */ function() {
    function SetOfAllowedTypesExtended2(value0) {
      this.value0 = value0;
    }
    ;
    SetOfAllowedTypesExtended2.create = function(value0) {
      return new SetOfAllowedTypesExtended2(value0);
    };
    return SetOfAllowedTypesExtended2;
  }();
  var MultipleIntroduced = /* @__PURE__ */ function() {
    function MultipleIntroduced2(value0) {
      this.value0 = value0;
    }
    ;
    MultipleIntroduced2.create = function(value0) {
      return new MultipleIntroduced2(value0);
    };
    return MultipleIntroduced2;
  }();
  var OldMultipleIsNotFactorOfNewMultiple = /* @__PURE__ */ function() {
    function OldMultipleIsNotFactorOfNewMultiple2(value0) {
      this.value0 = value0;
    }
    ;
    OldMultipleIsNotFactorOfNewMultiple2.create = function(value0) {
      return new OldMultipleIsNotFactorOfNewMultiple2(value0);
    };
    return OldMultipleIsNotFactorOfNewMultiple2;
  }();
  var RangeOfAllowedNumbersReduced = /* @__PURE__ */ function() {
    function RangeOfAllowedNumbersReduced2(value0) {
      this.value0 = value0;
    }
    ;
    RangeOfAllowedNumbersReduced2.create = function(value0) {
      return new RangeOfAllowedNumbersReduced2(value0);
    };
    return RangeOfAllowedNumbersReduced2;
  }();
  var SetOfAllowedTypesReduced = /* @__PURE__ */ function() {
    function SetOfAllowedTypesReduced2(value0) {
      this.value0 = value0;
    }
    ;
    SetOfAllowedTypesReduced2.create = function(value0) {
      return new SetOfAllowedTypesReduced2(value0);
    };
    return SetOfAllowedTypesReduced2;
  }();
  var Backward = /* @__PURE__ */ function() {
    function Backward2(value0) {
      this.value0 = value0;
    }
    ;
    Backward2.create = function(value0) {
      return new Backward2(value0);
    };
    return Backward2;
  }();
  var Forward = /* @__PURE__ */ function() {
    function Forward2(value0) {
      this.value0 = value0;
    }
    ;
    Forward2.create = function(value0) {
      return new Forward2(value0);
    };
    return Forward2;
  }();
  var Full = /* @__PURE__ */ function() {
    function Full2() {
    }
    ;
    Full2.value = new Full2();
    return Full2;
  }();
  var None = /* @__PURE__ */ function() {
    function None3(value0) {
      this.value0 = value0;
    }
    ;
    None3.create = function(value0) {
      return new None3(value0);
    };
    return None3;
  }();
  var eqNumberRangeChange = {
    eq: function(x) {
      return function(y) {
        if (x instanceof Lower && y instanceof Lower) {
          return eq7(x.value0.from)(y.value0.from) && eq7(x.value0.to)(y.value0.to);
        }
        ;
        if (x instanceof LowerAndUpper && y instanceof LowerAndUpper) {
          return eq7(x.value0.from)(y.value0.from) && eq7(x.value0.to)(y.value0.to) && (eq7(x.value1.from)(y.value1.from) && eq7(x.value1.to)(y.value1.to));
        }
        ;
        if (x instanceof Upper && y instanceof Upper) {
          return eq7(x.value0.from)(y.value0.from) && eq7(x.value0.to)(y.value0.to);
        }
        ;
        return false;
      };
    }
  };
  var eq34 = /* @__PURE__ */ eq(eqNumberRangeChange);
  var ordNumberRangeChange = {
    compare: function(x) {
      return function(y) {
        if (x instanceof Lower && y instanceof Lower) {
          var v = compare8(x.value0.from)(y.value0.from);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare8(x.value0.to)(y.value0.to);
        }
        ;
        if (x instanceof Lower) {
          return LT.value;
        }
        ;
        if (y instanceof Lower) {
          return GT.value;
        }
        ;
        if (x instanceof LowerAndUpper && y instanceof LowerAndUpper) {
          var v = function() {
            var v12 = compare8(x.value0.from)(y.value0.from);
            if (v12 instanceof LT) {
              return LT.value;
            }
            ;
            if (v12 instanceof GT) {
              return GT.value;
            }
            ;
            return compare8(x.value0.to)(y.value0.to);
          }();
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          var v1 = compare8(x.value1.from)(y.value1.from);
          if (v1 instanceof LT) {
            return LT.value;
          }
          ;
          if (v1 instanceof GT) {
            return GT.value;
          }
          ;
          return compare8(x.value1.to)(y.value1.to);
        }
        ;
        if (x instanceof LowerAndUpper) {
          return LT.value;
        }
        ;
        if (y instanceof LowerAndUpper) {
          return GT.value;
        }
        ;
        if (x instanceof Upper && y instanceof Upper) {
          var v = compare8(x.value0.from)(y.value0.from);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare8(x.value0.to)(y.value0.to);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqNumberRangeChange;
    }
  };
  var compare33 = /* @__PURE__ */ compare(ordNumberRangeChange);
  var eqForwardIncompatibilityT = {
    eq: function(x) {
      return function(y) {
        if (x instanceof MultipleWithdrawn && y instanceof MultipleWithdrawn) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof NewMultipleIsNotFactorOfOldMultiple && y instanceof NewMultipleIsNotFactorOfOldMultiple) {
          return x["value0"]["new"] === y["value0"]["new"] && x.value0.old === y.value0.old;
        }
        ;
        if (x instanceof RangeOfAllowedNumbersExtended && y instanceof RangeOfAllowedNumbersExtended) {
          return eq34(x.value0)(y.value0);
        }
        ;
        if (x instanceof SetOfAllowedTypesExtended && y instanceof SetOfAllowedTypesExtended) {
          return eq24(x.value0)(y.value0);
        }
        ;
        return false;
      };
    }
  };
  var ordForwardIncompatibility = {
    compare: function(x) {
      return function(y) {
        if (x instanceof MultipleWithdrawn && y instanceof MultipleWithdrawn) {
          return compare15(x.value0)(y.value0);
        }
        ;
        if (x instanceof MultipleWithdrawn) {
          return LT.value;
        }
        ;
        if (y instanceof MultipleWithdrawn) {
          return GT.value;
        }
        ;
        if (x instanceof NewMultipleIsNotFactorOfOldMultiple && y instanceof NewMultipleIsNotFactorOfOldMultiple) {
          var v = compare15(x["value0"]["new"])(y["value0"]["new"]);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare15(x.value0.old)(y.value0.old);
        }
        ;
        if (x instanceof NewMultipleIsNotFactorOfOldMultiple) {
          return LT.value;
        }
        ;
        if (y instanceof NewMultipleIsNotFactorOfOldMultiple) {
          return GT.value;
        }
        ;
        if (x instanceof RangeOfAllowedNumbersExtended && y instanceof RangeOfAllowedNumbersExtended) {
          return compare33(x.value0)(y.value0);
        }
        ;
        if (x instanceof RangeOfAllowedNumbersExtended) {
          return LT.value;
        }
        ;
        if (y instanceof RangeOfAllowedNumbersExtended) {
          return GT.value;
        }
        ;
        if (x instanceof SetOfAllowedTypesExtended && y instanceof SetOfAllowedTypesExtended) {
          return compare24(x.value0)(y.value0);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqForwardIncompatibilityT;
    }
  };
  var ordForwardIncompatibility1 = /* @__PURE__ */ ordRecord2(/* @__PURE__ */ ordRecordCons2(ordForwardIncompatibility));
  var eqBackwardIncompatibility = {
    eq: function(x) {
      return function(y) {
        if (x instanceof MultipleIntroduced && y instanceof MultipleIntroduced) {
          return x.value0 === y.value0;
        }
        ;
        if (x instanceof OldMultipleIsNotFactorOfNewMultiple && y instanceof OldMultipleIsNotFactorOfNewMultiple) {
          return x["value0"]["new"] === y["value0"]["new"] && x.value0.old === y.value0.old;
        }
        ;
        if (x instanceof RangeOfAllowedNumbersReduced && y instanceof RangeOfAllowedNumbersReduced) {
          return eq34(x.value0)(y.value0);
        }
        ;
        if (x instanceof SetOfAllowedTypesReduced && y instanceof SetOfAllowedTypesReduced) {
          return eq24(x.value0)(y.value0);
        }
        ;
        return false;
      };
    }
  };
  var ordBackwardIncompatibilit = {
    compare: function(x) {
      return function(y) {
        if (x instanceof MultipleIntroduced && y instanceof MultipleIntroduced) {
          return compare15(x.value0)(y.value0);
        }
        ;
        if (x instanceof MultipleIntroduced) {
          return LT.value;
        }
        ;
        if (y instanceof MultipleIntroduced) {
          return GT.value;
        }
        ;
        if (x instanceof OldMultipleIsNotFactorOfNewMultiple && y instanceof OldMultipleIsNotFactorOfNewMultiple) {
          var v = compare15(x["value0"]["new"])(y["value0"]["new"]);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare15(x.value0.old)(y.value0.old);
        }
        ;
        if (x instanceof OldMultipleIsNotFactorOfNewMultiple) {
          return LT.value;
        }
        ;
        if (y instanceof OldMultipleIsNotFactorOfNewMultiple) {
          return GT.value;
        }
        ;
        if (x instanceof RangeOfAllowedNumbersReduced && y instanceof RangeOfAllowedNumbersReduced) {
          return compare33(x.value0)(y.value0);
        }
        ;
        if (x instanceof RangeOfAllowedNumbersReduced) {
          return LT.value;
        }
        ;
        if (y instanceof RangeOfAllowedNumbersReduced) {
          return GT.value;
        }
        ;
        if (x instanceof SetOfAllowedTypesReduced && y instanceof SetOfAllowedTypesReduced) {
          return compare24(x.value0)(y.value0);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 0, column 0 - line 0, column 0): " + [x.constructor.name, y.constructor.name]);
      };
    },
    Eq0: function() {
      return eqBackwardIncompatibility;
    }
  };
  var ordBackwardIncompatibilit1 = /* @__PURE__ */ ordRecord2(/* @__PURE__ */ ordRecordCons2(ordBackwardIncompatibilit));
  var encodeJsonNumberRangeChan = {
    encodeJson: function(v) {
      if (v instanceof Lower) {
        return extend5(assoc4("lower")(encodeJson6(v.value0)))(jsonEmptyObject);
      }
      ;
      if (v instanceof LowerAndUpper) {
        return extend5(assoc4("lower")(encodeJson6(v.value0)))(extend5(assoc4("upper")(encodeJson6(v.value1)))(jsonEmptyObject));
      }
      ;
      if (v instanceof Upper) {
        return extend5(assoc4("upper")(encodeJson6(v.value0)))(jsonEmptyObject);
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 357, column 16 - line 367, column 29): " + [v.constructor.name]);
    }
  };
  var encodeJson32 = /* @__PURE__ */ encodeJson(encodeJsonNumberRangeChan);
  var encodeJsonForwardIncompat = {
    encodeJson: function(v) {
      if (v instanceof MultipleWithdrawn) {
        return extend5(assoc4("multipleWithdrawn")(id3(v.value0)))(jsonEmptyObject);
      }
      ;
      if (v instanceof NewMultipleIsNotFactorOfOldMultiple) {
        return extend5(assoc4("newMultipleIsNotFactorOfOldMultiple")(encodeJson13({
          "new": v["value0"]["new"],
          old: v.value0.old
        })))(jsonEmptyObject);
      }
      ;
      if (v instanceof RangeOfAllowedNumbersExtended) {
        return extend5(assoc4("rangeOfAllowedNumbersExtended")(encodeJson32(v.value0)))(jsonEmptyObject);
      }
      ;
      if (v instanceof SetOfAllowedTypesExtended) {
        return extend5(assoc4("setOfAllowedTypesExtended")(encodeJson22(toSet(v.value0))))(jsonEmptyObject);
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 282, column 16 - line 296, column 29): " + [v.constructor.name]);
    }
  };
  var encodeJsonForwardIncompat1 = /* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons(encodeJsonForwardIncompat)(gEncodeJsonCons22)(incompatibilityTypeIsSymbol)())();
  var encodeJson42 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeSet2(ordForwardIncompatibility1)(encodeJsonForwardIncompat1));
  var encodeJsonBackwardIncompa = {
    encodeJson: function(v) {
      if (v instanceof MultipleIntroduced) {
        return extend5(assoc4("multipleIntroduced")(id3(v.value0)))(jsonEmptyObject);
      }
      ;
      if (v instanceof OldMultipleIsNotFactorOfNewMultiple) {
        return extend5(assoc4("oldMultipleIsNotFactorOfNewMultiple")(encodeJson13({
          "new": v["value0"]["new"],
          old: v.value0.old
        })))(jsonEmptyObject);
      }
      ;
      if (v instanceof RangeOfAllowedNumbersReduced) {
        return extend5(assoc4("rangeOfAllowedNumbersReduced")(encodeJson32(v.value0)))(jsonEmptyObject);
      }
      ;
      if (v instanceof SetOfAllowedTypesReduced) {
        return extend5(assoc4("setOfAllowedTypesReduced")(encodeJson22(toSet(v.value0))))(jsonEmptyObject);
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 203, column 16 - line 217, column 29): " + [v.constructor.name]);
    }
  };
  var encodeJsonBackwardIncompa1 = /* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons(encodeJsonBackwardIncompa)(gEncodeJsonCons22)(incompatibilityTypeIsSymbol)())();
  var encodeJson52 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeSet2(ordBackwardIncompatibilit1)(encodeJsonBackwardIncompa1));
  var encodeJsonCompatibility = {
    encodeJson: function(v) {
      if (v instanceof Backward) {
        return extend5(assoc4("compatibilityType")(id3("backward")))(extend5(assoc4("incompabilities")(extend5(assoc4("forward")(encodeJson42(toSet(v.value0.forwardIncompatibilities))))(jsonEmptyString)))(jsonEmptyObject));
      }
      ;
      if (v instanceof Forward) {
        return extend5(assoc4("compatibilityType")(id3("forward")))(extend5(assoc4("incompabilities")(extend5(assoc4("backward")(encodeJson52(toSet(v.value0.backwardIncompatibilities))))(jsonEmptyString)))(jsonEmptyObject));
      }
      ;
      if (v instanceof Full) {
        return jsonEmptyObject;
      }
      ;
      if (v instanceof None) {
        return extend5(assoc4("compatibilityType")(id3("none")))(extend5(assoc4("incompabilities")(extend5(assoc4("backward")(encodeJson52(toSet(v.value0.backwardIncompatibilities))))(extend5(assoc4("forward")(encodeJson42(toSet(v.value0.forwardIncompatibilities))))(jsonEmptyString))))(jsonEmptyObject));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 61, column 16 - line 104, column 29): " + [v.constructor.name]);
    }
  };
  var union4 = function(dictOrd) {
    var unionSet2 = unionSet(dictOrd);
    return function(sl) {
      return function(sr) {
        return unionSet2(toSet(sl))(sr);
      };
    };
  };
  var union1 = /* @__PURE__ */ union4(ordForwardIncompatibility1);
  var union22 = /* @__PURE__ */ union4(ordBackwardIncompatibilit1);
  var renderNumberRangeChange = function(v) {
    if (v instanceof Lower) {
      return singleton8(renderRange(v.value0));
    }
    ;
    if (v instanceof LowerAndUpper) {
      return cons$prime2(renderRange(v.value0))([text2(nes17($$Proxy.value)), renderRange(v.value1)]);
    }
    ;
    if (v instanceof Upper) {
      return singleton8(renderRange(v.value0));
    }
    ;
    throw new Error("Failed pattern match at JsonSchema.Compatibility (line 374, column 27 - line 384, column 48): " + [v.constructor.name]);
  };
  var documentBackwardIncompati = {
    document: function($806) {
      return singleton14(paragraph4(function(v) {
        if (v instanceof MultipleIntroduced) {
          return cons$prime2(text2(nes18($$Proxy.value)))([text2(show13(v.value0)), text2(nes26($$Proxy.value))]);
        }
        ;
        if (v instanceof OldMultipleIsNotFactorOfNewMultiple) {
          return cons$prime2(text2(nes36($$Proxy.value)))([text2(show13(v.value0.old)), text2(nes46($$Proxy.value)), text2(show13(v["value0"]["new"]))]);
        }
        ;
        if (v instanceof RangeOfAllowedNumbersReduced) {
          return cons3(text2(nes54($$Proxy.value)))(renderNumberRangeChange(v.value0));
        }
        ;
        if (v instanceof SetOfAllowedTypesReduced) {
          return cons$prime2(text2(nes64($$Proxy.value)))([text2(join1With2(", ")(map27(renderJsonValueType)(fromFoldable17(v.value0))))]);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 224, column 5 - line 268, column 14): " + [v.constructor.name]);
      }($806)));
    }
  };
  var document5 = /* @__PURE__ */ document2(documentBackwardIncompati);
  var documentBackwardIncompati1 = {
    document: function(v) {
      return new NonEmpty(paragraph4(singleton8(text2(append10(nes74($$Proxy.value))(render7(v.path))))), fromFoldable18(document5(v.incompatibilityType)));
    }
  };
  var document1 = /* @__PURE__ */ document2(/* @__PURE__ */ documentNonEmptySet(documentBackwardIncompati1));
  var documentForwardIncompatib = {
    document: function($807) {
      return singleton14(paragraph4(function(v) {
        if (v instanceof MultipleWithdrawn) {
          return cons$prime2(text2(nes83($$Proxy.value)))([text2(show13(v.value0)), text2(nes92($$Proxy.value))]);
        }
        ;
        if (v instanceof NewMultipleIsNotFactorOfOldMultiple) {
          return cons$prime2(text2(nes102($$Proxy.value)))([text2(show13(v["value0"]["new"])), text2(nes112($$Proxy.value)), text2(show13(v.value0.old))]);
        }
        ;
        if (v instanceof RangeOfAllowedNumbersExtended) {
          return cons3(text2(nes123($$Proxy.value)))(renderNumberRangeChange(v.value0));
        }
        ;
        if (v instanceof SetOfAllowedTypesExtended) {
          return cons$prime2(text2(nes133($$Proxy.value)))([text2(join1With2(", ")(map27(renderJsonValueType)(fromFoldable17(v.value0))))]);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 303, column 5 - line 345, column 14): " + [v.constructor.name]);
      }($807)));
    }
  };
  var document22 = /* @__PURE__ */ document2(documentForwardIncompatib);
  var documentForwardIncompatib1 = {
    document: function(v) {
      return new NonEmpty(paragraph4(singleton8(text2(append10(nes74($$Proxy.value))(render7(v.path))))), fromFoldable18(document22(v.incompatibilityType)));
    }
  };
  var document32 = /* @__PURE__ */ document2(/* @__PURE__ */ documentNonEmptySet(documentForwardIncompatib1));
  var documentCompatibility = {
    document: function(v) {
      if (v instanceof Backward) {
        return new NonEmpty(paragraph4(singleton8(text2(nes142($$Proxy.value)))), [unorderedList4(map27(singleton8)(fromFoldable112(document32(v.value0.forwardIncompatibilities))))]);
      }
      ;
      if (v instanceof Forward) {
        return new NonEmpty(paragraph4(singleton8(text2(nes152($$Proxy.value)))), [unorderedList4(map27(singleton8)(fromFoldable112(document1(v.value0.backwardIncompatibilities))))]);
      }
      ;
      if (v instanceof Full) {
        return singleton14(paragraph4(singleton8(text2(nes162($$Proxy.value)))));
      }
      ;
      if (v instanceof None) {
        return append16(document2(documentCompatibility)(new Backward({
          forwardIncompatibilities: v.value0.forwardIncompatibilities
        })))(document2(documentCompatibility)(new Forward({
          backwardIncompatibilities: v.value0.backwardIncompatibilities
        })));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 110, column 14 - line 149, column 60): " + [v.constructor.name]);
    }
  };
  var mergeCompatibility = /* @__PURE__ */ function() {
    var mergeForwardIncompabilities = function(v) {
      return function(v1) {
        return union1(v.forwardIncompatibilities)(v1.forwardIncompatibilities);
      };
    };
    var mergeBackwardIncompabilities = function(v) {
      return function(v1) {
        return union22(v.backwardIncompatibilities)(v1.backwardIncompatibilities);
      };
    };
    return function(v) {
      return function(v1) {
        if (v instanceof Backward && v1 instanceof Backward) {
          return new Backward({
            forwardIncompatibilities: mergeForwardIncompabilities(v.value0)(v1.value0)
          });
        }
        ;
        if (v instanceof Backward && v1 instanceof Forward) {
          return new None({
            backwardIncompatibilities: v1.value0.backwardIncompatibilities,
            forwardIncompatibilities: v.value0.forwardIncompatibilities
          });
        }
        ;
        if (v instanceof Backward && v1 instanceof Full) {
          return new Backward(v.value0);
        }
        ;
        if (v instanceof Backward && v1 instanceof None) {
          return new None({
            backwardIncompatibilities: v1.value0.backwardIncompatibilities,
            forwardIncompatibilities: mergeForwardIncompabilities(v.value0)(v1.value0)
          });
        }
        ;
        if (v instanceof Forward && v1 instanceof Backward) {
          return new None({
            backwardIncompatibilities: v.value0.backwardIncompatibilities,
            forwardIncompatibilities: v1.value0.forwardIncompatibilities
          });
        }
        ;
        if (v instanceof Forward && v1 instanceof Forward) {
          return new Forward({
            backwardIncompatibilities: mergeBackwardIncompabilities(v.value0)(v1.value0)
          });
        }
        ;
        if (v instanceof Forward && v1 instanceof Full) {
          return new Forward(v.value0);
        }
        ;
        if (v instanceof Forward && v1 instanceof None) {
          return new None({
            backwardIncompatibilities: mergeBackwardIncompabilities(v.value0)(v1.value0),
            forwardIncompatibilities: v1.value0.forwardIncompatibilities
          });
        }
        ;
        if (v instanceof Full) {
          return v1;
        }
        ;
        if (v instanceof None && v1 instanceof Backward) {
          return new None({
            backwardIncompatibilities: v.value0.backwardIncompatibilities,
            forwardIncompatibilities: mergeForwardIncompabilities(v1.value0)(v.value0)
          });
        }
        ;
        if (v instanceof None && v1 instanceof Forward) {
          return new None({
            backwardIncompatibilities: mergeBackwardIncompabilities(v1.value0)(v.value0),
            forwardIncompatibilities: v.value0.forwardIncompatibilities
          });
        }
        ;
        if (v instanceof None && v1 instanceof None) {
          return new None({
            backwardIncompatibilities: union22(v.value0.backwardIncompatibilities)(v1.value0.backwardIncompatibilities),
            forwardIncompatibilities: union1(v.value0.forwardIncompatibilities)(v1.value0.forwardIncompatibilities)
          });
        }
        ;
        if (v instanceof None && v1 instanceof Full) {
          return new None(v.value0);
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 665, column 22 - line 711, column 11): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  }();
  var effectiveTypes = function(v) {
    if (v instanceof Just) {
      var $697 = member3(JsonNumber.value)(v.value0);
      if ($697) {
        return insert8(JsonInteger.value)(v.value0);
      }
      ;
      return v.value0;
    }
    ;
    if (v instanceof Nothing) {
      return fromFoldable24([JsonArray.value, JsonBoolean.value, JsonInteger.value, JsonNumber.value, JsonNull.value, JsonObject.value, JsonString.value]);
    }
    ;
    throw new Error("Failed pattern match at JsonSchema.Compatibility (line 648, column 18 - line 662, column 8): " + [v.constructor.name]);
  };
  var calculateTypeChange = function(mbTypesBefore) {
    return function(mbTypesAfter) {
      var acceptedTypesBefore = effectiveTypes(mbTypesBefore);
      var acceptedTypesAfter = effectiveTypes(mbTypesAfter);
      var typesAdded = difference4(acceptedTypesAfter)(acceptedTypesBefore);
      var typesAddedCompatibility = function() {
        var v = fromSet(typesAdded);
        if (v instanceof Nothing) {
          return Full.value;
        }
        ;
        if (v instanceof Just) {
          return new Backward({
            forwardIncompatibilities: singleton11({
              incompatibilityType: new SetOfAllowedTypesExtended(v.value0),
              path: Nil.value
            })
          });
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 620, column 31 - line 631, column 12): " + [v.constructor.name]);
      }();
      var typesRemoved = difference4(acceptedTypesBefore)(acceptedTypesAfter);
      var typesRemovedCompatibility = function() {
        var v = fromSet(typesRemoved);
        if (v instanceof Nothing) {
          return Full.value;
        }
        ;
        if (v instanceof Just) {
          return new Forward({
            backwardIncompatibilities: singleton11({
              incompatibilityType: new SetOfAllowedTypesReduced(v.value0),
              path: Nil.value
            })
          });
        }
        ;
        throw new Error("Failed pattern match at JsonSchema.Compatibility (line 632, column 33 - line 643, column 12): " + [v.constructor.name]);
      }();
      return mergeCompatibility(typesAddedCompatibility)(typesRemovedCompatibility);
    };
  };
  var calculateRangeChange = function(differences) {
    var mbReductionUpperRange = foldl5(function(acc) {
      return function(v) {
        if (v.differenceType instanceof ExclusiveMaximumChange && (v.differenceType.value0 instanceof Nothing && v.differenceType.value1 instanceof Just)) {
          return new Just({
            from: new Open(v.differenceType.value1.value0),
            to: new Open(top4)
          });
        }
        ;
        if (v.differenceType instanceof ExclusiveMaximumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $708 = v.differenceType.value1.value0 < v.differenceType.value0.value0;
          if ($708) {
            return new Just({
              from: new Closed(v.differenceType.value1.value0),
              to: new Open(v.differenceType.value0.value0)
            });
          }
          ;
          return acc;
        }
        ;
        if (v.differenceType instanceof MaximumChange && (v.differenceType.value0 instanceof Nothing && v.differenceType.value1 instanceof Just)) {
          return new Just({
            from: new Open(v.differenceType.value1.value0),
            to: new Open(top4)
          });
        }
        ;
        if (v.differenceType instanceof MaximumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $716 = v.differenceType.value1.value0 < v.differenceType.value0.value0;
          if ($716) {
            return new Just({
              from: new Open(v.differenceType.value1.value0),
              to: new Closed(v.differenceType.value0.value0)
            });
          }
          ;
          return acc;
        }
        ;
        return acc;
      };
    })(Nothing.value)(differences);
    var mbReductionLowerRange = foldl5(function(acc) {
      return function(v) {
        if (v.differenceType instanceof ExclusiveMinimumChange && (v.differenceType.value0 instanceof Nothing && v.differenceType.value1 instanceof Just)) {
          return new Just({
            from: new Open(bottom3),
            to: new Open(v.differenceType.value1.value0)
          });
        }
        ;
        if (v.differenceType instanceof ExclusiveMinimumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $727 = v.differenceType.value1.value0 > v.differenceType.value0.value0;
          if ($727) {
            return new Just({
              from: new Open(v.differenceType.value0.value0),
              to: new Closed(v.differenceType.value1.value0)
            });
          }
          ;
          return acc;
        }
        ;
        if (v.differenceType instanceof MinimumChange && (v.differenceType.value0 instanceof Nothing && v.differenceType.value1 instanceof Just)) {
          return new Just({
            from: new Open(bottom3),
            to: new Open(v.differenceType.value1.value0)
          });
        }
        ;
        if (v.differenceType instanceof MinimumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $735 = v.differenceType.value1.value0 > v.differenceType.value0.value0;
          if ($735) {
            return new Just({
              from: new Closed(v.differenceType.value0.value0),
              to: new Open(v.differenceType.value1.value0)
            });
          }
          ;
          return acc;
        }
        ;
        return acc;
      };
    })(Nothing.value)(differences);
    var mbExtensionUpperRange = foldl5(function(acc) {
      return function(v) {
        if (v.differenceType instanceof ExclusiveMaximumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Nothing)) {
          return new Just({
            from: new Closed(v.differenceType.value0.value0),
            to: new Open(top4)
          });
        }
        ;
        if (v.differenceType instanceof ExclusiveMaximumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $746 = v.differenceType.value1.value0 > v.differenceType.value0.value0;
          if ($746) {
            return new Just({
              from: new Closed(v.differenceType.value0.value0),
              to: new Open(v.differenceType.value1.value0)
            });
          }
          ;
          return acc;
        }
        ;
        if (v.differenceType instanceof MaximumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Nothing)) {
          return new Just({
            from: new Open(v.differenceType.value0.value0),
            to: new Open(top4)
          });
        }
        ;
        if (v.differenceType instanceof MaximumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $754 = v.differenceType.value1.value0 > v.differenceType.value0.value0;
          if ($754) {
            return new Just({
              from: new Open(v.differenceType.value0.value0),
              to: new Closed(v.differenceType.value1.value0)
            });
          }
          ;
          return acc;
        }
        ;
        return acc;
      };
    })(Nothing.value)(differences);
    var mbExtensionLowerRange = foldl5(function(acc) {
      return function(v) {
        if (v.differenceType instanceof ExclusiveMinimumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Nothing)) {
          return new Just({
            from: new Open(bottom3),
            to: new Closed(v.differenceType.value0.value0)
          });
        }
        ;
        if (v.differenceType instanceof ExclusiveMinimumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $765 = v.differenceType.value1.value0 < v.differenceType.value0.value0;
          if ($765) {
            return new Just({
              from: new Open(v.differenceType.value1.value0),
              to: new Closed(v.differenceType.value0.value0)
            });
          }
          ;
          return acc;
        }
        ;
        if (v.differenceType instanceof MinimumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Nothing)) {
          return new Just({
            from: new Open(bottom3),
            to: new Closed(v.differenceType.value0.value0)
          });
        }
        ;
        if (v.differenceType instanceof MinimumChange && (v.differenceType.value0 instanceof Just && v.differenceType.value1 instanceof Just)) {
          var $773 = v.differenceType.value1.value0 < v.differenceType.value0.value0;
          if ($773) {
            return new Just({
              from: new Closed(v.differenceType.value1.value0),
              to: new Open(v.differenceType.value0.value0)
            });
          }
          ;
          return acc;
        }
        ;
        return acc;
      };
    })(Nothing.value)(differences);
    var rangeReductionCompatibility = function() {
      if (mbReductionLowerRange instanceof Nothing && mbReductionUpperRange instanceof Nothing) {
        return Full.value;
      }
      ;
      if (mbReductionLowerRange instanceof Just && mbReductionUpperRange instanceof Nothing) {
        return new Forward({
          backwardIncompatibilities: singleton11({
            incompatibilityType: new RangeOfAllowedNumbersReduced(new Lower(mbReductionLowerRange.value0)),
            path: Nil.value
          })
        });
      }
      ;
      if (mbReductionLowerRange instanceof Just && mbReductionUpperRange instanceof Just) {
        return new Forward({
          backwardIncompatibilities: singleton11({
            incompatibilityType: new RangeOfAllowedNumbersReduced(new LowerAndUpper(mbReductionLowerRange.value0, mbReductionUpperRange.value0)),
            path: Nil.value
          })
        });
      }
      ;
      if (mbReductionLowerRange instanceof Nothing && mbReductionUpperRange instanceof Just) {
        return new Forward({
          backwardIncompatibilities: singleton11({
            incompatibilityType: new RangeOfAllowedNumbersReduced(new Upper(mbReductionUpperRange.value0)),
            path: Nil.value
          })
        });
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 438, column 7 - line 467, column 14): " + [mbReductionLowerRange.constructor.name, mbReductionUpperRange.constructor.name]);
    }();
    var rangeExtensionCompatibility = function() {
      if (mbExtensionLowerRange instanceof Nothing && mbExtensionUpperRange instanceof Nothing) {
        return Full.value;
      }
      ;
      if (mbExtensionLowerRange instanceof Just && mbExtensionUpperRange instanceof Nothing) {
        return new Backward({
          forwardIncompatibilities: singleton11({
            incompatibilityType: new RangeOfAllowedNumbersExtended(new Lower(mbExtensionLowerRange.value0)),
            path: Nil.value
          })
        });
      }
      ;
      if (mbExtensionLowerRange instanceof Just && mbExtensionUpperRange instanceof Just) {
        return new Backward({
          forwardIncompatibilities: singleton11({
            incompatibilityType: new RangeOfAllowedNumbersExtended(new LowerAndUpper(mbExtensionLowerRange.value0, mbExtensionUpperRange.value0)),
            path: Nil.value
          })
        });
      }
      ;
      if (mbExtensionLowerRange instanceof Nothing && mbExtensionUpperRange instanceof Just) {
        return new Backward({
          forwardIncompatibilities: singleton11({
            incompatibilityType: new RangeOfAllowedNumbersExtended(new Upper(mbExtensionUpperRange.value0)),
            path: Nil.value
          })
        });
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 407, column 7 - line 436, column 14): " + [mbExtensionLowerRange.constructor.name, mbExtensionUpperRange.constructor.name]);
    }();
    return mergeCompatibility(rangeExtensionCompatibility)(rangeReductionCompatibility);
  };
  var calculateMultipleOfChange = function(v) {
    return function(v1) {
      if (v instanceof Just && v1 instanceof Just) {
        var $793 = isInteger(v.value0 / v1.value0);
        if ($793) {
          return new Forward({
            backwardIncompatibilities: singleton11({
              incompatibilityType: new OldMultipleIsNotFactorOfNewMultiple({
                "new": v1.value0,
                old: v.value0
              }),
              path: Nil.value
            })
          });
        }
        ;
        var $794 = isInteger(v1.value0 / v.value0);
        if ($794) {
          return new Backward({
            forwardIncompatibilities: singleton11({
              incompatibilityType: new NewMultipleIsNotFactorOfOldMultiple({
                "new": v1.value0,
                old: v.value0
              }),
              path: Nil.value
            })
          });
        }
        ;
        return new None({
          backwardIncompatibilities: singleton11({
            incompatibilityType: new OldMultipleIsNotFactorOfNewMultiple({
              "new": v1.value0,
              old: v.value0
            }),
            path: Nil.value
          }),
          forwardIncompatibilities: singleton11({
            incompatibilityType: new NewMultipleIsNotFactorOfOldMultiple({
              "new": v1.value0,
              old: v.value0
            }),
            path: Nil.value
          })
        });
      }
      ;
      if (v instanceof Just && v1 instanceof Nothing) {
        return new Backward({
          forwardIncompatibilities: singleton11({
            incompatibilityType: new MultipleWithdrawn(v.value0),
            path: Nil.value
          })
        });
      }
      ;
      if (v instanceof Nothing && v1 instanceof Just) {
        return new Forward({
          backwardIncompatibilities: singleton11({
            incompatibilityType: new MultipleIntroduced(v1.value0),
            path: Nil.value
          })
        });
      }
      ;
      if (v instanceof Nothing && v1 instanceof Nothing) {
        return Full.value;
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Compatibility (line 558, column 29 - line 606, column 9): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var calculate2 = function(differences) {
    var f = function(v) {
      if (v instanceof MultipleOfChange) {
        return calculateMultipleOfChange(v.value0)(v.value1);
      }
      ;
      if (v instanceof TypeChange) {
        return calculateTypeChange(v.value0)(v.value1);
      }
      ;
      return Full.value;
    };
    return foldl5(function(acc) {
      return function(v) {
        return mergeCompatibility(acc)(f(v.differenceType));
      };
    })(calculateRangeChange(differences))(differences);
  };

  // output/CLI.Command.Compat/index.js
  var bind5 = /* @__PURE__ */ bind(bindEither);
  var map28 = /* @__PURE__ */ map(functorEither);
  var wrap4 = /* @__PURE__ */ wrap();
  var program = function(dictFileAccess) {
    var Monad0 = dictFileAccess.Monad0();
    var Applicative0 = Monad0.Applicative0();
    var commandProgram2 = commandProgram(Applicative0)(documentCompatibility)(encodeJsonCompatibility);
    var bind16 = bind(Monad0.Bind1());
    var readFileContent2 = readFileContent(dictFileAccess);
    var pure16 = pure(Applicative0);
    return function(dictMonadError) {
      var liftEither2 = liftEither(dictMonadError.MonadThrow0());
      var parseSchema2 = function(s) {
        return either(function($24) {
          return Left.create(error($24));
        })(Right.create)(bind5(map28(wrap4)(jsonParser(s)))(function(json) {
          return parseSchema(json);
        }));
      };
      return commandProgram2(dictMonadError)(function(v) {
        return bind16(readFileContent2(v.leftSchemaFilePath))(function(leftSchemaText) {
          return bind16(readFileContent2(v.rightSchemaFilePath))(function(rightSchemaText) {
            return bind16(liftEither2(parseSchema2(leftSchemaText)))(function(leftSchema) {
              return bind16(liftEither2(parseSchema2(rightSchemaText)))(function(rightSchema) {
                var differences = calculate(leftSchema)(rightSchema);
                return pure16(function() {
                  var v1 = calculate2(differences);
                  if (v1 instanceof Full) {
                    return new Right(Full.value);
                  }
                  ;
                  return new Left(v1);
                }());
              });
            });
          });
        });
      });
    };
  };

  // output/CLI.Command.Diff/index.js
  var bind6 = /* @__PURE__ */ bind(bindEither);
  var map29 = /* @__PURE__ */ map(functorEither);
  var wrap5 = /* @__PURE__ */ wrap();
  var documentSet2 = /* @__PURE__ */ documentSet(documentDifference);
  var encodeSet3 = /* @__PURE__ */ encodeSet2(ordDifference)(encodeJsonDifference);
  var program2 = function(dictFileAccess) {
    var Monad0 = dictFileAccess.Monad0();
    var Applicative0 = Monad0.Applicative0();
    var commandProgram2 = commandProgram(Applicative0)(documentSet2)(encodeSet3);
    var bind16 = bind(Monad0.Bind1());
    var readFileContent2 = readFileContent(dictFileAccess);
    var pure16 = pure(Applicative0);
    return function(dictMonadError) {
      var liftEither2 = liftEither(dictMonadError.MonadThrow0());
      var parseSchema2 = function(s) {
        return either(function($26) {
          return Left.create(error($26));
        })(Right.create)(bind6(map29(wrap5)(jsonParser(s)))(function(json) {
          return parseSchema(json);
        }));
      };
      return commandProgram2(dictMonadError)(function(v) {
        return bind16(readFileContent2(v.leftSchemaFilePath))(function(leftSchemaText) {
          return bind16(readFileContent2(v.rightSchemaFilePath))(function(rightSchemaText) {
            return bind16(liftEither2(parseSchema2(leftSchemaText)))(function(leftSchema) {
              return bind16(liftEither2(parseSchema2(rightSchemaText)))(function(rightSchema) {
                var differences = calculate(leftSchema)(rightSchema);
                return pure16(function() {
                  var $23 = isEmpty2(differences);
                  if ($23) {
                    return new Right(differences);
                  }
                  ;
                  return new Left(differences);
                }());
              });
            });
          });
        });
      });
    };
  };

  // output/Data.Map/index.js
  var keys3 = /* @__PURE__ */ function() {
    var $38 = $$void(functorMap);
    return function($39) {
      return fromMap($38($39));
    };
  }();

  // output/JsonSchema.JsonPath/index.js
  var eq15 = /* @__PURE__ */ eq(eqNonEmptyString);
  var compare9 = /* @__PURE__ */ compare(ordInt);
  var compare16 = /* @__PURE__ */ compare(ordNonEmptyString);
  var append11 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var nes19 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "[";
    }
  }));
  var show6 = /* @__PURE__ */ show(showInt);
  var nes110 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
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
  var Property2 = /* @__PURE__ */ function() {
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
        if (x instanceof Property2 && y instanceof Property2) {
          return eq15(x.value0)(y.value0);
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
          return compare9(x.value0)(y.value0);
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
        if (x instanceof Property2 && y instanceof Property2) {
          return compare16(x.value0)(y.value0);
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
        return id3(toNumber(v.value0));
      }
      ;
      if (v instanceof Property2) {
        return id3(toString2(v.value0));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.JsonPath (line 43, column 16 - line 47, column 43): " + [v.constructor.name]);
    }
  };
  var render8 = /* @__PURE__ */ function() {
    var f = function(acc) {
      return function($78) {
        return function(v) {
          return append11(acc)(v);
        }(function(v) {
          if (v instanceof ItemIndex) {
            return append11(appendString(nes19($$Proxy.value))(show6(v.value0)))(nes110($$Proxy.value));
          }
          ;
          if (v instanceof Property2) {
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
      return $79(reverse2($80));
    };
  }();

  // output/JsonSchema.Validation/index.js
  var $runtime_lazy9 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var fromIsSymbol2 = {
    reflectSymbol: function() {
      return "from";
    }
  };
  var toIsSymbol2 = {
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
  var eq16 = /* @__PURE__ */ eq(eqBoundary);
  var eq25 = /* @__PURE__ */ eq(eqJsonValueType);
  var eq35 = /* @__PURE__ */ eq(/* @__PURE__ */ eqSet(eqJsonValueType));
  var eq43 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqJsonPathSegment));
  var eq53 = /* @__PURE__ */ eq(/* @__PURE__ */ eqList(eqSchemaPathSegment));
  var compare10 = /* @__PURE__ */ compare(ordNumber);
  var compare17 = /* @__PURE__ */ compare(ordBoundary);
  var compare25 = /* @__PURE__ */ compare(ordJsonValueType);
  var compare34 = /* @__PURE__ */ compare(/* @__PURE__ */ ordSet(ordJsonValueType));
  var compare43 = /* @__PURE__ */ compare(/* @__PURE__ */ ordList(ordJsonPathSegment));
  var compare53 = /* @__PURE__ */ compare(/* @__PURE__ */ ordList(ordSchemaPathSegment));
  var map30 = /* @__PURE__ */ map(functorArray);
  var fromFoldable19 = /* @__PURE__ */ fromFoldable2(foldableNonEmptySet);
  var extend6 = /* @__PURE__ */ extend3(encodeJsonJson);
  var assoc5 = /* @__PURE__ */ assoc2(encodeJsonJson);
  var gEncodeJsonCons4 = /* @__PURE__ */ gEncodeJsonCons(encodeJsonBoundary);
  var encodeJson7 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons4(/* @__PURE__ */ gEncodeJsonCons4(gEncodeJsonNil)(toIsSymbol2)())(fromIsSymbol2)())());
  var encodeJson14 = /* @__PURE__ */ encodeJson(encodeJsonJsonValueType);
  var encodeJson23 = /* @__PURE__ */ encodeJson(/* @__PURE__ */ encodeSet2(ordJsonValueType)(encodeJsonJsonValueType));
  var singleton15 = /* @__PURE__ */ singleton2(plusArray);
  var paragraph5 = /* @__PURE__ */ paragraph(foldable1NonEmptyArray);
  var nes20 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Schema always fails validation.";
    }
  }));
  var nes111 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Invalid array:";
    }
  }));
  var unorderedList5 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var foldMap14 = /* @__PURE__ */ foldMap1(foldable1NonEmptySet)(semigroupNonEmptyArray);
  var append17 = /* @__PURE__ */ append(semigroupNonEmptyString);
  var show14 = /* @__PURE__ */ show1(show1Number);
  var nes27 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " is not a multiple of ";
    }
  }));
  var show7 = /* @__PURE__ */ show(showNumber);
  var nes37 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " is outside of the valid range of ";
    }
  }));
  var nes47 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Non-unique array item.";
    }
  }));
  var nes55 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "Invalid type. Expected ";
    }
  }));
  var fromFoldable110 = /* @__PURE__ */ fromFoldable5(foldableSet);
  var nes65 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "none";
    }
  }));
  var join1With3 = /* @__PURE__ */ join1With(foldable1NonEmptyArray);
  var map115 = /* @__PURE__ */ map(functorNonEmptyArray);
  var nes75 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return " but got ";
    }
  }));
  var nes84 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return ".";
    }
  }));
  var nes93 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "JSON is valid against schema from 'not'.";
    }
  }));
  var nes103 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "JSON value path: ";
    }
  }));
  var nes113 = /* @__PURE__ */ nes(/* @__PURE__ */ nonEmptyNonEmpty({
    reflectSymbol: function() {
      return "JSON schema path: ";
    }
  }));
  var fromFoldable25 = /* @__PURE__ */ fromFoldable2(/* @__PURE__ */ foldableNonEmpty(foldableArray));
  var foldl6 = /* @__PURE__ */ foldl(foldableArray);
  var insertWith2 = /* @__PURE__ */ insertWith(ordJsonValue);
  var add2 = /* @__PURE__ */ add(semiringInt);
  var filter5 = /* @__PURE__ */ filter(ordJsonValue);
  var member4 = /* @__PURE__ */ member2(ordJsonValue);
  var unwrap5 = /* @__PURE__ */ unwrap();
  var member1 = /* @__PURE__ */ member2(ordJsonValueType);
  var top5 = /* @__PURE__ */ top(boundedNumber);
  var bottom4 = /* @__PURE__ */ bottom(boundedNumber);
  var wrap6 = /* @__PURE__ */ wrap();
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
  var TypeMismatch2 = /* @__PURE__ */ function() {
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
          return eq16(x.value0.validRange.from)(y.value0.validRange.from) && eq16(x.value0.validRange.to)(y.value0.validRange.to) && x.value0.value === y.value0.value;
        }
        ;
        if (x instanceof NonUniqueArrayItem && y instanceof NonUniqueArrayItem) {
          return true;
        }
        ;
        if (x instanceof TypeMismatch2 && y instanceof TypeMismatch2) {
          return eq25(x.value0.actualJsonValueType)(y.value0.actualJsonValueType) && eq35(x.value0.allowedJsonValueTypes)(y.value0.allowedJsonValueTypes);
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
        return eq43(x.jsonPath)(y.jsonPath) && eq(eqViolationReason)(x.reason)(y.reason) && eq53(x.schemaPath)(y.schemaPath);
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
          var v = compare10(x.value0.expectedMultiple)(y.value0.expectedMultiple);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare10(x.value0.value)(y.value0.value);
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
            var v1 = compare17(x.value0.validRange.from)(y.value0.validRange.from);
            if (v1 instanceof LT) {
              return LT.value;
            }
            ;
            if (v1 instanceof GT) {
              return GT.value;
            }
            ;
            return compare17(x.value0.validRange.to)(y.value0.validRange.to);
          }();
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare10(x.value0.value)(y.value0.value);
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
        if (x instanceof TypeMismatch2 && y instanceof TypeMismatch2) {
          var v = compare25(x.value0.actualJsonValueType)(y.value0.actualJsonValueType);
          if (v instanceof LT) {
            return LT.value;
          }
          ;
          if (v instanceof GT) {
            return GT.value;
          }
          ;
          return compare34(x.value0.allowedJsonValueTypes)(y.value0.allowedJsonValueTypes);
        }
        ;
        if (x instanceof TypeMismatch2) {
          return LT.value;
        }
        ;
        if (y instanceof TypeMismatch2) {
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
        var v = compare43(x.jsonPath)(y.jsonPath);
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
        return compare53(x.schemaPath)(y.schemaPath);
      };
    },
    Eq0: function() {
      return eqViolation;
    }
  };
  var foldMapWithIndex2 = /* @__PURE__ */ foldMapWithIndex(foldableWithIndexArray)(/* @__PURE__ */ monoidSet(ordViolation));
  var append18 = /* @__PURE__ */ append(/* @__PURE__ */ semigroupSet(ordViolation));
  var encodeJsonViolationReason = {
    encodeJson: function(v) {
      if (v instanceof AlwaysFailingSchema) {
        return id3("always failing schema");
      }
      ;
      if (v instanceof InvalidArray) {
        return id3(map30(encodeJson($lazy_encodeJsonViolation(0)))(fromFoldable19(v.value0)));
      }
      ;
      if (v instanceof InvalidMultiple) {
        return extend6(assoc5("expectedMultiple")(id3(v.value0.expectedMultiple)))(extend6(assoc5("value")(id3(v.value0.value)))(jsonEmptyObject));
      }
      ;
      if (v instanceof InvalidRange) {
        return extend6(assoc5("validRange")(encodeJson7(v.value0.validRange)))(extend6(assoc5("value")(id3(v.value0.value)))(jsonEmptyObject));
      }
      ;
      if (v instanceof NonUniqueArrayItem) {
        return id3("non-unique array item");
      }
      ;
      if (v instanceof TypeMismatch2) {
        return extend6(assoc5("actualJsonValueType")(encodeJson14(v.value0.actualJsonValueType)))(extend6(assoc5("allowedJsonValueTypes")(encodeJson23(v.value0.allowedJsonValueTypes)))(jsonEmptyObject));
      }
      ;
      if (v instanceof ValidAgainstNotSchema) {
        return id3("valid against 'not' schema");
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Validation (line 90, column 16 - line 111, column 48): " + [v.constructor.name]);
    }
  };
  var $lazy_encodeJsonViolation = /* @__PURE__ */ $runtime_lazy9("encodeJsonViolation", "JsonSchema.Validation", /* @__PURE__ */ encodeRecord(/* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonList(encodeJsonJsonPathSegment))(/* @__PURE__ */ gEncodeJsonCons(encodeJsonViolationReason)(/* @__PURE__ */ gEncodeJsonCons(/* @__PURE__ */ encodeJsonList(encodeJsonSchemaPathSegme))(gEncodeJsonNil)(schemaPathIsSymbol)())(reasonIsSymbol)())(jsonPathIsSymbol)()));
  var encodeJsonViolation = /* @__PURE__ */ $lazy_encodeJsonViolation(56);
  var documentViolationReason = {
    document: function(v) {
      if (v instanceof AlwaysFailingSchema) {
        return singleton15(paragraph5(singleton8(text2(nes20($$Proxy.value)))));
      }
      ;
      if (v instanceof InvalidArray) {
        return new NonEmpty(paragraph5(singleton8(text2(nes111($$Proxy.value)))), [unorderedList5(foldMap14(function() {
          var $390 = document2(documentViolation);
          return function($391) {
            return singleton8(fromNonEmpty($390($391)));
          };
        }())(v.value0))]);
      }
      ;
      if (v instanceof InvalidMultiple) {
        return singleton15(paragraph5(singleton8(text2(append17(show14(v.value0.value))(appendString(nes27($$Proxy.value))(show7(v.value0.expectedMultiple)))))));
      }
      ;
      if (v instanceof InvalidRange) {
        return singleton15(paragraph5(cons$prime2(text2(prependString(show7(v.value0.value))(nes37($$Proxy.value))))([renderRange(v.value0.validRange)])));
      }
      ;
      if (v instanceof NonUniqueArrayItem) {
        return singleton15(paragraph5(singleton8(text2(nes47($$Proxy.value)))));
      }
      ;
      if (v instanceof TypeMismatch2) {
        return singleton15(paragraph5(singleton8(text2(append17(nes55($$Proxy.value))(append17(function() {
          var v1 = fromFoldable110(v.value0.allowedJsonValueTypes);
          if (v1 instanceof Nothing) {
            return nes65($$Proxy.value);
          }
          ;
          if (v1 instanceof Just) {
            return join1With3(" or ")(map115(renderJsonValueType)(v1.value0));
          }
          ;
          throw new Error("Failed pattern match at JsonSchema.Validation (line 163, column 15 - line 168, column 66): " + [v1.constructor.name]);
        }())(append17(nes75($$Proxy.value))(append17(renderJsonValueType(v.value0.actualJsonValueType))(nes84($$Proxy.value)))))))));
      }
      ;
      if (v instanceof ValidAgainstNotSchema) {
        return singleton15(paragraph5(singleton8(text2(nes93($$Proxy.value)))));
      }
      ;
      throw new Error("Failed pattern match at JsonSchema.Validation (line 117, column 14 - line 179, column 71): " + [v.constructor.name]);
    }
  };
  var documentViolation = {
    document: function(v) {
      return new NonEmpty(paragraph5(cons$prime2(text2(nes103($$Proxy.value)))([inlineCode(render8(v.jsonPath)), lineBreak, text2(nes113($$Proxy.value)), inlineCode(render7(v.schemaPath))])), fromFoldable25(document2(documentViolationReason)(v.reason)));
    }
  };
  var validateUniqueItems = function(schemaPath) {
    return function(jsonPath) {
      return function(itemJsons) {
        var frequencies = foldl6(function(acc) {
          return function(json) {
            return insertWith2(add2)(json)(1)(acc);
          };
        })(empty2)(itemJsons);
        var duplicates = keys3(filter5(function(v) {
          return v > 1;
        })(frequencies));
        var f = function(itemIndex) {
          return function(itemJson) {
            var $352 = member4(itemJson)(duplicates);
            if ($352) {
              return singleton9({
                jsonPath: new Cons(new ItemIndex(itemIndex), jsonPath),
                reason: NonUniqueArrayItem.value,
                schemaPath
              });
            }
            ;
            return empty7;
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
          })($$const(JsonString.value))($$const(JsonArray.value))($$const(JsonObject.value))(unwrap5(json));
          var integersAreAllowed = member1(JsonInteger.value)(allowedJsonValueTypes) || member1(JsonNumber.value)(allowedJsonValueTypes);
          var $354 = eq25(jsonValueType)(JsonInteger.value) && integersAreAllowed;
          if ($354) {
            return empty7;
          }
          ;
          var $355 = member1(jsonValueType)(allowedJsonValueTypes);
          if ($355) {
            return empty7;
          }
          ;
          return singleton9({
            jsonPath,
            reason: new TypeMismatch2({
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
              return empty7;
            }
            ;
            return singleton9({
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
            return empty7;
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
              return new Open(top5);
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
              return new Open(bottom4);
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
          var minimumViolations = maybe(empty7)(function(minimum2) {
            var $373 = x >= minimum2;
            if ($373) {
              return empty7;
            }
            ;
            return singleton9({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(Minimum.value, schemaPath)
            });
          })(constraints.minimum);
          var maximumViolations = maybe(empty7)(function(maximum2) {
            var $374 = x <= maximum2;
            if ($374) {
              return empty7;
            }
            ;
            return singleton9({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(Maximum.value, schemaPath)
            });
          })(constraints.maximum);
          var exclusiveMinimumViolations = maybe(empty7)(function(exclusiveMinimum) {
            var $375 = x > exclusiveMinimum;
            if ($375) {
              return empty7;
            }
            ;
            return singleton9({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(ExclusiveMinimum.value, schemaPath)
            });
          })(constraints.exclusiveMinimum);
          var exclusiveMaximumViolations = maybe(empty7)(function(exclusiveMaximum) {
            var $376 = x < exclusiveMaximum;
            if ($376) {
              return empty7;
            }
            ;
            return singleton9({
              jsonPath,
              reason: new InvalidRange({
                validRange,
                value: x
              }),
              schemaPath: new Cons(ExclusiveMaximum.value, schemaPath)
            });
          })(constraints.exclusiveMaximum);
          var rangeViolations = append18(exclusiveMaximumViolations)(append18(exclusiveMinimumViolations)(append18(maximumViolations)(minimumViolations)));
          return append18(validateMultipleOf(schemaPath)(jsonPath)(x)(constraints.multipleOf))(rangeViolations);
        };
      };
    };
  };
  var $lazy_validateAgainst = /* @__PURE__ */ $runtime_lazy9("validateAgainst", "JsonSchema.Validation", function() {
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
              return empty7;
            }();
            var itemsViolations = maybe(empty7)(validateItems(new Cons(Items.value, schemaPath))(jsonPath)(array))(constraints.items);
            return append18(itemsViolations)(uniqueItemsViolations);
          };
        };
      };
    };
    var validateAgainstObjectSchema = function(schemaPath) {
      return function(jsonPath) {
        return function(json) {
          return function(keywords) {
            var typeKeywordViolations = maybe(empty7)(validateTypeKeyword(schemaPath)(jsonPath)(json))(keywords.typeKeyword);
            var notViolations = function() {
              if (keywords.not instanceof Just) {
                var $379 = isEmpty2($lazy_validateAgainst(235)(json)(keywords.not.value0));
                if ($379) {
                  return singleton9({
                    jsonPath,
                    reason: ValidAgainstNotSchema.value,
                    schemaPath
                  });
                }
                ;
                return empty7;
              }
              ;
              if (keywords.not instanceof Nothing) {
                return empty7;
              }
              ;
              throw new Error("Failed pattern match at JsonSchema.Validation (line 233, column 21 - line 243, column 18): " + [keywords.not.constructor.name]);
            }();
            return append18(notViolations)(append18(typeKeywordViolations)(caseJson($$const(empty7))($$const(empty7))(validateNumber(schemaPath)(jsonPath)(keywords))($$const(empty7))(function(array) {
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
                var itemViolations = validateArray(schemaPath)(jsonPath)(map30(wrap6)(array))(keywords);
                var v = fromSet(itemViolations);
                if (v instanceof Just) {
                  return singleton9({
                    jsonPath,
                    reason: new InvalidArray(v.value0),
                    schemaPath
                  });
                }
                ;
                if (v instanceof Nothing) {
                  return empty7;
                }
                ;
                throw new Error("Failed pattern match at JsonSchema.Validation (line 218, column 17 - line 226, column 30): " + [v.constructor.name]);
              }
              ;
              return empty7;
            })($$const(empty7))(unwrap5(json))));
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
                return empty7;
              }
              ;
              return singleton9({
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

  // output/CLI.Command.Validate/index.js
  var bind7 = /* @__PURE__ */ bind(bindEither);
  var map31 = /* @__PURE__ */ map(functorEither);
  var wrap7 = /* @__PURE__ */ wrap();
  var documentSet3 = /* @__PURE__ */ documentSet(documentViolation);
  var encodeSet4 = /* @__PURE__ */ encodeSet2(ordViolation)(encodeJsonViolation);
  var program3 = function(dictFileAccess) {
    var Monad0 = dictFileAccess.Monad0();
    var Applicative0 = Monad0.Applicative0();
    var commandProgram2 = commandProgram(Applicative0)(documentSet3)(encodeSet4);
    var bind16 = bind(Monad0.Bind1());
    var readFileContent2 = readFileContent(dictFileAccess);
    var pure16 = pure(Applicative0);
    return function(dictMonadError) {
      var liftEither2 = liftEither(dictMonadError.MonadThrow0());
      var parseSchema2 = function(s) {
        return either(function($26) {
          return Left.create(error($26));
        })(Right.create)(bind7(map31(wrap7)(jsonParser(s)))(function(json) {
          return parseSchema(json);
        }));
      };
      var parseJson = function(s) {
        return either(function($27) {
          return Left.create(error($27));
        })(Right.create)(map31(wrap7)(jsonParser(s)));
      };
      return commandProgram2(dictMonadError)(function(v) {
        return bind16(readFileContent2(v.schemaFilePath))(function(schemaText) {
          return bind16(readFileContent2(v.jsonFilePath))(function(jsonText) {
            return bind16(liftEither2(parseSchema2(schemaText)))(function(schema) {
              return bind16(liftEither2(parseJson(jsonText)))(function(json) {
                var violations = validateAgainst(json)(schema);
                return pure16(function() {
                  var $23 = isEmpty2(violations);
                  if ($23) {
                    return new Right(violations);
                  }
                  ;
                  return new Left(violations);
                }());
              });
            });
          });
        });
      });
    };
  };

  // output/Sandbox.Component/index.js
  var lookup6 = /* @__PURE__ */ lookup(ordString);
  var show8 = /* @__PURE__ */ show(showInt);
  var optionsFormIsSymbol = {
    reflectSymbol: function() {
      return "optionsForm";
    }
  };
  var slot2 = /* @__PURE__ */ slot()(optionsFormIsSymbol)(ordUnit);
  var type_5 = /* @__PURE__ */ type_3(isPropButtonType);
  var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bind8 = /* @__PURE__ */ bind(bindHalogenM);
  var request2 = /* @__PURE__ */ request()(optionsFormIsSymbol)(ordUnit);
  var pure8 = /* @__PURE__ */ pure(applicativeHalogenM);
  var put2 = /* @__PURE__ */ put(monadStateHalogenM);
  var SelectedCommandChanged = /* @__PURE__ */ function() {
    function SelectedCommandChanged2() {
    }
    ;
    SelectedCommandChanged2.value = new SelectedCommandChanged2();
    return SelectedCommandChanged2;
  }();
  var GetInput2 = /* @__PURE__ */ function() {
    function GetInput3(value0) {
      this.value0 = value0;
    }
    ;
    GetInput3.create = function(value0) {
      return new GetInput3(value0);
    };
    return GetInput3;
  }();
  var HandleOptionsFormOutput = /* @__PURE__ */ function() {
    function HandleOptionsFormOutput2(value0) {
      this.value0 = value0;
    }
    ;
    HandleOptionsFormOutput2.create = function(value0) {
      return new HandleOptionsFormOutput2(value0);
    };
    return HandleOptionsFormOutput2;
  }();
  var RunProgram = /* @__PURE__ */ function() {
    function RunProgram2() {
    }
    ;
    RunProgram2.value = new RunProgram2();
    return RunProgram2;
  }();
  var monadThrowErrorAppM = /* @__PURE__ */ monadThrowReaderT(monadThrowEffect);
  var throwError2 = /* @__PURE__ */ throwError(monadThrowErrorAppM);
  var monadErrorErrorAppM = /* @__PURE__ */ monadErrorReaderT(monadErrorEffect);
  var monadAskMapStringStringAp = /* @__PURE__ */ monadAskReaderT(monadEffect);
  var asks2 = /* @__PURE__ */ asks(monadAskMapStringStringAp);
  var monadAppM = /* @__PURE__ */ monadReaderT(monadEffect);
  var bindAppM = /* @__PURE__ */ bindReaderT(bindEffect);
  var bind12 = /* @__PURE__ */ bind(bindAppM);
  var applicativeAppM = /* @__PURE__ */ applicativeReaderT(applicativeEffect);
  var pure1 = /* @__PURE__ */ pure(applicativeAppM);
  var fileAccessAppM = {
    readFileContent: function(filePath) {
      return bind12(asks2(lookup6(filePath)))(function(fileContents) {
        return maybe(throwError2(error("file " + (filePath + " not found"))))(pure1)(fileContents);
      });
    },
    Monad0: function() {
      return monadAppM;
    }
  };
  var program4 = /* @__PURE__ */ program(fileAccessAppM)(monadErrorErrorAppM);
  var program1 = /* @__PURE__ */ program2(fileAccessAppM)(monadErrorErrorAppM);
  var program22 = /* @__PURE__ */ program3(fileAccessAppM)(monadErrorErrorAppM);
  var runApp = function(v) {
    return function(env) {
      return runReaderT(v)(env);
    };
  };
  var renderProgramOutput = function(v) {
    return div_([div_([label_([text("Exit code:")]), text(show8(v.exitCode))]), div_([label_([text("Standard output:")]), pre_([samp_([text(v.stdout)])])]), div_([label_([text("Standard error:")]), pre_([samp_([text(v.stderr)])])])]);
  };
  var render9 = function(dictMonadAff) {
    return function(dictMonadError) {
      return function(formComponent) {
        return function(v) {
          return div2([style("display: flex")])([div2([style("width: 100%")])([div_([text("Options Form:")]), slot2($$Proxy.value)(unit)(formComponent)(unit)(HandleOptionsFormOutput.create), button([onClick($$const(RunProgram.value)), type_5(ButtonButton.value)])([text("execute program")])]), div2([style("width: 100%")])([div_([text("Program Output:")]), maybe(text(""))(function($87) {
            return fromPlainHTML(renderProgramOutput($87));
          })(v.programOutput)])]);
        };
      };
    };
  };
  var handleAction3 = function(dictMonadAff) {
    var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
    return function(dictMonadError) {
      var throwError1 = throwError(monadThrowHalogenM(dictMonadError.MonadThrow0()));
      var runCommand = function(filesystem) {
        return function(command2) {
          return liftAff2(liftEffect3(runApp(command2)(filesystem)));
        };
      };
      var getOptionsFormInput = bind8(request2($$Proxy.value)(unit)(GetInput2.create))(function(response) {
        return maybe(throwError1(error("Failed to read options form data")))(pure8)(response);
      });
      return function(v) {
        if (v instanceof HandleOptionsFormOutput) {
          return put2({
            programOutput: Nothing.value
          });
        }
        ;
        if (v instanceof RunProgram) {
          return bind8(getOptionsFormInput)(function(v1) {
            return bind8(runCommand(v1.filesystem)(function() {
              if (v1.command instanceof Compat) {
                return program4(v1.command.value0)(v1.command.value1);
              }
              ;
              if (v1.command instanceof Diff) {
                return program1(v1.command.value0)(v1.command.value1);
              }
              ;
              if (v1.command instanceof Validate) {
                return program22(v1.command.value0)(v1.command.value1);
              }
              ;
              throw new Error("Failed pattern match at Sandbox.Component (line 157, column 43 - line 163, column 53): " + [v1.command.constructor.name]);
            }()))(function(programOutput) {
              return put2({
                programOutput: new Just(programOutput)
              });
            });
          });
        }
        ;
        throw new Error("Failed pattern match at Sandbox.Component (line 151, column 16 - line 164, column 46): " + [v.constructor.name]);
      };
    };
  };
  var component5 = function(dictMonadAff) {
    var handleAction1 = handleAction3(dictMonadAff);
    var render1 = render9(dictMonadAff);
    return function(dictMonadError) {
      var handleAction22 = handleAction1(dictMonadError);
      var render22 = render1(dictMonadError);
      return function(formComponent) {
        return mkComponent({
          initialState: $$const({
            programOutput: Nothing.value
          }),
          "eval": mkEval({
            handleAction: handleAction22,
            handleQuery: defaultEval.handleQuery,
            receive: defaultEval.receive,
            initialize: defaultEval.initialize,
            finalize: defaultEval.finalize
          }),
          render: render22(formComponent)
        });
      };
    };
  };

  // output/CLI.Halogen/index.js
  var slot_2 = /* @__PURE__ */ slot_();
  var compatOptionsIsSymbol = {
    reflectSymbol: function() {
      return "compatOptions";
    }
  };
  var slot_1 = /* @__PURE__ */ slot_2(compatOptionsIsSymbol)(ordUnit);
  var diffOptionsIsSymbol = {
    reflectSymbol: function() {
      return "diffOptions";
    }
  };
  var slot_22 = /* @__PURE__ */ slot_2(diffOptionsIsSymbol)(ordUnit);
  var validateOptionsIsSymbol = {
    reflectSymbol: function() {
      return "validateOptions";
    }
  };
  var slot_3 = /* @__PURE__ */ slot_2(validateOptionsIsSymbol)(ordUnit);
  var outputFormatIsSymbol = {
    reflectSymbol: function() {
      return "outputFormat";
    }
  };
  var slot_4 = /* @__PURE__ */ slot_2(outputFormatIsSymbol)(ordUnit);
  var bind9 = /* @__PURE__ */ bind(bindHalogenM);
  var get5 = /* @__PURE__ */ get(monadStateHalogenM);
  var request3 = /* @__PURE__ */ request();
  var request1 = /* @__PURE__ */ request3(outputFormatIsSymbol)(ordUnit);
  var pure9 = /* @__PURE__ */ pure(applicativeHalogenM);
  var request22 = /* @__PURE__ */ request3(compatOptionsIsSymbol)(ordUnit);
  var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorMaybe);
  var request32 = /* @__PURE__ */ request3(diffOptionsIsSymbol)(ordUnit);
  var request4 = /* @__PURE__ */ request3(validateOptionsIsSymbol)(ordUnit);
  var discard2 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
  var modify_8 = /* @__PURE__ */ modify_(monadStateHalogenM);
  var CompatCommand = /* @__PURE__ */ function() {
    function CompatCommand2() {
    }
    ;
    CompatCommand2.value = new CompatCommand2();
    return CompatCommand2;
  }();
  var DiffCommand = /* @__PURE__ */ function() {
    function DiffCommand2() {
    }
    ;
    DiffCommand2.value = new DiffCommand2();
    return DiffCommand2;
  }();
  var ValidateCommand = /* @__PURE__ */ function() {
    function ValidateCommand2() {
    }
    ;
    ValidateCommand2.value = new ValidateCommand2();
    return ValidateCommand2;
  }();
  var Dummy = /* @__PURE__ */ function() {
    function Dummy2() {
    }
    ;
    Dummy2.value = new Dummy2();
    return Dummy2;
  }();
  var SelectedCommandUpdated = /* @__PURE__ */ function() {
    function SelectedCommandUpdated2(value0) {
      this.value0 = value0;
    }
    ;
    SelectedCommandUpdated2.create = function(value0) {
      return new SelectedCommandUpdated2(value0);
    };
    return SelectedCommandUpdated2;
  }();
  var eqSelectedCommand = {
    eq: function(x) {
      return function(y) {
        if (x instanceof CompatCommand && y instanceof CompatCommand) {
          return true;
        }
        ;
        if (x instanceof DiffCommand && y instanceof DiffCommand) {
          return true;
        }
        ;
        if (x instanceof ValidateCommand && y instanceof ValidateCommand) {
          return true;
        }
        ;
        return false;
      };
    }
  };
  var eq8 = /* @__PURE__ */ eq(eqSelectedCommand);
  var showSelectedCommand = function(v) {
    if (v instanceof CompatCommand) {
      return "compat";
    }
    ;
    if (v instanceof DiffCommand) {
      return "diff";
    }
    ;
    if (v instanceof ValidateCommand) {
      return "validate";
    }
    ;
    throw new Error("Failed pattern match at CLI.Halogen (line 168, column 23 - line 174, column 15): " + [v.constructor.name]);
  };
  var render10 = function(v) {
    var renderCommandSelectionItemFor = function(command2) {
      return option([selected(eq8(command2)(v.selectedCommand))])([text(showSelectedCommand(command2))]);
    };
    var renderCommandSelectionDropdown = div_([label([$$for("command")])([text("Command")]), select([id2("command"), name3("command"), onSelectedIndexChange(function(v1) {
      if (v1 === 0) {
        return new SelectedCommandUpdated(CompatCommand.value);
      }
      ;
      if (v1 === 1) {
        return new SelectedCommandUpdated(DiffCommand.value);
      }
      ;
      if (v1 === 2) {
        return new SelectedCommandUpdated(ValidateCommand.value);
      }
      ;
      return Dummy.value;
    })])([renderCommandSelectionItemFor(CompatCommand.value), renderCommandSelectionItemFor(DiffCommand.value), renderCommandSelectionItemFor(ValidateCommand.value)])]);
    var renderCommandOptionsForm = fieldset_([legend_([text("Command Options")]), function() {
      if (v.selectedCommand instanceof CompatCommand) {
        return slot_1($$Proxy.value)(unit)(component)(unit);
      }
      ;
      if (v.selectedCommand instanceof DiffCommand) {
        return slot_22($$Proxy.value)(unit)(component2)(unit);
      }
      ;
      if (v.selectedCommand instanceof ValidateCommand) {
        return slot_3($$Proxy.value)(unit)(component4)(unit);
      }
      ;
      throw new Error("Failed pattern match at CLI.Halogen (line 115, column 7 - line 133, column 17): " + [v.selectedCommand.constructor.name]);
    }()]);
    return form_([renderCommandSelectionDropdown, slot_4($$Proxy.value)(unit)(component3)(unit), renderCommandOptionsForm]);
  };
  var handleQuery3 = function(v) {
    return bind9(get5)(function(state3) {
      return bind9(request1($$Proxy.value)(unit)(GetOutputFormat.create))(function(outputFormat) {
        if (outputFormat instanceof Nothing) {
          return pure9(Nothing.value);
        }
        ;
        if (outputFormat instanceof Just) {
          if (state3.selectedCommand instanceof CompatCommand) {
            return bind9(request22($$Proxy.value)(unit)(GetInput.create))(function(input3) {
              return pure9(mapFlipped2(input3)(function(v1) {
                return v.value0({
                  command: new Compat(outputFormat.value0, v1.options),
                  filesystem: v1.filesystem
                });
              }));
            });
          }
          ;
          if (state3.selectedCommand instanceof DiffCommand) {
            return bind9(request32($$Proxy.value)(unit)(GetInput.create))(function(input3) {
              return pure9(mapFlipped2(input3)(function(v1) {
                return v.value0({
                  command: new Diff(outputFormat.value0, v1.options),
                  filesystem: v1.filesystem
                });
              }));
            });
          }
          ;
          if (state3.selectedCommand instanceof ValidateCommand) {
            return bind9(request4($$Proxy.value)(unit)(GetInput.create))(function(input3) {
              return pure9(mapFlipped2(input3)(function(v1) {
                return v.value0({
                  command: new Validate(outputFormat.value0, v1.options),
                  filesystem: v1.filesystem
                });
              }));
            });
          }
          ;
          throw new Error("Failed pattern match at CLI.Halogen (line 78, column 9 - line 99, column 69): " + [state3.selectedCommand.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at CLI.Halogen (line 74, column 5 - line 99, column 69): " + [outputFormat.constructor.name]);
      });
    });
  };
  var handleAction4 = function(v) {
    if (v instanceof Dummy) {
      return pure9(unit);
    }
    ;
    if (v instanceof SelectedCommandUpdated) {
      return discard2(modify_8(function(st) {
        var $65 = {};
        for (var $66 in st) {
          if ({}.hasOwnProperty.call(st, $66)) {
            $65[$66] = st[$66];
          }
          ;
        }
        ;
        $65.selectedCommand = v.value0;
        return $65;
      }))(function() {
        return raise(SelectedCommandChanged.value);
      });
    }
    ;
    throw new Error("Failed pattern match at CLI.Halogen (line 59, column 16 - line 64, column 43): " + [v.constructor.name]);
  };
  var commandFormComponent = /* @__PURE__ */ function() {
    return mkComponent({
      "eval": mkEval({
        handleAction: handleAction4,
        handleQuery: handleQuery3,
        receive: defaultEval.receive,
        initialize: defaultEval.initialize,
        finalize: defaultEval.finalize
      }),
      initialState: $$const({
        selectedCommand: ValidateCommand.value
      }),
      render: render10
    });
  }();

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
  var map33 = /* @__PURE__ */ map(functorEffect);
  var toParentNode = unsafeCoerce2;
  var toDocument = unsafeCoerce2;
  var readyState = function(doc) {
    return map33(function() {
      var $4 = fromMaybe(Loading.value);
      return function($5) {
        return $4(parse($5));
      };
    }())(function() {
      return _readyState(doc);
    });
  };

  // output/Web.HTML.Window/foreign.js
  function document6(window2) {
    return function() {
      return window2.document;
    };
  }

  // output/Web.HTML.Window/index.js
  var toEventTarget = unsafeCoerce2;

  // output/Halogen.Aff.Util/index.js
  var bind10 = /* @__PURE__ */ bind(bindAff);
  var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindEffect);
  var composeKleisliFlipped4 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
  var pure10 = /* @__PURE__ */ pure(applicativeAff);
  var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
  var pure12 = /* @__PURE__ */ pure(applicativeEffect);
  var map34 = /* @__PURE__ */ map(functorEffect);
  var discard3 = /* @__PURE__ */ discard(discardUnit);
  var throwError3 = /* @__PURE__ */ throwError(monadThrowAff);
  var selectElement = function(query3) {
    return bind10(liftEffect4(bindFlipped5(composeKleisliFlipped4(function() {
      var $16 = querySelector(query3);
      return function($17) {
        return $16(toParentNode($17));
      };
    }())(document6))(windowImpl)))(function(mel) {
      return pure10(bindFlipped1(fromElement)(mel));
    });
  };
  var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure12(unit))));
  var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
    return function __do2() {
      var rs = bindFlipped5(readyState)(bindFlipped5(document6)(windowImpl))();
      if (rs instanceof Loading) {
        var et = map34(toEventTarget)(windowImpl)();
        var listener = eventListener(function(v) {
          return callback(new Right(unit));
        })();
        addEventListener2(domcontentloaded)(listener)(false)(et)();
        return effectCanceler(removeEventListener2(domcontentloaded)(listener)(false)(et));
      }
      ;
      callback(new Right(unit))();
      return nonCanceler;
    };
  });
  var awaitBody = /* @__PURE__ */ discard3(bindAff)(awaitLoad)(function() {
    return bind10(selectElement("body"))(function(body2) {
      return maybe(throwError3(error("Could not find body")))(pure10)(body2);
    });
  });

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
  var fork2 = function(dict) {
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
  var initDriverState = function(component7) {
    return function(input3) {
      return function(handler3) {
        return function(lchs) {
          return function __do2() {
            var selfRef = $$new({})();
            var childrenIn = $$new(empty6)();
            var childrenOut = $$new(empty6)();
            var handlerRef = $$new(handler3)();
            var pendingQueries = $$new(new Just(Nil.value))();
            var pendingOuts = $$new(new Just(Nil.value))();
            var pendingHandlers = $$new(Nothing.value)();
            var fresh2 = $$new(1)();
            var subscriptions = $$new(new Just(empty2))();
            var forks = $$new(empty2)();
            var ds = {
              component: component7,
              state: component7.initialState(input3),
              refs: empty2,
              children: empty6,
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
  var lookup7 = /* @__PURE__ */ lookup(ordSubscriptionId);
  var bind13 = /* @__PURE__ */ bind(bindAff);
  var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var discard4 = /* @__PURE__ */ discard(discardUnit);
  var discard1 = /* @__PURE__ */ discard4(bindAff);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var fork3 = /* @__PURE__ */ fork2(monadForkAff);
  var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var pure11 = /* @__PURE__ */ pure(applicativeAff);
  var map35 = /* @__PURE__ */ map(functorCoyoneda);
  var parallel3 = /* @__PURE__ */ parallel(parallelAff);
  var map116 = /* @__PURE__ */ map(functorAff);
  var sequential2 = /* @__PURE__ */ sequential(parallelAff);
  var map210 = /* @__PURE__ */ map(functorMaybe);
  var insert9 = /* @__PURE__ */ insert(ordSubscriptionId);
  var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
  var $$delete4 = /* @__PURE__ */ $$delete(ordForkId);
  var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
  var insert12 = /* @__PURE__ */ insert(ordForkId);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var lookup12 = /* @__PURE__ */ lookup(ordForkId);
  var lookup22 = /* @__PURE__ */ lookup(ordString);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe3 = function(sid) {
    return function(ref2) {
      return function __do2() {
        var v = read(ref2)();
        var subs = read(v.subscriptions)();
        return traverse_4(unsubscribe)(bindFlipped6(lookup7(sid))(subs))();
      };
    };
  };
  var queueOrRun = function(ref2) {
    return function(au) {
      return bind13(liftEffect5(read(ref2)))(function(v) {
        if (v instanceof Nothing) {
          return au;
        }
        ;
        if (v instanceof Just) {
          return liftEffect5(write(new Just(new Cons(au, v.value0)))(ref2));
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
      });
    };
  };
  var handleLifecycle = function(lchs) {
    return function(f) {
      return discard1(liftEffect5(write({
        initializers: Nil.value,
        finalizers: Nil.value
      })(lchs)))(function() {
        return bind13(liftEffect5(f))(function(result) {
          return bind13(liftEffect5(read(lchs)))(function(v) {
            return discard1(traverse_22(fork3)(v.finalizers))(function() {
              return discard1(parSequence_2(v.initializers))(function() {
                return pure11(result);
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
      return bind13(liftEffect5(read(ref2)))(function(v) {
        return liftEffect5(modify$prime(function(i2) {
          return {
            state: i2 + 1 | 0,
            value: f(i2)
          };
        })(v.fresh));
      });
    };
  };
  var evalQ = function(render11) {
    return function(ref2) {
      return function(q2) {
        return bind13(liftEffect5(read(ref2)))(function(v) {
          return evalM(render11)(ref2)(v["component"]["eval"](new Query(map35(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
        });
      };
    };
  };
  var evalM = function(render11) {
    return function(initRef) {
      return function(v) {
        var evalChildQuery = function(ref2) {
          return function(cqb) {
            return bind13(liftEffect5(read(ref2)))(function(v1) {
              return unChildQueryBox(function(v2) {
                var evalChild = function(v3) {
                  return parallel3(bind13(liftEffect5(read(v3)))(function(dsx) {
                    return unDriverStateX(function(ds) {
                      return evalQ(render11)(ds.selfRef)(v2.value1);
                    })(dsx);
                  }));
                };
                return map116(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
              })(cqb);
            });
          };
        };
        var go2 = function(ref2) {
          return function(v1) {
            if (v1 instanceof State) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                var v3 = v1.value0(v2.state);
                if (unsafeRefEq(v2.state)(v3.value1)) {
                  return pure11(v3.value0);
                }
                ;
                if (otherwise) {
                  return discard1(liftEffect5(write({
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
                    return discard1(handleLifecycle(v2.lifecycleHandlers)(render11(v2.lifecycleHandlers)(ref2)))(function() {
                      return pure11(v3.value0);
                    });
                  });
                }
                ;
                throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
              });
            }
            ;
            if (v1 instanceof Subscribe) {
              return bind13(fresh(SubscriptionId)(ref2))(function(sid) {
                return bind13(liftEffect5(subscribe(v1.value0(sid))(function(act) {
                  return handleAff(evalF(render11)(ref2)(new Action(act)));
                })))(function(finalize) {
                  return bind13(liftEffect5(read(ref2)))(function(v2) {
                    return discard1(liftEffect5(modify_2(map210(insert9(sid)(finalize)))(v2.subscriptions)))(function() {
                      return pure11(v1.value1(sid));
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Unsubscribe) {
              return discard1(liftEffect5(unsubscribe3(v1.value0)(ref2)))(function() {
                return pure11(v1.value1);
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
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return bind13(liftEffect5(read(v2.handlerRef)))(function(handler3) {
                  return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                    return pure11(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Par) {
              return sequential2(retractFreeAp2(hoistFreeAp(function() {
                var $119 = evalM(render11)(ref2);
                return function($120) {
                  return parallel3($119($120));
                };
              }())(v1.value0)));
            }
            ;
            if (v1 instanceof Fork) {
              return bind13(fresh(ForkId)(ref2))(function(fid) {
                return bind13(liftEffect5(read(ref2)))(function(v2) {
                  return bind13(liftEffect5($$new(false)))(function(doneRef) {
                    return bind13(fork3($$finally(liftEffect5(function __do2() {
                      modify_2($$delete4(fid))(v2.forks)();
                      return write(true)(doneRef)();
                    }))(evalM(render11)(ref2)(v1.value0))))(function(fiber) {
                      return discard1(liftEffect5(unlessM2(read(doneRef))(modify_2(insert12(fid)(fiber))(v2.forks))))(function() {
                        return pure11(v1.value1(fid));
                      });
                    });
                  });
                });
              });
            }
            ;
            if (v1 instanceof Join) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return bind13(liftEffect5(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(joinFiber)(lookup12(v1.value0)(forkMap)))(function() {
                    return pure11(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof Kill) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return bind13(liftEffect5(read(v2.forks)))(function(forkMap) {
                  return discard1(traverse_32(killFiber(error("Cancelled")))(lookup12(v1.value0)(forkMap)))(function() {
                    return pure11(v1.value1);
                  });
                });
              });
            }
            ;
            if (v1 instanceof GetRef) {
              return bind13(liftEffect5(read(ref2)))(function(v2) {
                return pure11(v1.value1(lookup22(v1.value0)(v2.refs)));
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
  var evalF = function(render11) {
    return function(ref2) {
      return function(v) {
        if (v instanceof RefUpdate) {
          return liftEffect5(flip(modify_2)(ref2)(mapDriverState(function(st) {
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
          return bind13(liftEffect5(read(ref2)))(function(v1) {
            return evalM(render11)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
          });
        }
        ;
        throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
      };
    };
  };

  // output/Halogen.Aff.Driver/index.js
  var bind11 = /* @__PURE__ */ bind(bindEffect);
  var discard5 = /* @__PURE__ */ discard(discardUnit);
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var fork4 = /* @__PURE__ */ fork2(monadForkAff);
  var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var discard22 = /* @__PURE__ */ discard5(bindAff);
  var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
  var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var pure13 = /* @__PURE__ */ pure(applicativeEffect);
  var map36 = /* @__PURE__ */ map(functorEffect);
  var pure14 = /* @__PURE__ */ pure(applicativeAff);
  var when2 = /* @__PURE__ */ when(applicativeEffect);
  var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
  var $$void6 = /* @__PURE__ */ $$void(functorAff);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
  var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
  var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
  var bind14 = /* @__PURE__ */ bind(bindAff);
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
          return handleAff($59(reverse2($60)));
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
      return write(empty2)(v.forks)();
    };
  };
  var runUI = function(renderSpec2) {
    return function(component7) {
      return function(i2) {
        var squashChildInitializers = function(lchs) {
          return function(preInits) {
            return unDriverStateX(function(st) {
              var parentInitializer = evalM(render11)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
              return modify_2(function(handlers) {
                return {
                  initializers: new Cons(discard22(parSequence_3(reverse2(handlers.initializers)))(function() {
                    return discard22(parentInitializer)(function() {
                      return liftEffect6(function __do2() {
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
                    var $63 = render11(lchs);
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
                return unComponentSlot(function(slot3) {
                  return function __do2() {
                    var childrenIn = map36(slot3.pop)(read(childrenInRef))();
                    var $$var2 = function() {
                      if (childrenIn instanceof Just) {
                        write(childrenIn.value0.value1)(childrenInRef)();
                        var dsx = read(childrenIn.value0.value0)();
                        unDriverStateX(function(st) {
                          return function __do3() {
                            flip(write)(st.handlerRef)(function() {
                              var $65 = maybe(pure14(unit))(handler3);
                              return function($66) {
                                return $65(slot3.output($66));
                              };
                            }())();
                            return handleAff(evalM(render11)(st.selfRef)(st["component"]["eval"](new Receive(slot3.input, unit))))();
                          };
                        })(dsx)();
                        return childrenIn.value0.value0;
                      }
                      ;
                      if (childrenIn instanceof Nothing) {
                        return runComponent(lchs)(function() {
                          var $67 = maybe(pure14(unit))(handler3);
                          return function($68) {
                            return $67(slot3.output($68));
                          };
                        }())(slot3.input)(slot3.component)();
                      }
                      ;
                      throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                    }();
                    var isDuplicate = map36(function($69) {
                      return isJust(slot3.get($69));
                    })(read(childrenOutRef))();
                    when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                    modify_2(slot3.set($$var2))(childrenOutRef)();
                    return bind11(read($$var2))(renderStateX2(function(v) {
                      if (v instanceof Nothing) {
                        return $$throw("Halogen internal error: child was not initialized in renderChild");
                      }
                      ;
                      if (v instanceof Just) {
                        return pure13(renderSpec2.renderChild(v.value0));
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
        var render11 = function(lchs) {
          return function($$var2) {
            return function __do2() {
              var v = read($$var2)();
              var shouldProcessHandlers = map36(isNothing)(read(v.pendingHandlers))();
              when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
              write(empty6)(v.childrenOut)();
              write(v.children)(v.childrenIn)();
              var handler3 = function() {
                var $70 = queueOrRun(v.pendingHandlers);
                var $71 = evalF(render11)(v.selfRef);
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
              flip(modify_2)(v.selfRef)(mapDriverState(function(ds$prime) {
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
                      return handleAff($76(reverse2($77)));
                    };
                  }())(handlers)();
                  var mmore = read(v.pendingHandlers)();
                  var $52 = maybe(false)($$null2)(mmore);
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
              var f = evalM(render11)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
              modify_2(function(handlers) {
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
              return bind14(liftEffect6(read(disposed)))(function(v) {
                if (v) {
                  return pure14(Nothing.value);
                }
                ;
                return evalQ(render11)(ref2)(q2);
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
        return bind14(liftEffect6(newLifecycleHandlers))(function(lchs) {
          return bind14(liftEffect6($$new(false)))(function(disposed) {
            return handleLifecycle(lchs)(function __do2() {
              var sio = create();
              var dsx = bindFlipped7(read)(runComponent(lchs)(function() {
                var $78 = notify(sio.listener);
                return function($79) {
                  return liftEffect6($78($79));
                };
              }())(i2)(component7))();
              return unDriverStateX(function(st) {
                return pure13({
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
  var getEffProp2 = function(name16) {
    return function(node) {
      return function() {
        return node[name16];
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
  var map37 = /* @__PURE__ */ map(functorEffect);
  var parentNode2 = /* @__PURE__ */ function() {
    var $6 = map37(toMaybe);
    return function($7) {
      return $6(_parentNode($7));
    };
  }();
  var nextSibling = /* @__PURE__ */ function() {
    var $15 = map37(toMaybe);
    return function($16) {
      return $15(_nextSibling($16));
    };
  }();

  // output/Halogen.VDom.Driver/index.js
  var $runtime_lazy10 = function(name16, moduleName, init3) {
    var state3 = 0;
    var val;
    return function(lineNumber) {
      if (state3 === 2)
        return val;
      if (state3 === 1)
        throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state3 = 1;
      val = init3();
      state3 = 2;
      return val;
    };
  };
  var $$void7 = /* @__PURE__ */ $$void(functorEffect);
  var pure15 = /* @__PURE__ */ pure(applicativeEffect);
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var unwrap6 = /* @__PURE__ */ unwrap();
  var when3 = /* @__PURE__ */ when(applicativeEffect);
  var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
  var identity9 = /* @__PURE__ */ identity(categoryFn);
  var bind15 = /* @__PURE__ */ bind(bindAff);
  var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
  var map38 = /* @__PURE__ */ map(functorEffect);
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
        return pure15(unit);
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
      return function(document7) {
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
            return function(st, slot3) {
              if (st instanceof Just) {
                if (slot3 instanceof ComponentSlot) {
                  halt(st.value0);
                  return $lazy_renderComponentSlot(100)(slot3.value0);
                }
                ;
                if (slot3 instanceof ThunkSlot) {
                  var step$prime = step(st.value0, slot3.value0);
                  return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
                }
                ;
                throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot3.constructor.name]);
              }
              ;
              return $lazy_render(104)(slot3);
            };
          });
          var $lazy_render = $runtime_lazy10("render", "Halogen.VDom.Driver", function() {
            return function(slot3) {
              if (slot3 instanceof ComponentSlot) {
                return $lazy_renderComponentSlot(86)(slot3.value0);
              }
              ;
              if (slot3 instanceof ThunkSlot) {
                var step3 = buildThunk2(slot3.value0);
                return mkStep(new Step(extract2(step3), new Just(step3), $lazy_patch(89), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot3.constructor.name]);
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
          var render11 = $lazy_render(82);
          var renderComponentSlot = $lazy_renderComponentSlot(109);
          return render11;
        };
        var buildAttributes = buildProp(handler3);
        return {
          buildWidget: buildWidget2,
          buildAttributes,
          document: document7
        };
      };
    };
  };
  var renderSpec = function(document7) {
    return function(container) {
      var render11 = function(handler3) {
        return function(child) {
          return function(v) {
            return function(v1) {
              if (v1 instanceof Nothing) {
                return function __do2() {
                  var renderChildRef = $$new(child)();
                  var spec = mkSpec(handler3)(renderChildRef)(document7);
                  var machine = buildVDom(spec)(v);
                  var node = extract2(machine);
                  $$void7(appendChild(node)(toNode2(container)))();
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
                  var machine$prime = step(v1.value0.machine, v);
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
        render: render11,
        renderChild: identity9,
        removeChild: removeChild3,
        dispose: removeChild3
      };
    };
  };
  var runUI2 = function(component7) {
    return function(i2) {
      return function(element3) {
        return bind15(liftEffect7(map38(toDocument)(bindFlipped8(document6)(windowImpl))))(function(document7) {
          return runUI(renderSpec(document7)(element3))(component7)(i2);
        });
      };
    };
  };

  // output/Sandbox.Main/index.js
  var component6 = /* @__PURE__ */ component5(monadAffAff)(monadErrorAff);
  var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
    return runUI2(component6(commandFormComponent))(unit)(body2);
  }));

  // docs/sandbox.js
  main2();
})();
