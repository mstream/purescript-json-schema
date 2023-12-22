(() => {
  // output-es/runtime.js
  function binding(init) {
    let state = 0;
    let value4;
    return () => {
      if (state === 2) {
        return value4;
      }
      if (state === 1) {
        throw new Error("Binding demanded before initialized");
      }
      state = 1;
      value4 = init();
      state = 2;
      return value4;
    };
  }
  function fail() {
    throw new Error("Failed pattern match");
  }
  function intDiv(x, y) {
    if (y > 0)
      return Math.floor(x / y);
    if (y < 0)
      return -Math.floor(x / -y);
    return 0;
  }

  // output-es/Record.Unsafe/foreign.js
  var unsafeGet = function(label) {
    return function(rec) {
      return rec[label];
    };
  };

  // output-es/Type.Proxy/index.js
  var $$$Proxy = () => ({ tag: "Proxy" });
  var $$Proxy = /* @__PURE__ */ $$$Proxy();

  // output-es/Data.Show/foreign.js
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
      function(c, i) {
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
        var k = i + 1;
        var empty2 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty2;
      }
    ) + '"';
  };

  // output-es/Data.Ordering/index.js
  var $Ordering = (tag) => tag;
  var LT = /* @__PURE__ */ $Ordering("LT");
  var GT = /* @__PURE__ */ $Ordering("GT");
  var EQ = /* @__PURE__ */ $Ordering("EQ");

  // output-es/Data.Maybe/index.js
  var $Maybe = (tag, _1) => ({ tag, _1 });
  var Nothing = /* @__PURE__ */ $Maybe("Nothing");
  var Just = (value0) => $Maybe("Just", value0);
  var isNothing = (v2) => {
    if (v2.tag === "Nothing") {
      return true;
    }
    if (v2.tag === "Just") {
      return false;
    }
    fail();
  };
  var functorMaybe = {
    map: (v) => (v1) => {
      if (v1.tag === "Just") {
        return $Maybe("Just", v(v1._1));
      }
      return Nothing;
    }
  };
  var ordMaybe = (dictOrd) => {
    const $0 = dictOrd.Eq0();
    const eqMaybe1 = {
      eq: (x) => (y) => {
        if (x.tag === "Nothing") {
          return y.tag === "Nothing";
        }
        return x.tag === "Just" && y.tag === "Just" && $0.eq(x._1)(y._1);
      }
    };
    return {
      compare: (x) => (y) => {
        if (x.tag === "Nothing") {
          if (y.tag === "Nothing") {
            return EQ;
          }
          return LT;
        }
        if (y.tag === "Nothing") {
          return GT;
        }
        if (x.tag === "Just" && y.tag === "Just") {
          return dictOrd.compare(x._1)(y._1);
        }
        fail();
      },
      Eq0: () => eqMaybe1
    };
  };

  // output-es/Data.Function/index.js
  var $$const = (a) => (v) => a;

  // output-es/Data.Functor/foreign.js
  var arrayMap = function(f) {
    return function(arr) {
      var l = arr.length;
      var result = new Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(arr[i]);
      }
      return result;
    };
  };

  // output-es/Data.Functor/index.js
  var functorArray = { map: arrayMap };

  // output-es/Control.Apply/index.js
  var identity = (x) => x;

  // output-es/Data.Either/index.js
  var $Either = (tag, _1) => ({ tag, _1 });
  var Left = (value0) => $Either("Left", value0);
  var Right = (value0) => $Either("Right", value0);
  var functorEither = {
    map: (f) => (m) => {
      if (m.tag === "Left") {
        return $Either("Left", m._1);
      }
      if (m.tag === "Right") {
        return $Either("Right", f(m._1));
      }
      fail();
    }
  };
  var applyEither = {
    apply: (v) => (v1) => {
      if (v.tag === "Left") {
        return $Either("Left", v._1);
      }
      if (v.tag === "Right") {
        if (v1.tag === "Left") {
          return $Either("Left", v1._1);
        }
        if (v1.tag === "Right") {
          return $Either("Right", v._1(v1._1));
        }
      }
      fail();
    },
    Functor0: () => functorEither
  };
  var applicativeEither = { pure: Right, Apply0: () => applyEither };

  // output-es/Data.Identity/index.js
  var Identity = (x) => x;
  var functorIdentity = { map: (f) => (m) => f(m) };
  var applyIdentity = { apply: (v) => (v1) => v(v1), Functor0: () => functorIdentity };
  var bindIdentity = { bind: (v) => (f) => f(v), Apply0: () => applyIdentity };
  var applicativeIdentity = { pure: Identity, Apply0: () => applyIdentity };
  var monadIdentity = { Applicative0: () => applicativeIdentity, Bind1: () => bindIdentity };

  // output-es/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };
  var untilE = function(f) {
    return function() {
      while (!f())
        ;
    };
  };

  // output-es/Effect/index.js
  var monadEffect = { Applicative0: () => applicativeEffect, Bind1: () => bindEffect };
  var bindEffect = { bind: bindE, Apply0: () => applyEffect };
  var applyEffect = {
    apply: (f) => (a) => () => {
      const f$p = f();
      const a$p = a();
      return applicativeEffect.pure(f$p(a$p))();
    },
    Functor0: () => functorEffect
  };
  var applicativeEffect = { pure: pureE, Apply0: () => applyEffect };
  var functorEffect = {
    map: (f) => (a) => () => {
      const a$p = a();
      return f(a$p);
    }
  };

  // output-es/Control.Monad.Rec.Class/index.js
  var $Step = (tag, _1) => ({ tag, _1 });
  var Done = (value0) => $Step("Done", value0);
  var monadRecEffect = {
    tailRecM: (f) => (a) => {
      const $0 = f(a);
      return () => {
        const $1 = $0();
        let r = $1;
        untilE(() => {
          const v = r;
          if (v.tag === "Loop") {
            const e = f(v._1)();
            r = e;
            return false;
          }
          if (v.tag === "Done") {
            return true;
          }
          fail();
        })();
        const a$p = r;
        if (a$p.tag === "Done") {
          return a$p._1;
        }
        fail();
      };
    },
    Monad0: () => monadEffect
  };

  // output-es/Data.Foldable/foreign.js
  var foldrArray = function(f) {
    return function(init) {
      return function(xs) {
        var acc = init;
        var len = xs.length;
        for (var i = len - 1; i >= 0; i--) {
          acc = f(xs[i])(acc);
        }
        return acc;
      };
    };
  };
  var foldlArray = function(f) {
    return function(init) {
      return function(xs) {
        var acc = init;
        var len = xs.length;
        for (var i = 0; i < len; i++) {
          acc = f(acc)(xs[i]);
        }
        return acc;
      };
    };
  };

  // output-es/Data.Foldable/index.js
  var traverse_ = (dictApplicative) => {
    const $0 = dictApplicative.Apply0();
    return (dictFoldable) => (f) => dictFoldable.foldr((x) => {
      const $1 = f(x);
      return (b) => $0.apply($0.Functor0().map((v) => identity)($1))(b);
    })(dictApplicative.pure());
  };
  var for_ = (dictApplicative) => {
    const traverse_14 = traverse_(dictApplicative);
    return (dictFoldable) => {
      const $0 = traverse_14(dictFoldable);
      return (b) => (a) => $0(a)(b);
    };
  };
  var foldableMaybe = {
    foldr: (v) => (v1) => (v2) => {
      if (v2.tag === "Nothing") {
        return v1;
      }
      if (v2.tag === "Just") {
        return v(v2._1)(v1);
      }
      fail();
    },
    foldl: (v) => (v1) => (v2) => {
      if (v2.tag === "Nothing") {
        return v1;
      }
      if (v2.tag === "Just") {
        return v(v1)(v2._1);
      }
      fail();
    },
    foldMap: (dictMonoid) => {
      const mempty = dictMonoid.mempty;
      return (v) => (v1) => {
        if (v1.tag === "Nothing") {
          return mempty;
        }
        if (v1.tag === "Just") {
          return v(v1._1);
        }
        fail();
      };
    }
  };
  var foldableArray = {
    foldr: foldrArray,
    foldl: foldlArray,
    foldMap: (dictMonoid) => {
      const mempty = dictMonoid.mempty;
      return (f) => foldableArray.foldr((x) => (acc) => dictMonoid.Semigroup0().append(f(x))(acc))(mempty);
    }
  };

  // output-es/Data.Tuple/index.js
  var $Tuple = (_1, _2) => ({ tag: "Tuple", _1, _2 });
  var Tuple = (value0) => (value1) => $Tuple(value0, value1);
  var snd = (v) => v._2;
  var fst = (v) => v._1;
  var ordTuple = (dictOrd) => {
    const $0 = dictOrd.Eq0();
    return (dictOrd1) => {
      const $1 = dictOrd1.Eq0();
      const eqTuple2 = { eq: (x) => (y) => $0.eq(x._1)(y._1) && $1.eq(x._2)(y._2) };
      return {
        compare: (x) => (y) => {
          const v = dictOrd.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return dictOrd1.compare(x._2)(y._2);
        },
        Eq0: () => eqTuple2
      };
    };
  };

  // output-es/Data.FunctorWithIndex/foreign.js
  var mapWithIndexArray = function(f) {
    return function(xs) {
      var l = xs.length;
      var result = Array(l);
      for (var i = 0; i < l; i++) {
        result[i] = f(i)(xs[i]);
      }
      return result;
    };
  };

  // output-es/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqBooleanImpl = refEq;
  var eqIntImpl = refEq;
  var eqNumberImpl = refEq;
  var eqStringImpl = refEq;

  // output-es/Data.Eq/index.js
  var eqUnit = { eq: (v) => (v1) => true };
  var eqString = { eq: eqStringImpl };
  var eqNumber = { eq: eqNumberImpl };
  var eqInt = { eq: eqIntImpl };
  var eqBoolean = { eq: eqBooleanImpl };

  // output-es/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq : gt;
          };
        };
      };
    };
  };
  var ordBooleanImpl = unsafeCompareImpl;
  var ordIntImpl = unsafeCompareImpl;
  var ordNumberImpl = unsafeCompareImpl;
  var ordStringImpl = unsafeCompareImpl;

  // output-es/Data.Ord/index.js
  var ordUnit = { compare: (v) => (v1) => EQ, Eq0: () => eqUnit };
  var ordString = { compare: /* @__PURE__ */ ordStringImpl(LT)(EQ)(GT), Eq0: () => eqString };
  var ordNumber = { compare: /* @__PURE__ */ ordNumberImpl(LT)(EQ)(GT), Eq0: () => eqNumber };
  var ordInt = { compare: /* @__PURE__ */ ordIntImpl(LT)(EQ)(GT), Eq0: () => eqInt };
  var ordBoolean = { compare: /* @__PURE__ */ ordBooleanImpl(LT)(EQ)(GT), Eq0: () => eqBoolean };
  var ordRecord = () => (dictOrdRecord) => {
    const eqRec1 = { eq: dictOrdRecord.EqRecord0().eqRecord($$Proxy) };
    return { compare: dictOrdRecord.compareRecord($$Proxy), Eq0: () => eqRec1 };
  };

  // output-es/Unsafe.Coerce/foreign.js
  var unsafeCoerce = function(x) {
    return x;
  };

  // output-es/Data.Traversable/foreign.js
  var traverseArrayImpl = /* @__PURE__ */ function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply) {
      return function(map2) {
        return function(pure) {
          return function(f) {
            return function(array) {
              function go(bot, top) {
                switch (top - bot) {
                  case 0:
                    return pure([]);
                  case 1:
                    return map2(array1)(f(array[bot]));
                  case 2:
                    return apply(map2(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply(apply(map2(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top - bot) / 4) * 2;
                    return apply(map2(concat2)(go(bot, pivot)))(go(pivot, top));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output-es/Data.Traversable/index.js
  var identity2 = (x) => x;
  var traversableArray = {
    traverse: (dictApplicative) => {
      const Apply0 = dictApplicative.Apply0();
      return traverseArrayImpl(Apply0.apply)(Apply0.Functor0().map)(dictApplicative.pure);
    },
    sequence: (dictApplicative) => traversableArray.traverse(dictApplicative)(identity2),
    Functor0: () => functorArray,
    Foldable1: () => foldableArray
  };

  // output-es/Data.Array/foreign.js
  var replicateFill = function(count, value4) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value4);
  };
  var replicatePolyfill = function(count, value4) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value4;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = /* @__PURE__ */ function() {
    function Cons2(head, tail) {
      this.head = head;
      this.tail = tail;
    }
    var emptyList = {};
    function curryCons(head) {
      return function(tail) {
        return new Cons2(head, tail);
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
    return function(foldr2, xs) {
      return listToArray(foldr2(curryCons)(emptyList)(xs));
    };
  }();
  var findIndexImpl = function(just, nothing, f, xs) {
    for (var i = 0, l = xs.length; i < l; i++) {
      if (f(xs[i]))
        return just(i);
    }
    return nothing;
  };
  var _deleteAt = function(just, nothing, i, l) {
    if (i < 0 || i >= l.length)
      return nothing;
    var l1 = l.slice();
    l1.splice(i, 1);
    return just(l1);
  };
  var reverse = function(l) {
    return l.slice().reverse();
  };

  // output-es/Data.Array/index.js
  var deleteBy = (v) => (v1) => (v2) => {
    if (v2.length === 0) {
      return [];
    }
    const $0 = findIndexImpl(Just, Nothing, v(v1), v2);
    if ($0.tag === "Nothing") {
      return v2;
    }
    if ($0.tag === "Just") {
      const $1 = _deleteAt(Just, Nothing, $0._1, v2);
      if ($1.tag === "Just") {
        return $1._1;
      }
    }
    fail();
  };

  // output-es/Data.FoldableWithIndex/index.js
  var foldableWithIndexArray = {
    foldrWithIndex: (f) => (z) => {
      const $0 = foldrArray((v) => {
        const $02 = v._1;
        const $12 = v._2;
        return (y) => f($02)($12)(y);
      })(z);
      const $1 = mapWithIndexArray(Tuple);
      return (x) => $0($1(x));
    },
    foldlWithIndex: (f) => (z) => {
      const $0 = foldlArray((y) => (v) => f(v._1)(y)(v._2))(z);
      const $1 = mapWithIndexArray(Tuple);
      return (x) => $0($1(x));
    },
    foldMapWithIndex: (dictMonoid) => {
      const mempty = dictMonoid.mempty;
      return (f) => foldableWithIndexArray.foldrWithIndex((i) => (x) => (acc) => dictMonoid.Semigroup0().append(f(i)(x))(acc))(mempty);
    },
    Foldable0: () => foldableArray
  };

  // output-es/Data.Unfoldable1/foreign.js
  var unfoldr1ArrayImpl = function(isNothing2) {
    return function(fromJust3) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value4 = b;
              while (true) {
                var tuple = f(value4);
                result.push(fst2(tuple));
                var maybe = snd2(tuple);
                if (isNothing2(maybe))
                  return result;
                value4 = fromJust3(maybe);
              }
            };
          };
        };
      };
    };
  };

  // output-es/Data.Unfoldable1/index.js
  var fromJust = (v) => {
    if (v.tag === "Just") {
      return v._1;
    }
    fail();
  };
  var unfoldable1Array = { unfoldr1: /* @__PURE__ */ unfoldr1ArrayImpl(isNothing)(fromJust)(fst)(snd) };

  // output-es/Data.Unfoldable/foreign.js
  var unfoldrArrayImpl = function(isNothing2) {
    return function(fromJust3) {
      return function(fst2) {
        return function(snd2) {
          return function(f) {
            return function(b) {
              var result = [];
              var value4 = b;
              while (true) {
                var maybe = f(value4);
                if (isNothing2(maybe))
                  return result;
                var tuple = fromJust3(maybe);
                result.push(fst2(tuple));
                value4 = snd2(tuple);
              }
            };
          };
        };
      };
    };
  };

  // output-es/Data.Unfoldable/index.js
  var fromJust2 = (v) => {
    if (v.tag === "Just") {
      return v._1;
    }
    fail();
  };
  var unfoldableArray = {
    unfoldr: /* @__PURE__ */ unfoldrArrayImpl(isNothing)(fromJust2)(fst)(snd),
    Unfoldable10: () => unfoldable1Array
  };

  // output-es/Foreign.Object/foreign.js
  var empty = {};
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

  // output-es/Foreign.Object/index.js
  var mutate = (f) => (m) => {
    const s = { ...m };
    f(s)();
    return s;
  };
  var fromFoldable = (dictFoldable) => {
    const $0 = dictFoldable.foldr;
    return (l) => {
      const s = {};
      for (const v of fromFoldableImpl($0, l)) {
        s[v._1] = v._2;
      }
      return s;
    };
  };

  // output-es/Data.Argonaut.Core/foreign.js
  function id(x) {
    return x;
  }
  var jsonNull = null;
  function stringifyWithIndent(i) {
    return function(j) {
      return JSON.stringify(j, null, i);
    };
  }
  function isArray(a) {
    return Object.prototype.toString.call(a) === "[object Array]";
  }
  function _caseJson(isNull2, isBool, isNum, isStr, isArr, isObj, j) {
    if (j == null)
      return isNull2();
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
  function _compare(EQ2, GT2, LT2, a, b) {
    if (a == null) {
      if (b == null)
        return EQ2;
      else
        return LT2;
    } else if (typeof a === "boolean") {
      if (typeof b === "boolean") {
        if (a === b)
          return EQ2;
        else if (a === false)
          return LT2;
        else
          return GT2;
      } else if (b == null)
        return GT2;
      else
        return LT2;
    } else if (typeof a === "number") {
      if (typeof b === "number") {
        if (a === b)
          return EQ2;
        else if (a < b)
          return LT2;
        else
          return GT2;
      } else if (b == null)
        return GT2;
      else if (typeof b === "boolean")
        return GT2;
      else
        return LT2;
    } else if (typeof a === "string") {
      if (typeof b === "string") {
        if (a === b)
          return EQ2;
        else if (a < b)
          return LT2;
        else
          return GT2;
      } else if (b == null)
        return GT2;
      else if (typeof b === "boolean")
        return GT2;
      else if (typeof b === "number")
        return GT2;
      else
        return LT2;
    } else if (isArray(a)) {
      if (isArray(b)) {
        for (var i = 0; i < Math.min(a.length, b.length); i++) {
          var ca = _compare(EQ2, GT2, LT2, a[i], b[i]);
          if (ca !== EQ2)
            return ca;
        }
        if (a.length === b.length)
          return EQ2;
        else if (a.length < b.length)
          return LT2;
        else
          return GT2;
      } else if (b == null)
        return GT2;
      else if (typeof b === "boolean")
        return GT2;
      else if (typeof b === "number")
        return GT2;
      else if (typeof b === "string")
        return GT2;
      else
        return LT2;
    } else {
      if (b == null)
        return GT2;
      else if (typeof b === "boolean")
        return GT2;
      else if (typeof b === "number")
        return GT2;
      else if (typeof b === "string")
        return GT2;
      else if (isArray(b))
        return GT2;
      else {
        var akeys = Object.keys(a);
        var bkeys = Object.keys(b);
        if (akeys.length < bkeys.length)
          return LT2;
        else if (akeys.length > bkeys.length)
          return GT2;
        var keys4 = akeys.concat(bkeys).sort();
        for (var j = 0; j < keys4.length; j++) {
          var k = keys4[j];
          if (a[k] === void 0)
            return LT2;
          else if (b[k] === void 0)
            return GT2;
          var ck = _compare(EQ2, GT2, LT2, a[k], b[k]);
          if (ck !== EQ2)
            return ck;
        }
        return EQ2;
      }
    }
  }

  // output-es/Data.Argonaut.Core/index.js
  var jsonSingletonObject = (key) => (val) => id((() => {
    const $0 = {};
    $0[key] = val;
    return $0;
  })());
  var jsonEmptyString = /* @__PURE__ */ id("");
  var jsonEmptyObject = /* @__PURE__ */ id(empty);
  var ordJson = { compare: (a) => (b) => _compare(EQ, GT, LT, a, b), Eq0: () => eqJson };
  var eqJson = { eq: (j1) => (j2) => _compare(EQ, GT, LT, j1, j2) === "EQ" };
  var caseJsonObject = (d) => (f) => (j) => _caseJson((v) => d, (v) => d, (v) => d, (v) => d, (v) => d, f, j);

  // output-es/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output-es/Data.Semigroup/index.js
  var semigroupArray = { append: concatArray };

  // output-es/Data.Array.NonEmpty.Internal/foreign.js
  var foldr1Impl = function(f, xs) {
    var acc = xs[xs.length - 1];
    for (var i = xs.length - 2; i >= 0; i--) {
      acc = f(xs[i])(acc);
    }
    return acc;
  };
  var foldl1Impl = function(f, xs) {
    var acc = xs[0];
    var len = xs.length;
    for (var i = 1; i < len; i++) {
      acc = f(acc)(xs[i]);
    }
    return acc;
  };

  // output-es/Data.Array.NonEmpty.Internal/index.js
  var foldable1NonEmptyArray = {
    foldMap1: (dictSemigroup) => {
      const append = dictSemigroup.append;
      return (f) => {
        const $0 = arrayMap(f);
        const $1 = foldable1NonEmptyArray.foldl1(append);
        return (x) => $1($0(x));
      };
    },
    foldr1: ($0) => ($1) => foldr1Impl($0, $1),
    foldl1: ($0) => ($1) => foldl1Impl($0, $1),
    Foldable0: () => foldableArray
  };

  // output-es/Data.NonEmpty/index.js
  var $NonEmpty = (_1, _2) => ({ tag: "NonEmpty", _1, _2 });
  var unfoldable1NonEmpty = (dictUnfoldable) => ({
    unfoldr1: (f) => (b) => {
      const $0 = f(b);
      return $NonEmpty($0._1, dictUnfoldable.unfoldr(functorMaybe.map(f))($0._2));
    }
  });
  var foldable1NonEmpty = (dictFoldable) => {
    const foldableNonEmpty1 = {
      foldMap: (dictMonoid) => {
        const foldMap13 = dictFoldable.foldMap(dictMonoid);
        return (f) => (v) => dictMonoid.Semigroup0().append(f(v._1))(foldMap13(f)(v._2));
      },
      foldl: (f) => (b) => (v) => dictFoldable.foldl(f)(f(b)(v._1))(v._2),
      foldr: (f) => (b) => (v) => f(v._1)(dictFoldable.foldr(f)(b)(v._2))
    };
    return {
      foldMap1: (dictSemigroup) => (f) => (v) => dictFoldable.foldl((s) => (a1) => dictSemigroup.append(s)(f(a1)))(f(v._1))(v._2),
      foldr1: (f) => (v) => {
        const $0 = f(v._1);
        const $1 = dictFoldable.foldr((a1) => {
          const $12 = f(a1);
          return (x) => $Maybe(
            "Just",
            (() => {
              if (x.tag === "Nothing") {
                return a1;
              }
              if (x.tag === "Just") {
                return $12(x._1);
              }
              fail();
            })()
          );
        })(Nothing)(v._2);
        if ($1.tag === "Nothing") {
          return v._1;
        }
        if ($1.tag === "Just") {
          return $0($1._1);
        }
        fail();
      },
      foldl1: (f) => (v) => dictFoldable.foldl(f)(v._1)(v._2),
      Foldable0: () => foldableNonEmpty1
    };
  };

  // output-es/Data.List.Types/index.js
  var $List = (tag, _1, _2) => ({ tag, _1, _2 });
  var Nil = /* @__PURE__ */ $List("Nil");
  var Cons = (value0) => (value1) => $List("Cons", value0, value1);
  var listMap = (f) => {
    const chunkedRevMap = (chunkedRevMap$a0$copy) => (chunkedRevMap$a1$copy) => {
      let chunkedRevMap$a0 = chunkedRevMap$a0$copy, chunkedRevMap$a1 = chunkedRevMap$a1$copy, chunkedRevMap$c = true, chunkedRevMap$r;
      while (chunkedRevMap$c) {
        const v = chunkedRevMap$a0, v1 = chunkedRevMap$a1;
        if (v1.tag === "Cons" && v1._2.tag === "Cons" && v1._2._2.tag === "Cons") {
          chunkedRevMap$a0 = $List("Cons", v1, v);
          chunkedRevMap$a1 = v1._2._2._2;
          continue;
        }
        const reverseUnrolledMap = (reverseUnrolledMap$a0$copy) => (reverseUnrolledMap$a1$copy) => {
          let reverseUnrolledMap$a0 = reverseUnrolledMap$a0$copy, reverseUnrolledMap$a1 = reverseUnrolledMap$a1$copy, reverseUnrolledMap$c = true, reverseUnrolledMap$r;
          while (reverseUnrolledMap$c) {
            const v2 = reverseUnrolledMap$a0, v3 = reverseUnrolledMap$a1;
            if (v2.tag === "Cons" && v2._1.tag === "Cons" && v2._1._2.tag === "Cons" && v2._1._2._2.tag === "Cons") {
              reverseUnrolledMap$a0 = v2._2;
              reverseUnrolledMap$a1 = $List("Cons", f(v2._1._1), $List("Cons", f(v2._1._2._1), $List("Cons", f(v2._1._2._2._1), v3)));
              continue;
            }
            reverseUnrolledMap$c = false;
            reverseUnrolledMap$r = v3;
          }
          return reverseUnrolledMap$r;
        };
        chunkedRevMap$c = false;
        chunkedRevMap$r = reverseUnrolledMap(v)((() => {
          if (v1.tag === "Cons") {
            if (v1._2.tag === "Cons") {
              if (v1._2._2.tag === "Nil") {
                return $List("Cons", f(v1._1), $List("Cons", f(v1._2._1), Nil));
              }
              return Nil;
            }
            if (v1._2.tag === "Nil") {
              return $List("Cons", f(v1._1), Nil);
            }
          }
          return Nil;
        })());
      }
      return chunkedRevMap$r;
    };
    return chunkedRevMap(Nil);
  };
  var foldableList = {
    foldr: (f) => (b) => {
      const $0 = foldableList.foldl((b$1) => (a) => f(a)(b$1))(b);
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const v = go$a0, v1 = go$a1;
          if (v1.tag === "Nil") {
            go$c = false;
            go$r = v;
            continue;
          }
          if (v1.tag === "Cons") {
            go$a0 = $List("Cons", v1._1, v);
            go$a1 = v1._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      const $1 = go(Nil);
      return (x) => $0($1(x));
    },
    foldl: (f) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const b = go$a0, v = go$a1;
          if (v.tag === "Nil") {
            go$c = false;
            go$r = b;
            continue;
          }
          if (v.tag === "Cons") {
            go$a0 = f(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go;
    },
    foldMap: (dictMonoid) => {
      const mempty = dictMonoid.mempty;
      return (f) => foldableList.foldl((acc) => {
        const $0 = dictMonoid.Semigroup0().append(acc);
        return (x) => $0(f(x));
      })(mempty);
    }
  };
  var foldableNonEmptyList = {
    foldMap: (dictMonoid) => {
      const foldMap13 = foldableList.foldMap(dictMonoid);
      return (f) => (v) => dictMonoid.Semigroup0().append(f(v._1))(foldMap13(f)(v._2));
    },
    foldl: (f) => (b) => (v) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const b$1 = go$a0, v$1 = go$a1;
          if (v$1.tag === "Nil") {
            go$c = false;
            go$r = b$1;
            continue;
          }
          if (v$1.tag === "Cons") {
            go$a0 = f(b$1)(v$1._1);
            go$a1 = v$1._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(f(b)(v._1))(v._2);
    },
    foldr: (f) => (b) => (v) => f(v._1)(foldableList.foldr(f)(b)(v._2))
  };
  var semigroupNonEmptyList = { append: (v) => (as$p) => $NonEmpty(v._1, foldableList.foldr(Cons)($List("Cons", as$p._1, as$p._2))(v._2)) };
  var unfoldable1List = {
    unfoldr1: (f) => (b) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const source = go$a0, memo = go$a1;
          const v = f(source);
          if (v._2.tag === "Just") {
            go$a0 = v._2._1;
            go$a1 = $List("Cons", v._1, memo);
            continue;
          }
          if (v._2.tag === "Nothing") {
            const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const b$1 = go$1$a0, v$1 = go$1$a1;
                if (v$1.tag === "Nil") {
                  go$1$c = false;
                  go$1$r = b$1;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$1$a0 = $List("Cons", v$1._1, b$1);
                  go$1$a1 = v$1._2;
                  continue;
                }
                fail();
              }
              return go$1$r;
            };
            go$c = false;
            go$r = go$1(Nil)($List("Cons", v._1, memo));
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(b)(Nil);
    }
  };
  var unfoldableList = {
    unfoldr: (f) => (b) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const source = go$a0, memo = go$a1;
          const v = f(source);
          if (v.tag === "Nothing") {
            const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const b$1 = go$1$a0, v$1 = go$1$a1;
                if (v$1.tag === "Nil") {
                  go$1$c = false;
                  go$1$r = b$1;
                  continue;
                }
                if (v$1.tag === "Cons") {
                  go$1$a0 = $List("Cons", v$1._1, b$1);
                  go$1$a1 = v$1._2;
                  continue;
                }
                fail();
              }
              return go$1$r;
            };
            go$c = false;
            go$r = go$1(Nil)(memo);
            continue;
          }
          if (v.tag === "Just") {
            go$a0 = v._1._2;
            go$a1 = $List("Cons", v._1._1, memo);
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(b)(Nil);
    },
    Unfoldable10: () => unfoldable1List
  };
  var unfoldable1NonEmptyList = /* @__PURE__ */ unfoldable1NonEmpty(unfoldableList);
  var foldable1NonEmptyList = /* @__PURE__ */ foldable1NonEmpty(foldableList);
  var eq1List = {
    eq1: (dictEq) => (xs) => (ys) => {
      const go = (v) => (v1) => (v2) => {
        if (!v2) {
          return false;
        }
        if (v.tag === "Nil") {
          return v1.tag === "Nil" && v2;
        }
        return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && dictEq.eq(v1._1)(v._1));
      };
      return go(xs)(ys)(true);
    }
  };
  var ord1List = {
    compare1: (dictOrd) => (xs) => (ys) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const v = go$a0, v1 = go$a1;
          if (v.tag === "Nil") {
            if (v1.tag === "Nil") {
              go$c = false;
              go$r = EQ;
              continue;
            }
            go$c = false;
            go$r = LT;
            continue;
          }
          if (v1.tag === "Nil") {
            go$c = false;
            go$r = GT;
            continue;
          }
          if (v.tag === "Cons" && v1.tag === "Cons") {
            const v2 = dictOrd.compare(v._1)(v1._1);
            if (v2 === "EQ") {
              go$a0 = v._2;
              go$a1 = v1._2;
              continue;
            }
            go$c = false;
            go$r = v2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      return go(xs)(ys);
    },
    Eq10: () => eq1List
  };
  var ordList = (dictOrd) => {
    const $0 = dictOrd.Eq0();
    const eqList1 = {
      eq: (xs) => (ys) => {
        const go = (v) => (v1) => (v2) => {
          if (!v2) {
            return false;
          }
          if (v.tag === "Nil") {
            return v1.tag === "Nil" && v2;
          }
          return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v2 && $0.eq(v1._1)(v._1));
        };
        return go(xs)(ys)(true);
      }
    };
    return { compare: ord1List.compare1(dictOrd), Eq0: () => eqList1 };
  };

  // output-es/Data.List/index.js
  var toUnfoldable2 = (dictUnfoldable) => dictUnfoldable.unfoldr((xs) => {
    if (xs.tag === "Nil") {
      return Nothing;
    }
    if (xs.tag === "Cons") {
      return $Maybe("Just", $Tuple(xs._1, xs._2));
    }
    fail();
  });
  var reverse2 = /* @__PURE__ */ (() => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v1.tag === "Nil") {
          go$c = false;
          go$r = v;
          continue;
        }
        if (v1.tag === "Cons") {
          go$a0 = $List("Cons", v1._1, v);
          go$a1 = v1._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    return go(Nil);
  })();
  var unsnoc = (lst) => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0, v1 = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = Nothing;
          continue;
        }
        if (v.tag === "Cons") {
          if (v._2.tag === "Nil") {
            go$c = false;
            go$r = $Maybe("Just", { revInit: v1, last: v._1 });
            continue;
          }
          go$a0 = v._2;
          go$a1 = $List("Cons", v._1, v1);
          continue;
        }
        fail();
      }
      return go$r;
    };
    const $0 = go(lst)(Nil);
    if ($0.tag === "Just") {
      return $Maybe("Just", { init: reverse2($0._1.revInit), last: $0._1.last });
    }
    return Nothing;
  };

  // output-es/Partial/foreign.js
  var _crashWith = function(msg) {
    throw new Error(msg);
  };

  // output-es/Data.List.NonEmpty/index.js
  var wrappedOperation = (name4) => (f) => (v) => {
    const v1 = f($List("Cons", v._1, v._2));
    if (v1.tag === "Cons") {
      return $NonEmpty(v1._1, v1._2);
    }
    if (v1.tag === "Nil") {
      return _crashWith("Impossible: empty list in NonEmptyList " + name4);
    }
    fail();
  };
  var unsnoc2 = (v) => {
    const v1 = unsnoc(v._2);
    if (v1.tag === "Nothing") {
      return { init: Nil, last: v._1 };
    }
    if (v1.tag === "Just") {
      return { init: $List("Cons", v._1, v1._1.init), last: v1._1.last };
    }
    fail();
  };
  var snoc$p = (v) => (v1) => {
    if (v.tag === "Cons") {
      return $NonEmpty(v._1, foldableList.foldr(Cons)($List("Cons", v1, Nil))(v._2));
    }
    if (v.tag === "Nil") {
      return $NonEmpty(v1, Nil);
    }
    fail();
  };
  var appendFoldable = (dictFoldable) => {
    const fromFoldable13 = dictFoldable.foldr(Cons)(Nil);
    return (v) => (ys) => $NonEmpty(v._1, foldableList.foldr(Cons)(fromFoldable13(ys))(v._2));
  };

  // output-es/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };

  // output-es/Data.EuclideanRing/foreign.js
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output-es/Data.String.Common/foreign.js
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

  // output-es/Data.Bounded/foreign.js
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output-es/Data.Enum/foreign.js
  function toCharCode(c) {
    return c.charCodeAt(0);
  }
  function fromCharCode(c) {
    return String.fromCharCode(c);
  }

  // output-es/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var trunc = Math.trunc ? Math.trunc : function(x) {
    return x < 0 ? Math.ceil(x) : Math.floor(x);
  };

  // output-es/Data.Int/foreign.js
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

  // output-es/Data.Int/index.js
  var fromNumber = /* @__PURE__ */ fromNumberImpl(Just)(Nothing);
  var unsafeClamp = (x) => {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    if (x >= toNumber(2147483647)) {
      return 2147483647;
    }
    if (x <= toNumber(-2147483648)) {
      return -2147483648;
    }
    const $0 = fromNumber(x);
    if ($0.tag === "Nothing") {
      return 0;
    }
    if ($0.tag === "Just") {
      return $0._1;
    }
    fail();
  };

  // output-es/Data.String.Unsafe/foreign.js
  var charAt = function(i) {
    return function(s) {
      if (i >= 0 && i < s.length)
        return s.charAt(i);
      throw new Error("Data.String.Unsafe.charAt: Invalid index.");
    };
  };

  // output-es/Data.String.CodeUnits/foreign.js
  var singleton = function(c) {
    return c;
  };
  var length2 = function(s) {
    return s.length;
  };
  var _lastIndexOfStartingAt = function(just) {
    return function(nothing) {
      return function(x) {
        return function(startAt) {
          return function(s) {
            var i = s.lastIndexOf(x, startAt);
            return i === -1 ? nothing : just(i);
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
  var splitAt = function(i) {
    return function(s) {
      return { before: s.substring(0, i), after: s.substring(i) };
    };
  };

  // output-es/Data.String.CodeUnits/index.js
  var stripPrefix = (v) => (str) => {
    const v1 = splitAt(length2(v))(str);
    if (v1.before === v) {
      return $Maybe("Just", v1.after);
    }
    return Nothing;
  };
  var lastIndexOf$p = /* @__PURE__ */ _lastIndexOfStartingAt(Just)(Nothing);

  // output-es/Data.String.CodePoints/foreign.js
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
          for (var i = 0; i < n; ++i) {
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

  // output-es/Data.String.CodePoints/index.js
  var uncons = (s) => {
    const v = length2(s);
    if (v === 0) {
      return Nothing;
    }
    if (v === 1) {
      return $Maybe("Just", { head: toCharCode(charAt(0)(s)), tail: "" });
    }
    const cu1 = toCharCode(charAt(1)(s));
    const cu0 = toCharCode(charAt(0)(s));
    if (55296 <= cu0 && cu0 <= 56319 && 56320 <= cu1 && cu1 <= 57343) {
      return $Maybe("Just", { head: (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0, tail: drop2(2)(s) });
    }
    return $Maybe("Just", { head: cu0, tail: drop2(1)(s) });
  };
  var unconsButWithTuple = (s) => {
    const $0 = uncons(s);
    if ($0.tag === "Just") {
      return $Maybe("Just", $Tuple($0._1.head, $0._1.tail));
    }
    return Nothing;
  };
  var toCodePointArrayFallback = (s) => unfoldableArray.unfoldr(unconsButWithTuple)(s);
  var unsafeCodePointAt0Fallback = (s) => {
    const cu0 = toCharCode(charAt(0)(s));
    if (55296 <= cu0 && cu0 <= 56319 && length2(s) > 1) {
      const cu1 = toCharCode(charAt(1)(s));
      if (56320 <= cu1 && cu1 <= 57343) {
        return (((cu0 - 55296 | 0) * 1024 | 0) + (cu1 - 56320 | 0) | 0) + 65536 | 0;
      }
    }
    return cu0;
  };
  var unsafeCodePointAt0 = /* @__PURE__ */ _unsafeCodePointAt0(unsafeCodePointAt0Fallback);
  var toCodePointArray = /* @__PURE__ */ _toCodePointArray(toCodePointArrayFallback)(unsafeCodePointAt0);
  var fromCharCode2 = (x) => singleton((() => {
    if (x >= 0 && x <= 65535) {
      return fromCharCode(x);
    }
    if (x < 0) {
      return "\0";
    }
    return "\uFFFF";
  })());
  var singletonFallback = (v) => {
    if (v <= 65535) {
      return fromCharCode2(v);
    }
    return fromCharCode2(intDiv(v - 65536 | 0, 1024) + 55296 | 0) + fromCharCode2(intMod(v - 65536 | 0)(1024) + 56320 | 0);
  };
  var singleton2 = /* @__PURE__ */ _singleton(singletonFallback);
  var takeFallback = (v) => (v1) => {
    if (v < 1) {
      return "";
    }
    const v2 = uncons(v1);
    if (v2.tag === "Just") {
      return singleton2(v2._1.head) + takeFallback(v - 1 | 0)(v2._1.tail);
    }
    return v1;
  };
  var take3 = /* @__PURE__ */ _take(takeFallback);
  var lastIndexOf$p2 = (p) => (i) => (s) => {
    const $0 = lastIndexOf$p(p)(length2(take3(i)(s)))(s);
    if ($0.tag === "Just") {
      return $Maybe("Just", toCodePointArray(take2($0._1)(s)).length);
    }
    return Nothing;
  };
  var splitAt2 = (i) => (s) => {
    const before = take3(i)(s);
    return { before, after: drop2(length2(before))(s) };
  };

  // output-es/Data.String.NonEmpty.CodePoints/index.js
  var splitAt3 = (i) => (nes) => {
    const v = splitAt2(i)(nes);
    return { before: v.before === "" ? Nothing : $Maybe("Just", v.before), after: v.after === "" ? Nothing : $Maybe("Just", v.after) };
  };

  // output-es/Data.String.NonEmpty.Internal/index.js
  var toString = (v) => v;
  var stripPrefix2 = (pat) => (a) => {
    const $0 = stripPrefix(pat)(a);
    if ($0.tag === "Just") {
      if ($0._1 === "") {
        return Nothing;
      }
      return $Maybe("Just", $0._1);
    }
    if ($0.tag === "Nothing") {
      return Nothing;
    }
    fail();
  };

  // output-es/Data.Markdown/index.js
  var $FlowContentNode = (tag, _1, _2) => ({ tag, _1, _2 });
  var $Node = (tag, _1) => ({ tag, _1 });
  var $PhrasingContentNode = (tag, _1, _2) => ({ tag, _1, _2 });
  var fromFoldable2 = /* @__PURE__ */ foldrArray(Cons)(Nil);
  var appendFoldable2 = /* @__PURE__ */ appendFoldable(foldableList);
  var foldMap1 = /* @__PURE__ */ (() => foldable1NonEmptyArray.foldMap1(semigroupNonEmptyList))();
  var LineBreak = /* @__PURE__ */ $PhrasingContentNode("LineBreak");
  var List = (value0) => (value1) => $FlowContentNode("List", value0, value1);
  var FlowContent = (value0) => $Node("FlowContent", value0);
  var unorderedList = (dictFoldable1) => {
    const $0 = dictFoldable1.Foldable0().foldr;
    const $1 = List(false);
    const $2 = arrayMap((x) => fromFoldableImpl($0, x));
    return (x) => $1($2(fromFoldableImpl($0, x)));
  };
  var renderRule = /* @__PURE__ */ (() => semigroupNonEmptyList.append($NonEmpty("", Nil))($NonEmpty(
    "---",
    Nil
  )))();
  var renderCodeBlock = (codeLanguage) => (code) => semigroupNonEmptyList.append(appendFoldable2(semigroupNonEmptyList.append($NonEmpty(
    "",
    Nil
  ))($NonEmpty("```", Nil)))(reverse2(fromFoldable2(code))))($NonEmpty(
    (() => {
      if (codeLanguage === "Json") {
        return "```json";
      }
      if (codeLanguage === "Mermaid") {
        return "```mermaid";
      }
      if (codeLanguage === "PlainText") {
        return "```text";
      }
      fail();
    })(),
    Nil
  ));
  var orderedList = (dictFoldable1) => {
    const $0 = dictFoldable1.Foldable0().foldr;
    const $1 = List(true);
    const $2 = arrayMap((x) => fromFoldableImpl($0, x));
    return (x) => $1($2(fromFoldableImpl($0, x)));
  };
  var mapLast = (f) => (x) => {
    if (x._2.tag === "Nil") {
      return $NonEmpty(f(x._1), Nil);
    }
    if (x._2.tag === "Cons") {
      return semigroupNonEmptyList.append($NonEmpty(x._1, Nil))(mapLast(f)($NonEmpty(x._2._1, x._2._2)));
    }
    fail();
  };
  var mapLast$p = (f) => (x) => {
    if (x.tag === "Nil") {
      return Nil;
    }
    if (x.tag === "Cons") {
      const $0 = mapLast(f)($NonEmpty(x._1, x._2));
      return $List("Cons", $0._1, $0._2);
    }
    fail();
  };
  var formatText = (maxLineLength) => unfoldable1NonEmptyList.unfoldr1((s) => {
    if (toCodePointArray(s).length <= maxLineLength) {
      return $Tuple(s, Nothing);
    }
    const v = lastIndexOf$p2(" ")(maxLineLength)(s);
    if (v.tag === "Nothing") {
      return $Tuple(s, Nothing);
    }
    if (v.tag === "Just") {
      const v1 = splitAt3(v._1)(s);
      if (v1.before.tag === "Just" && v1.after.tag === "Just") {
        return $Tuple(v1.before._1, stripPrefix2(" ")(v1.after._1));
      }
      return $Tuple(s, Nothing);
    }
    fail();
  });
  var renderPhrasingContentNodes = (options) => foldlArray((acc) => (node) => {
    if (node.tag === "Emphasis") {
      const $0 = renderEmphasis(options)(node._1);
      return appendFoldable2($NonEmpty(acc._1 + $0._1, acc._2))($0._2);
    }
    if (node.tag === "Link") {
      return $NonEmpty(
        acc._1 + "[" + node._1 + "](" + (() => {
          if (node._2.tag === "ExternalUrl") {
            return node._2._1;
          }
          if (node._2.tag === "InternalUrl") {
            return "#" + node._2._1;
          }
          fail();
        })() + ")",
        acc._2
      );
    }
    if (node.tag === "InlineCode") {
      return $NonEmpty(acc._1 + "`" + node._1 + "`", acc._2);
    }
    if (node.tag === "LineBreak") {
      return semigroupNonEmptyList.append($NonEmpty("", Nil))($NonEmpty(acc._1 + "\\", acc._2));
    }
    if (node.tag === "Text") {
      const $0 = formatText(options.maxLineLength)(node._1);
      if ($0._2.tag === "Nil") {
        return $NonEmpty(acc._1 + $0._1, acc._2);
      }
      if ($0._2.tag === "Cons") {
        return semigroupNonEmptyList.append((() => {
          const $1 = wrappedOperation("reverse")(reverse2)($NonEmpty($0._2._1, $0._2._2));
          return $NonEmpty($1._1, listMap(toString)($1._2));
        })())($NonEmpty(acc._1 + $0._1, acc._2));
      }
    }
    fail();
  })($NonEmpty("", Nil));
  var renderEmphasis = (options) => (x) => mapLast((v1) => v1 + "_")((() => {
    const $0 = renderPhrasingContentNodes(options)(x);
    return $NonEmpty("_" + $0._1, $0._2);
  })());
  var renderHeading = (options) => (headingLevel) => (children2) => semigroupNonEmptyList.append($NonEmpty("", Nil))($NonEmpty(
    (() => {
      if (headingLevel === "L1") {
        return "# ";
      }
      if (headingLevel === "L2") {
        return "## ";
      }
      if (headingLevel === "L3") {
        return "### ";
      }
      if (headingLevel === "L4") {
        return "#### ";
      }
      if (headingLevel === "L5") {
        return "##### ";
      }
      if (headingLevel === "L6") {
        return "###### ";
      }
      fail();
    })() + joinWith("")(arrayMap(replaceAll("\\")("<br/>"))(fromFoldableImpl(
      foldableNonEmptyList.foldr,
      wrappedOperation("reverse")(reverse2)(renderPhrasingContentNodes(options)(children2))
    ))),
    Nil
  ));
  var renderList = (options) => (isOrdered) => (children2) => {
    const itemPrefix = isOrdered ? "1. " : "- ";
    const indent = joinWith("")(replicateImpl(toCodePointArray(itemPrefix).length, " "));
    return foldMap1((nodes) => {
      const v = unsnoc2(foldMap1(renderFlowContentNode(options))(reverse(nodes)));
      return snoc$p($List(
        "Cons",
        "",
        listMap((s) => {
          if (s === "") {
            return s;
          }
          return indent + s;
        })((() => {
          const go = (go$a0$copy) => {
            let go$a0 = go$a0$copy, go$c = true, go$r;
            while (go$c) {
              const v$1 = go$a0;
              if (v$1.tag === "Cons" && v$1._1 === "") {
                go$a0 = v$1._2;
                continue;
              }
              go$c = false;
              go$r = v$1;
            }
            return go$r;
          };
          return go(v.init);
        })())
      ))(itemPrefix + v.last);
    })(reverse(children2));
  };
  var renderFlowContentNode = (options) => (x) => {
    if (x.tag === "Blockquote") {
      return renderBlockquote(options)(x._1);
    }
    if (x.tag === "CodeBlock") {
      return renderCodeBlock(x._1)(x._2);
    }
    if (x.tag === "Heading") {
      return renderHeading({ ...options, maxLineLength: 2147483647 })(x._1)(x._2);
    }
    if (x.tag === "List") {
      return renderList(options)(x._1)(x._2);
    }
    if (x.tag === "Paragraph") {
      return semigroupNonEmptyList.append($NonEmpty("", Nil))(renderPhrasingContentNodes(options)(x._1));
    }
    if (x.tag === "Rule") {
      return renderRule;
    }
    fail();
  };
  var renderBlockquote = (options) => (children2) => {
    const v = unsnoc2(foldMap1(renderFlowContentNode(options))(reverse(children2)));
    return snoc$p($List(
      "Cons",
      "",
      listMap((s) => {
        if (s === "") {
          return ">";
        }
        return "> " + s;
      })((() => {
        const go = (go$a0$copy) => {
          let go$a0 = go$a0$copy, go$c = true, go$r;
          while (go$c) {
            const v$1 = go$a0;
            if (v$1.tag === "Cons" && v$1._1 === "") {
              go$a0 = v$1._2;
              continue;
            }
            go$c = false;
            go$r = v$1;
          }
          return go$r;
        };
        return go(v.init);
      })())
    ))(v.last === "" ? ">" : "> " + v.last);
  };
  var document = (dictFoldable) => {
    const $0 = arrayMap(FlowContent);
    const $1 = dictFoldable.foldr;
    return (x) => $0(fromFoldableImpl($1, x));
  };
  var appendWith$p = (f) => (left) => (x) => {
    if (x.tag === "Nil") {
      return left;
    }
    if (x.tag === "Cons") {
      const $0 = x._1;
      return foldableList.foldr(Cons)(x._2)(mapLast$p((x$1) => f(x$1)($0))(left));
    }
    fail();
  };
  var render2 = (options) => {
    const $0 = joinWith("\n");
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Cons" && v._1 === "") {
          go$a0 = v._2;
          continue;
        }
        go$c = false;
        go$r = v;
      }
      return go$r;
    };
    const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
      let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
      while (go$1$c) {
        const b = go$1$a0, v = go$1$a1;
        if (v.tag === "Nil") {
          go$1$c = false;
          go$1$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$1$a0 = (() => {
            if (v._1.tag === "FlowContent") {
              const $12 = renderFlowContentNode(options)(v._1._1);
              return foldableList.foldr(Cons)(b)($List("Cons", $12._1, $12._2));
            }
            if (v._1.tag === "PhrasingContent") {
              return appendWith$p((b$1) => (a) => a + b$1)((() => {
                const $12 = renderPhrasingContentNodes(options)([v._1._1]);
                return $List("Cons", $12._1, $12._2);
              })())(b);
            }
            fail();
          })();
          go$1$a1 = v._2;
          continue;
        }
        fail();
      }
      return go$1$r;
    };
    const $1 = go$1(Nil);
    return (x) => $0(fromFoldableImpl(foldableList.foldr, reverse2(go($1(fromFoldable2(x)))))) + "\n";
  };

  // output-es/Effect.Exception/foreign.js
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

  // output-es/CLI.Command/index.js
  var document2 = /* @__PURE__ */ document({
    foldMap: (dictMonoid) => {
      const foldMap13 = foldableArray.foldMap(dictMonoid);
      return (f) => (v) => dictMonoid.Semigroup0().append(f(v._1))(foldMap13(f)(v._2));
    },
    foldl: (f) => (b) => (v) => foldlArray(f)(f(b)(v._1))(v._2),
    foldr: (f) => (b) => (v) => f(v._1)(foldrArray(f)(b)(v._2))
  });
  var commandProgram = (dictApplicative) => (dictDocument) => (dictEncodeJson) => (dictMonadError) => {
    const $0 = dictMonadError.MonadThrow0().Monad0().Bind1();
    return (program5) => (outputFormat) => (options) => {
      const renderOutput = (() => {
        if (outputFormat === "Json") {
          const $1 = stringifyWithIndent(2);
          return (x) => $1(dictEncodeJson.encodeJson(x));
        }
        if (outputFormat === "Markdown") {
          const $1 = render2({ maxLineLength: 72 });
          return (x) => $1(document2(dictDocument.document(x)));
        }
        fail();
      })();
      return dictMonadError.catchError($0.bind(program5(options))((x) => dictApplicative.pure((() => {
        if (x.tag === "Left") {
          return { exitCode: 1, stderr: "", stdout: renderOutput(x._1) };
        }
        if (x.tag === "Right") {
          return { exitCode: 0, stderr: "", stdout: renderOutput(x._1) };
        }
        fail();
      })())))((x) => dictApplicative.pure({ exitCode: 2, stderr: message(x), stdout: "" }));
    };
  };

  // output-es/Data.Argonaut.Parser/foreign.js
  function _jsonParser(fail2, succ, s) {
    try {
      return succ(JSON.parse(s));
    } catch (e) {
      return fail2(e.message);
    }
  }

  // output-es/Data.Map.Internal/index.js
  var $$$Map = (tag, _1, _2, _3, _4, _5, _6) => ({ tag, _1, _2, _3, _4, _5, _6 });
  var $MapIter = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
  var $MapIterStep = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
  var $Split = (_1, _2, _3) => ({ tag: "Split", _1, _2, _3 });
  var $SplitLast = (_1, _2, _3) => ({ tag: "SplitLast", _1, _2, _3 });
  var Leaf2 = /* @__PURE__ */ $$$Map("Leaf");
  var IterLeaf = /* @__PURE__ */ $MapIter("IterLeaf");
  var IterDone = /* @__PURE__ */ $MapIterStep("IterDone");
  var unsafeNode = (k, v, l, r) => {
    if (l.tag === "Leaf") {
      if (r.tag === "Leaf") {
        return $$$Map("Node", 1, 1, k, v, l, r);
      }
      if (r.tag === "Node") {
        return $$$Map("Node", 1 + r._1 | 0, 1 + r._2 | 0, k, v, l, r);
      }
      fail();
    }
    if (l.tag === "Node") {
      if (r.tag === "Leaf") {
        return $$$Map("Node", 1 + l._1 | 0, 1 + l._2 | 0, k, v, l, r);
      }
      if (r.tag === "Node") {
        return $$$Map("Node", l._1 > r._1 ? 1 + l._1 | 0 : 1 + r._1 | 0, (1 + l._2 | 0) + r._2 | 0, k, v, l, r);
      }
    }
    fail();
  };
  var unsafeBalancedNode = (k, v, l, r) => {
    if (l.tag === "Leaf") {
      if (r.tag === "Leaf") {
        return $$$Map("Node", 1, 1, k, v, Leaf2, Leaf2);
      }
      if (r.tag === "Node" && r._1 > 1) {
        if (r._5.tag === "Node" && (() => {
          if (r._6.tag === "Leaf") {
            return r._5._1 > 0;
          }
          if (r._6.tag === "Node") {
            return r._5._1 > r._6._1;
          }
          fail();
        })()) {
          return unsafeNode(r._5._3, r._5._4, unsafeNode(k, v, l, r._5._5), unsafeNode(r._3, r._4, r._5._6, r._6));
        }
        return unsafeNode(r._3, r._4, unsafeNode(k, v, l, r._5), r._6);
      }
      return unsafeNode(k, v, l, r);
    }
    if (l.tag === "Node") {
      if (r.tag === "Node") {
        if (r._1 > (l._1 + 1 | 0)) {
          if (r._5.tag === "Node" && (() => {
            if (r._6.tag === "Leaf") {
              return r._5._1 > 0;
            }
            if (r._6.tag === "Node") {
              return r._5._1 > r._6._1;
            }
            fail();
          })()) {
            return unsafeNode(r._5._3, r._5._4, unsafeNode(k, v, l, r._5._5), unsafeNode(r._3, r._4, r._5._6, r._6));
          }
          return unsafeNode(r._3, r._4, unsafeNode(k, v, l, r._5), r._6);
        }
        if (l._1 > (r._1 + 1 | 0)) {
          if (l._6.tag === "Node" && (() => {
            if (l._5.tag === "Leaf") {
              return 0 <= l._6._1;
            }
            if (l._5.tag === "Node") {
              return l._5._1 <= l._6._1;
            }
            fail();
          })()) {
            return unsafeNode(l._6._3, l._6._4, unsafeNode(l._3, l._4, l._5, l._6._5), unsafeNode(k, v, l._6._6, r));
          }
          return unsafeNode(l._3, l._4, l._5, unsafeNode(k, v, l._6, r));
        }
        return unsafeNode(k, v, l, r);
      }
      if (r.tag === "Leaf" && l._1 > 1) {
        if (l._6.tag === "Node" && (() => {
          if (l._5.tag === "Leaf") {
            return 0 <= l._6._1;
          }
          if (l._5.tag === "Node") {
            return l._5._1 <= l._6._1;
          }
          fail();
        })()) {
          return unsafeNode(l._6._3, l._6._4, unsafeNode(l._3, l._4, l._5, l._6._5), unsafeNode(k, v, l._6._6, r));
        }
        return unsafeNode(l._3, l._4, l._5, unsafeNode(k, v, l._6, r));
      }
      return unsafeNode(k, v, l, r);
    }
    fail();
  };
  var unsafeSplit = (comp, k, m) => {
    if (m.tag === "Leaf") {
      return $Split(Nothing, Leaf2, Leaf2);
    }
    if (m.tag === "Node") {
      const v = comp(k)(m._3);
      if (v === "LT") {
        const v1 = unsafeSplit(comp, k, m._5);
        return $Split(v1._1, v1._2, unsafeBalancedNode(m._3, m._4, v1._3, m._6));
      }
      if (v === "GT") {
        const v1 = unsafeSplit(comp, k, m._6);
        return $Split(v1._1, unsafeBalancedNode(m._3, m._4, m._5, v1._2), v1._3);
      }
      if (v === "EQ") {
        return $Split($Maybe("Just", m._4), m._5, m._6);
      }
    }
    fail();
  };
  var unsafeSplitLast = (k, v, l, r) => {
    if (r.tag === "Leaf") {
      return $SplitLast(k, v, l);
    }
    if (r.tag === "Node") {
      const v1 = unsafeSplitLast(r._3, r._4, r._5, r._6);
      return $SplitLast(v1._1, v1._2, unsafeBalancedNode(k, v, l, v1._3));
    }
    fail();
  };
  var unsafeJoinNodes = (v, v1) => {
    if (v.tag === "Leaf") {
      return v1;
    }
    if (v.tag === "Node") {
      const v2 = unsafeSplitLast(v._3, v._4, v._5, v._6);
      return unsafeBalancedNode(v2._1, v2._2, v2._3, v1);
    }
    fail();
  };
  var unsafeDifference = (comp, l, r) => {
    if (l.tag === "Leaf") {
      return Leaf2;
    }
    if (r.tag === "Leaf") {
      return l;
    }
    if (r.tag === "Node") {
      const v = unsafeSplit(comp, r._3, l);
      return unsafeJoinNodes(unsafeDifference(comp, v._2, r._5), unsafeDifference(comp, v._3, r._6));
    }
    fail();
  };
  var unsafeUnionWith = (comp, app, l, r) => {
    if (l.tag === "Leaf") {
      return r;
    }
    if (r.tag === "Leaf") {
      return l;
    }
    if (r.tag === "Node") {
      const v = unsafeSplit(comp, r._3, l);
      const l$p = unsafeUnionWith(comp, app, v._2, r._5);
      const r$p = unsafeUnionWith(comp, app, v._3, r._6);
      if (v._1.tag === "Just") {
        return unsafeBalancedNode(r._3, app(v._1._1)(r._4), l$p, r$p);
      }
      if (v._1.tag === "Nothing") {
        return unsafeBalancedNode(r._3, r._4, l$p, r$p);
      }
    }
    fail();
  };
  var pop = (dictOrd) => {
    const compare = dictOrd.compare;
    return (k) => (m) => {
      const v = unsafeSplit(compare, k, m);
      if (v._1.tag === "Just") {
        return $Maybe("Just", $Tuple(v._1._1, unsafeJoinNodes(v._2, v._3)));
      }
      return Nothing;
    };
  };
  var lookup = (dictOrd) => (k) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Leaf") {
          go$c = false;
          go$r = Nothing;
          continue;
        }
        if (v.tag === "Node") {
          const v1 = dictOrd.compare(k)(v._3);
          if (v1 === "LT") {
            go$a0 = v._5;
            continue;
          }
          if (v1 === "GT") {
            go$a0 = v._6;
            continue;
          }
          if (v1 === "EQ") {
            go$c = false;
            go$r = $Maybe("Just", v._4);
            continue;
          }
        }
        fail();
      }
      return go$r;
    };
    return go;
  };
  var stepAscCps = (next) => (done) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "IterLeaf") {
          go$c = false;
          go$r = done();
          continue;
        }
        if (v.tag === "IterEmit") {
          go$c = false;
          go$r = next(v._1, v._2, v._3);
          continue;
        }
        if (v.tag === "IterNode") {
          go$a0 = (() => {
            const go$1 = (go$1$a0$copy) => (go$1$a1$copy) => {
              let go$1$a0 = go$1$a0$copy, go$1$a1 = go$1$a1$copy, go$1$c = true, go$1$r;
              while (go$1$c) {
                const iter = go$1$a0, v$1 = go$1$a1;
                if (v$1.tag === "Leaf") {
                  go$1$c = false;
                  go$1$r = iter;
                  continue;
                }
                if (v$1.tag === "Node") {
                  if (v$1._6.tag === "Leaf") {
                    go$1$a0 = $MapIter("IterEmit", v$1._3, v$1._4, iter);
                    go$1$a1 = v$1._5;
                    continue;
                  }
                  go$1$a0 = $MapIter("IterEmit", v$1._3, v$1._4, $MapIter("IterNode", v$1._6, iter));
                  go$1$a1 = v$1._5;
                  continue;
                }
                fail();
              }
              return go$1$r;
            };
            return go$1(v._2)(v._1);
          })();
          continue;
        }
        fail();
      }
      return go$r;
    };
    return go;
  };
  var stepAsc = /* @__PURE__ */ stepAscCps((k, v, next) => $MapIterStep("IterNext", k, v, next))((v) => IterDone);
  var eqMapIter = (dictEq) => (dictEq1) => ({
    eq: /* @__PURE__ */ (() => {
      const go = (a) => (b) => {
        const v = stepAsc(a);
        if (v.tag === "IterNext") {
          const v2 = stepAsc(b);
          return v2.tag === "IterNext" && dictEq.eq(v._1)(v2._1) && dictEq1.eq(v._2)(v2._2) && go(v._3)(v2._3);
        }
        if (v.tag === "IterDone") {
          return true;
        }
        fail();
      };
      return go;
    })()
  });
  var insertWith = (dictOrd) => (app) => (k) => (v) => {
    const go = (v1) => {
      if (v1.tag === "Leaf") {
        return $$$Map("Node", 1, 1, k, v, Leaf2, Leaf2);
      }
      if (v1.tag === "Node") {
        const v2 = dictOrd.compare(k)(v1._3);
        if (v2 === "LT") {
          return unsafeBalancedNode(v1._3, v1._4, go(v1._5), v1._6);
        }
        if (v2 === "GT") {
          return unsafeBalancedNode(v1._3, v1._4, v1._5, go(v1._6));
        }
        if (v2 === "EQ") {
          return $$$Map("Node", v1._1, v1._2, k, app(v1._4)(v), v1._5, v1._6);
        }
      }
      fail();
    };
    return go;
  };
  var insert = (dictOrd) => (k) => (v) => {
    const go = (v1) => {
      if (v1.tag === "Leaf") {
        return $$$Map("Node", 1, 1, k, v, Leaf2, Leaf2);
      }
      if (v1.tag === "Node") {
        const v2 = dictOrd.compare(k)(v1._3);
        if (v2 === "LT") {
          return unsafeBalancedNode(v1._3, v1._4, go(v1._5), v1._6);
        }
        if (v2 === "GT") {
          return unsafeBalancedNode(v1._3, v1._4, v1._5, go(v1._6));
        }
        if (v2 === "EQ") {
          return $$$Map("Node", v1._1, v1._2, k, v, v1._5, v1._6);
        }
      }
      fail();
    };
    return go;
  };
  var functorMap = {
    map: (f) => {
      const go = (v) => {
        if (v.tag === "Leaf") {
          return Leaf2;
        }
        if (v.tag === "Node") {
          return $$$Map("Node", v._1, v._2, v._3, f(v._4), go(v._5), go(v._6));
        }
        fail();
      };
      return go;
    }
  };
  var foldableMap = {
    foldr: (f) => (z) => {
      const go = (m$p, z$p) => {
        if (m$p.tag === "Leaf") {
          return z$p;
        }
        if (m$p.tag === "Node") {
          return go(m$p._5, f(m$p._4)(go(m$p._6, z$p)));
        }
        fail();
      };
      return (m) => go(m, z);
    },
    foldl: (f) => (z) => {
      const go = (z$p, m$p) => {
        if (m$p.tag === "Leaf") {
          return z$p;
        }
        if (m$p.tag === "Node") {
          return go(f(go(z$p, m$p._5))(m$p._4), m$p._6);
        }
        fail();
      };
      return (m) => go(z, m);
    },
    foldMap: (dictMonoid) => {
      const mempty = dictMonoid.mempty;
      const $0 = dictMonoid.Semigroup0();
      return (f) => {
        const go = (v) => {
          if (v.tag === "Leaf") {
            return mempty;
          }
          if (v.tag === "Node") {
            return $0.append(go(v._5))($0.append(f(v._4))(go(v._6)));
          }
          fail();
        };
        return go;
      };
    }
  };
  var keys2 = /* @__PURE__ */ (() => {
    const go = (m$p, z$p) => {
      if (m$p.tag === "Leaf") {
        return z$p;
      }
      if (m$p.tag === "Node") {
        return go(m$p._5, $List("Cons", m$p._3, go(m$p._6, z$p)));
      }
      fail();
    };
    return (m) => go(m, Nil);
  })();
  var filterWithKey = (dictOrd) => (f) => {
    const go = (v) => {
      if (v.tag === "Leaf") {
        return Leaf2;
      }
      if (v.tag === "Node") {
        if (f(v._3)(v._4)) {
          return unsafeBalancedNode(v._3, v._4, go(v._5), go(v._6));
        }
        return unsafeJoinNodes(go(v._5), go(v._6));
      }
      fail();
    };
    return go;
  };
  var eqMap = (dictEq) => (dictEq1) => ({
    eq: (xs) => (ys) => {
      if (xs.tag === "Leaf") {
        return ys.tag === "Leaf";
      }
      if (xs.tag === "Node") {
        return ys.tag === "Node" && xs._2 === ys._2 && eqMapIter(dictEq)(dictEq1).eq($MapIter("IterNode", xs, IterLeaf))($MapIter("IterNode", ys, IterLeaf));
      }
      fail();
    }
  });
  var fromFoldable3 = (dictOrd) => (dictFoldable) => dictFoldable.foldl((m) => (v) => insert(dictOrd)(v._1)(v._2)(m))(Leaf2);
  var $$delete = (dictOrd) => (k) => {
    const go = (v) => {
      if (v.tag === "Leaf") {
        return Leaf2;
      }
      if (v.tag === "Node") {
        const v1 = dictOrd.compare(k)(v._3);
        if (v1 === "LT") {
          return unsafeBalancedNode(v._3, v._4, go(v._5), v._6);
        }
        if (v1 === "GT") {
          return unsafeBalancedNode(v._3, v._4, v._5, go(v._6));
        }
        if (v1 === "EQ") {
          return unsafeJoinNodes(v._5, v._6);
        }
      }
      fail();
    };
    return go;
  };
  var alter = (dictOrd) => {
    const compare = dictOrd.compare;
    return (f) => (k) => (m) => {
      const v = unsafeSplit(compare, k, m);
      const v2 = f(v._1);
      if (v2.tag === "Nothing") {
        return unsafeJoinNodes(v._2, v._3);
      }
      if (v2.tag === "Just") {
        return unsafeBalancedNode(k, v2._1, v._2, v._3);
      }
      fail();
    };
  };

  // output-es/Data.Set/index.js
  var foldableSet = {
    foldMap: (dictMonoid) => {
      const foldMap13 = foldableList.foldMap(dictMonoid);
      return (f) => {
        const $0 = foldMap13(f);
        return (x) => $0(keys2(x));
      };
    },
    foldl: (f) => (x) => {
      const go = (go$a0$copy) => (go$a1$copy) => {
        let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
        while (go$c) {
          const b = go$a0, v = go$a1;
          if (v.tag === "Nil") {
            go$c = false;
            go$r = b;
            continue;
          }
          if (v.tag === "Cons") {
            go$a0 = f(b)(v._1);
            go$a1 = v._2;
            continue;
          }
          fail();
        }
        return go$r;
      };
      const $0 = go(x);
      return (x$1) => $0(keys2(x$1));
    },
    foldr: (f) => (x) => {
      const $0 = foldableList.foldr(f)(x);
      return (x$1) => $0(keys2(x$1));
    }
  };
  var ordSet = (dictOrd) => {
    const $0 = dictOrd.Eq0();
    const eqSet1 = { eq: (v) => (v1) => eqMap($0)(eqUnit).eq(v)(v1) };
    return { compare: (s1) => (s2) => ordList(dictOrd).compare(keys2(s1))(keys2(s2)), Eq0: () => eqSet1 };
  };
  var monoidSet = (dictOrd) => {
    const semigroupSet1 = {
      append: (() => {
        const compare = dictOrd.compare;
        return (m1) => (m2) => unsafeUnionWith(compare, $$const, m1, m2);
      })()
    };
    return { mempty: Leaf2, Semigroup0: () => semigroupSet1 };
  };

  // output-es/Data.Argonaut.Encode.Encoders/index.js
  var toUnfoldable3 = /* @__PURE__ */ toUnfoldable2(unfoldableArray);
  var toUnfoldable22 = /* @__PURE__ */ (() => {
    const $0 = toUnfoldable2(unfoldableList);
    return (x) => $0(keys2(x));
  })();
  var extend = (encoder) => (v) => {
    const $0 = caseJsonObject(jsonSingletonObject(v._1)(v._2))((x) => id((() => {
      const $02 = v._1;
      const $1 = v._2;
      return mutate(($2) => () => {
        $2[$02] = $1;
        return $2;
      })(x);
    })()));
    return (x) => $0(encoder(x));
  };
  var encodeMaybe = (encoder) => (v) => {
    if (v.tag === "Nothing") {
      return jsonNull;
    }
    if (v.tag === "Just") {
      return encoder(v._1);
    }
    fail();
  };
  var encodeList = (encoder) => {
    const $0 = arrayMap(encoder);
    return (x) => id($0(toUnfoldable3(x)));
  };

  // output-es/Data.Argonaut.Encode.Class/index.js
  var gEncodeJsonNil = { gEncodeJson: (v) => (v1) => empty };
  var encodeJsonJson = { encodeJson: (x) => x };
  var encodeJsonJString = { encodeJson: id };
  var encodeJsonJNumber = { encodeJson: id };
  var encodeJsonJBoolean = { encodeJson: id };
  var encodeSet = (dictOrd) => (dictEncodeJson) => ({
    encodeJson: (() => {
      const $0 = encodeList(dictEncodeJson.encodeJson);
      return (x) => $0(toUnfoldable22(x));
    })()
  });
  var gEncodeJsonCons = (dictEncodeJson) => (dictGEncodeJson) => (dictIsSymbol) => () => ({
    gEncodeJson: (row) => (v) => {
      const $0 = dictIsSymbol.reflectSymbol($$Proxy);
      const $1 = dictEncodeJson.encodeJson(unsafeGet(dictIsSymbol.reflectSymbol($$Proxy))(row));
      return mutate(($2) => () => {
        $2[$0] = $1;
        return $2;
      })(dictGEncodeJson.gEncodeJson(row)($$Proxy));
    }
  });

  // output-es/JsonSchema/index.js
  var $JsonSchema = (tag, _1) => ({ tag, _1 });
  var $JsonValueType = (tag) => tag;
  var compare1 = /* @__PURE__ */ (() => ordMaybe(ordNumber).compare)();
  var compare2 = /* @__PURE__ */ (() => ordSet(ordString).compare)();
  var toUnfoldable4 = /* @__PURE__ */ (() => {
    const $0 = toUnfoldable2(unfoldableArray);
    return (x) => $0(keys2(x));
  })();
  var fromFoldable4 = /* @__PURE__ */ fromFoldable(foldableArray);
  var JsonArray = /* @__PURE__ */ $JsonValueType("JsonArray");
  var JsonBoolean = /* @__PURE__ */ $JsonValueType("JsonBoolean");
  var JsonInteger = /* @__PURE__ */ $JsonValueType("JsonInteger");
  var JsonNull = /* @__PURE__ */ $JsonValueType("JsonNull");
  var JsonNumber = /* @__PURE__ */ $JsonValueType("JsonNumber");
  var JsonObject = /* @__PURE__ */ $JsonValueType("JsonObject");
  var JsonString = /* @__PURE__ */ $JsonValueType("JsonString");
  var eqJsonValueType = {
    eq: (x) => (y) => {
      if (x === "JsonArray") {
        return y === "JsonArray";
      }
      if (x === "JsonBoolean") {
        return y === "JsonBoolean";
      }
      if (x === "JsonInteger") {
        return y === "JsonInteger";
      }
      if (x === "JsonNull") {
        return y === "JsonNull";
      }
      if (x === "JsonNumber") {
        return y === "JsonNumber";
      }
      if (x === "JsonObject") {
        return y === "JsonObject";
      }
      return x === "JsonString" && y === "JsonString";
    }
  };
  var eq3 = (x) => (y) => {
    if (x.tag === "Nothing") {
      return y.tag === "Nothing";
    }
    return x.tag === "Just" && y.tag === "Just" && eqMap(eqJsonValueType)(eqUnit).eq(x._1)(y._1);
  };
  var ordJsonValueType = {
    compare: (x) => (y) => {
      if (x === "JsonArray") {
        if (y === "JsonArray") {
          return EQ;
        }
        return LT;
      }
      if (y === "JsonArray") {
        return GT;
      }
      if (x === "JsonBoolean") {
        if (y === "JsonBoolean") {
          return EQ;
        }
        return LT;
      }
      if (y === "JsonBoolean") {
        return GT;
      }
      if (x === "JsonInteger") {
        if (y === "JsonInteger") {
          return EQ;
        }
        return LT;
      }
      if (y === "JsonInteger") {
        return GT;
      }
      if (x === "JsonNull") {
        if (y === "JsonNull") {
          return EQ;
        }
        return LT;
      }
      if (y === "JsonNull") {
        return GT;
      }
      if (x === "JsonNumber") {
        if (y === "JsonNumber") {
          return EQ;
        }
        return LT;
      }
      if (y === "JsonNumber") {
        return GT;
      }
      if (x === "JsonObject") {
        if (y === "JsonObject") {
          return EQ;
        }
        return LT;
      }
      if (y === "JsonObject") {
        return GT;
      }
      if (x === "JsonString" && y === "JsonString") {
        return EQ;
      }
      fail();
    },
    Eq0: () => eqJsonValueType
  };
  var compare3 = /* @__PURE__ */ (() => ordMaybe(ordSet(ordJsonValueType)).compare)();
  var eqJsonSchema = {
    eq: (x) => (y) => {
      if (x.tag === "BooleanSchema") {
        return y.tag === "BooleanSchema" && x._1 === y._1;
      }
      return x.tag === "ObjectSchema" && y.tag === "ObjectSchema" && (x._1.exclusiveMaximum.tag === "Nothing" ? y._1.exclusiveMaximum.tag === "Nothing" : x._1.exclusiveMaximum.tag === "Just" && y._1.exclusiveMaximum.tag === "Just" && x._1.exclusiveMaximum._1 === y._1.exclusiveMaximum._1) && (x._1.exclusiveMinimum.tag === "Nothing" ? y._1.exclusiveMinimum.tag === "Nothing" : x._1.exclusiveMinimum.tag === "Just" && y._1.exclusiveMinimum.tag === "Just" && x._1.exclusiveMinimum._1 === y._1.exclusiveMinimum._1) && (x._1.items.tag === "Nothing" ? y._1.items.tag === "Nothing" : x._1.items.tag === "Just" && y._1.items.tag === "Just" && eqJsonSchema.eq(x._1.items._1)(y._1.items._1)) && (x._1.maximum.tag === "Nothing" ? y._1.maximum.tag === "Nothing" : x._1.maximum.tag === "Just" && y._1.maximum.tag === "Just" && x._1.maximum._1 === y._1.maximum._1) && (x._1.minimum.tag === "Nothing" ? y._1.minimum.tag === "Nothing" : x._1.minimum.tag === "Just" && y._1.minimum.tag === "Just" && x._1.minimum._1 === y._1.minimum._1) && (x._1.multipleOf.tag === "Nothing" ? y._1.multipleOf.tag === "Nothing" : x._1.multipleOf.tag === "Just" && y._1.multipleOf.tag === "Just" && x._1.multipleOf._1 === y._1.multipleOf._1) && (x._1.not.tag === "Nothing" ? y._1.not.tag === "Nothing" : x._1.not.tag === "Just" && y._1.not.tag === "Just" && eqJsonSchema.eq(x._1.not._1)(y._1.not._1)) && eqMap(eqString)(eqUnit).eq(x._1.required)(y._1.required) && eq3(x._1.typeKeyword)(y._1.typeKeyword) && x._1.uniqueItems === y._1.uniqueItems;
    }
  };
  var ordJsonSchema = {
    compare: (x) => (y) => {
      if (x.tag === "BooleanSchema") {
        if (y.tag === "BooleanSchema") {
          return ordBoolean.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "BooleanSchema") {
        return GT;
      }
      if (x.tag === "ObjectSchema" && y.tag === "ObjectSchema") {
        const v = compare1(x._1.exclusiveMaximum)(y._1.exclusiveMaximum);
        if (v === "LT") {
          return LT;
        }
        if (v === "GT") {
          return GT;
        }
        const v1 = compare1(x._1.exclusiveMinimum)(y._1.exclusiveMinimum);
        if (v1 === "LT") {
          return LT;
        }
        if (v1 === "GT") {
          return GT;
        }
        const v2 = ordMaybe(ordJsonSchema).compare(x._1.items)(y._1.items);
        if (v2 === "LT") {
          return LT;
        }
        if (v2 === "GT") {
          return GT;
        }
        const v3 = compare1(x._1.maximum)(y._1.maximum);
        if (v3 === "LT") {
          return LT;
        }
        if (v3 === "GT") {
          return GT;
        }
        const v4 = compare1(x._1.minimum)(y._1.minimum);
        if (v4 === "LT") {
          return LT;
        }
        if (v4 === "GT") {
          return GT;
        }
        const v5 = compare1(x._1.multipleOf)(y._1.multipleOf);
        if (v5 === "LT") {
          return LT;
        }
        if (v5 === "GT") {
          return GT;
        }
        const v6 = ordMaybe(ordJsonSchema).compare(x._1.not)(y._1.not);
        if (v6 === "LT") {
          return LT;
        }
        if (v6 === "GT") {
          return GT;
        }
        const v7 = compare2(x._1.required)(y._1.required);
        if (v7 === "LT") {
          return LT;
        }
        if (v7 === "GT") {
          return GT;
        }
        const v8 = compare3(x._1.typeKeyword)(y._1.typeKeyword);
        if (v8 === "LT") {
          return LT;
        }
        if (v8 === "GT") {
          return GT;
        }
        return ordBoolean.compare(x._1.uniqueItems)(y._1.uniqueItems);
      }
      fail();
    },
    Eq0: () => eqJsonSchema
  };
  var renderJsonValueType = (v) => {
    if (v === "JsonArray") {
      return "array";
    }
    if (v === "JsonBoolean") {
      return "boolean";
    }
    if (v === "JsonInteger") {
      return "integer";
    }
    if (v === "JsonNull") {
      return "null";
    }
    if (v === "JsonNumber") {
      return "number";
    }
    if (v === "JsonObject") {
      return "object";
    }
    if (v === "JsonString") {
      return "string";
    }
    fail();
  };
  var encodeJsonJsonValueType = {
    encodeJson: (x) => id((() => {
      if (x === "JsonArray") {
        return "array";
      }
      if (x === "JsonBoolean") {
        return "boolean";
      }
      if (x === "JsonInteger") {
        return "integer";
      }
      if (x === "JsonNull") {
        return "null";
      }
      if (x === "JsonNumber") {
        return "number";
      }
      if (x === "JsonObject") {
        return "object";
      }
      if (x === "JsonString") {
        return "string";
      }
      fail();
    })())
  };
  var printRequiredKeywordSpec = /* @__PURE__ */ (() => {
    const $0 = arrayMap(id);
    return (x) => id($0(toUnfoldable4(x)));
  })();
  var printJsonValueType = (x) => id((() => {
    if (x === "JsonArray") {
      return "array";
    }
    if (x === "JsonBoolean") {
      return "boolean";
    }
    if (x === "JsonInteger") {
      return "integer";
    }
    if (x === "JsonNull") {
      return "null";
    }
    if (x === "JsonNumber") {
      return "number";
    }
    if (x === "JsonObject") {
      return "object";
    }
    if (x === "JsonString") {
      return "string";
    }
    fail();
  })());
  var printTypeKeywordSpec = /* @__PURE__ */ (() => {
    const $0 = arrayMap(printJsonValueType);
    return (x) => id($0(toUnfoldable4(x)));
  })();
  var defaultKeywords = {
    exclusiveMaximum: Nothing,
    exclusiveMinimum: Nothing,
    items: Nothing,
    maximum: Nothing,
    minimum: Nothing,
    multipleOf: Nothing,
    not: Nothing,
    required: Leaf2,
    typeKeyword: Nothing,
    uniqueItems: false
  };
  var printObjectSchema = (keywords) => id(fromFoldable4(arrayMap((m) => $Tuple(m._1, m._2))([
    ...(() => {
      if (keywords.exclusiveMaximum.tag === "Just") {
        return [$Tuple("exclusiveMaximum", id(keywords.exclusiveMaximum._1))];
      }
      if (keywords.exclusiveMaximum.tag === "Nothing") {
        return [];
      }
      fail();
    })(),
    ...(() => {
      if (keywords.exclusiveMinimum.tag === "Just") {
        return [$Tuple("exclusiveMinimum", id(keywords.exclusiveMinimum._1))];
      }
      if (keywords.exclusiveMinimum.tag === "Nothing") {
        return [];
      }
      fail();
    })(),
    ...(() => {
      if (keywords.items.tag === "Nothing") {
        return [];
      }
      if (keywords.items.tag === "Just") {
        return [$Tuple("items", print(keywords.items._1))];
      }
      fail();
    })(),
    ...(() => {
      if (keywords.maximum.tag === "Just") {
        return [$Tuple("maximum", id(keywords.maximum._1))];
      }
      if (keywords.maximum.tag === "Nothing") {
        return [];
      }
      fail();
    })(),
    ...(() => {
      if (keywords.minimum.tag === "Just") {
        return [$Tuple("minimum", id(keywords.minimum._1))];
      }
      if (keywords.minimum.tag === "Nothing") {
        return [];
      }
      fail();
    })(),
    ...(() => {
      if (keywords.multipleOf.tag === "Just") {
        return [$Tuple("multipleOf", id(keywords.multipleOf._1))];
      }
      if (keywords.multipleOf.tag === "Nothing") {
        return [];
      }
      fail();
    })(),
    ...(() => {
      if (keywords.not.tag === "Nothing") {
        return [];
      }
      if (keywords.not.tag === "Just") {
        return [$Tuple("not", print(keywords.not._1))];
      }
      fail();
    })(),
    ...keywords.required.tag === "Leaf" ? [] : [$Tuple("required", printRequiredKeywordSpec(keywords.required))],
    ...(() => {
      if (keywords.typeKeyword.tag === "Just") {
        return [$Tuple("type", printTypeKeywordSpec(keywords.typeKeyword._1))];
      }
      if (keywords.typeKeyword.tag === "Nothing") {
        return [];
      }
      fail();
    })(),
    ...!keywords.uniqueItems ? [] : [$Tuple("uniqueItems", id(keywords.uniqueItems))]
  ])));
  var print = (v) => {
    if (v.tag === "BooleanSchema") {
      return id(v._1);
    }
    if (v.tag === "ObjectSchema") {
      return printObjectSchema(v._1);
    }
    fail();
  };
  var encodeJsonJsonSchema = { encodeJson: (x) => print(x) };

  // output-es/JsonSchema.Codec.Parsing/index.js
  var traverse = /* @__PURE__ */ (() => traversableArray.traverse(applicativeEither))();
  var fromFoldable1 = /* @__PURE__ */ foldlArray((m) => (a) => insert(ordString)(a)()(m))(Leaf2);
  var fromFoldable22 = /* @__PURE__ */ foldlArray((m) => (a) => insert(ordJsonValueType)(a)()(m))(Leaf2);
  var parseRequiredKeywordSpec = (specJson) => {
    const $0 = _caseJson(
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      Just,
      (v) => Nothing,
      specJson
    );
    if ($0.tag === "Nothing") {
      return $Either("Left", "Property names are not an array.");
    }
    if ($0.tag === "Just") {
      const $1 = traverse((x) => {
        const $12 = _caseJson(
          (v) => Nothing,
          (v) => Nothing,
          (v) => Nothing,
          Just,
          (v) => Nothing,
          (v) => Nothing,
          x
        );
        if ($12.tag === "Nothing") {
          return $Either("Left", "Property name is not a string.");
        }
        if ($12.tag === "Just") {
          return $Either("Right", $12._1);
        }
        fail();
      })($0._1);
      return (() => {
        if ($1.tag === "Left") {
          const $2 = $1._1;
          return (v) => $Either("Left", $2);
        }
        if ($1.tag === "Right") {
          const $2 = $1._1;
          return (f) => f($2);
        }
        fail();
      })()((propertyNames) => $Either("Right", fromFoldable1(propertyNames)));
    }
    fail();
  };
  var parseOptionalNumber = (name4) => (x) => {
    const $0 = _lookup(Nothing, Just, name4, x);
    if ($0.tag === "Just") {
      const v1 = _caseJson(
        (v) => Nothing,
        (v) => Nothing,
        Just,
        (v) => Nothing,
        (v) => Nothing,
        (v) => Nothing,
        $0._1
      );
      if (v1.tag === "Just") {
        return $Either("Right", $Maybe("Just", v1._1));
      }
      if (v1.tag === "Nothing") {
        return $Either("Left", name4 + " is not a number.");
      }
      fail();
    }
    if ($0.tag === "Nothing") {
      return $Either("Right", Nothing);
    }
    fail();
  };
  var parseMultipleOf = (x) => {
    const $0 = _lookup(Nothing, Just, "multipleOf", x);
    if ($0.tag === "Just") {
      if (_caseJson((v) => true, (v) => false, (v) => false, (v) => false, (v) => false, (v) => false, $0._1)) {
        return $Either("Right", Nothing);
      }
      const v1 = _caseJson(
        (v) => Nothing,
        (v) => Nothing,
        Just,
        (v) => Nothing,
        (v) => Nothing,
        (v) => Nothing,
        $0._1
      );
      if (v1.tag === "Just") {
        if (v1._1 > 0) {
          return $Either("Right", $Maybe("Just", v1._1));
        }
        return $Either("Left", "multipleOf must be greater than zero.");
      }
      if (v1.tag === "Nothing") {
        return $Either("Left", "multipleOf is not a number.");
      }
      fail();
    }
    if ($0.tag === "Nothing") {
      return $Either("Right", Nothing);
    }
    fail();
  };
  var parseJsonValueType = (json) => {
    const v = _caseJson(
      (v2) => Nothing,
      (v2) => Nothing,
      (v2) => Nothing,
      Just,
      (v2) => Nothing,
      (v2) => Nothing,
      json
    );
    if (v.tag === "Just") {
      if (v._1 === "array") {
        return $Either("Right", JsonArray);
      }
      if (v._1 === "boolean") {
        return $Either("Right", JsonBoolean);
      }
      if (v._1 === "integer") {
        return $Either("Right", JsonInteger);
      }
      if (v._1 === "null") {
        return $Either("Right", JsonNull);
      }
      if (v._1 === "number") {
        return $Either("Right", JsonNumber);
      }
      if (v._1 === "object") {
        return $Either("Right", JsonObject);
      }
      if (v._1 === "string") {
        return $Either("Right", JsonString);
      }
      return $Either("Left", "Unsupported JSON value type: " + v._1);
    }
    if (v.tag === "Nothing") {
      return $Either("Left", "JSON value type is not a string");
    }
    fail();
  };
  var parseTypeKeywordSpec = (specJson) => {
    const $0 = parseJsonValueType(specJson);
    const $1 = (() => {
      if ($0.tag === "Left") {
        return $Either("Left", $0._1);
      }
      if ($0.tag === "Right") {
        return $Either("Right", $$$Map("Node", 1, 1, $0._1, void 0, Leaf2, Leaf2));
      }
      fail();
    })();
    const $2 = _caseJson(
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      Just,
      (v) => Nothing,
      specJson
    );
    const $3 = (() => {
      if ($2.tag === "Nothing") {
        return $Either("Left", "Types are not an array.");
      }
      if ($2.tag === "Just") {
        const $32 = traverse(parseJsonValueType)($2._1);
        return (() => {
          if ($32.tag === "Left") {
            const $4 = $32._1;
            return (v) => $Either("Left", $4);
          }
          if ($32.tag === "Right") {
            const $4 = $32._1;
            return (f) => f($4);
          }
          fail();
        })()((types) => $Either("Right", fromFoldable22(types)));
      }
      fail();
    })();
    if ($1.tag === "Left") {
      return $3;
    }
    return $1;
  };
  var parseBooleanSchema = (json) => {
    const $0 = _caseJson(
      (v) => Nothing,
      Just,
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      json
    );
    if ($0.tag === "Nothing") {
      return $Either("Left", "Invalid schema: the JSON value is not a JSON boolean");
    }
    if ($0.tag === "Just") {
      return $Either("Right", $JsonSchema("BooleanSchema", $0._1));
    }
    fail();
  };
  var parseSchema = (json) => {
    const $0 = parseBooleanSchema(json);
    const $1 = parseObjectSchema(json);
    const $2 = $1.tag === "Left" ? $Either("Left", "the JSON value is neither a boolean nor an object") : $1;
    if ($0.tag === "Left") {
      return $2;
    }
    return $0;
  };
  var parseObjectSchema = (keywordsJson) => {
    const $0 = _caseJson(
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      (v) => Nothing,
      Just,
      keywordsJson
    );
    const $1 = (() => {
      if ($0.tag === "Nothing") {
        return $Either("Left", "Invalid schema: the JSON value is not a JSON object");
      }
      if ($0.tag === "Just") {
        return $Either("Right", $0._1);
      }
      fail();
    })();
    return (() => {
      if ($1.tag === "Left") {
        const $2 = $1._1;
        return (v) => $Either("Left", $2);
      }
      if ($1.tag === "Right") {
        const $2 = $1._1;
        return (f) => f($2);
      }
      fail();
    })()((schemaObject) => {
      const $2 = parseOptionalNumber("exclusiveMaximum")(schemaObject);
      return (() => {
        if ($2.tag === "Left") {
          const $3 = $2._1;
          return (v) => $Either("Left", $3);
        }
        if ($2.tag === "Right") {
          const $3 = $2._1;
          return (f) => f($3);
        }
        fail();
      })()((exclusiveMaximum) => {
        const $3 = parseOptionalNumber("exclusiveMinimum")(schemaObject);
        return (() => {
          if ($3.tag === "Left") {
            const $4 = $3._1;
            return (v) => $Either("Left", $4);
          }
          if ($3.tag === "Right") {
            const $4 = $3._1;
            return (f) => f($4);
          }
          fail();
        })()((exclusiveMinimum) => {
          const $4 = _lookup(Nothing, Just, "items", schemaObject);
          const $5 = (() => {
            if ($4.tag === "Just") {
              const $52 = parseSchema($4._1);
              if ($52.tag === "Left") {
                return $Either("Left", $52._1);
              }
              if ($52.tag === "Right") {
                return $Either("Right", $Maybe("Just", $52._1));
              }
              fail();
            }
            return $Either("Right", Nothing);
          })();
          return (() => {
            if ($5.tag === "Left") {
              const $6 = $5._1;
              return (v) => $Either("Left", $6);
            }
            if ($5.tag === "Right") {
              const $6 = $5._1;
              return (f) => f($6);
            }
            fail();
          })()((items2) => {
            const $6 = parseOptionalNumber("maximum")(schemaObject);
            return (() => {
              if ($6.tag === "Left") {
                const $7 = $6._1;
                return (v) => $Either("Left", $7);
              }
              if ($6.tag === "Right") {
                const $7 = $6._1;
                return (f) => f($7);
              }
              fail();
            })()((maximum) => {
              const $7 = parseOptionalNumber("minimum")(schemaObject);
              return (() => {
                if ($7.tag === "Left") {
                  const $8 = $7._1;
                  return (v) => $Either("Left", $8);
                }
                if ($7.tag === "Right") {
                  const $8 = $7._1;
                  return (f) => f($8);
                }
                fail();
              })()((minimum) => {
                const $8 = parseMultipleOf(schemaObject);
                return (() => {
                  if ($8.tag === "Left") {
                    const $9 = $8._1;
                    return (v) => $Either("Left", $9);
                  }
                  if ($8.tag === "Right") {
                    const $9 = $8._1;
                    return (f) => f($9);
                  }
                  fail();
                })()((multipleOf) => {
                  const $9 = _lookup(Nothing, Just, "not", schemaObject);
                  const $10 = (() => {
                    if ($9.tag === "Just") {
                      const $102 = parseSchema($9._1);
                      if ($102.tag === "Left") {
                        return $Either("Left", $102._1);
                      }
                      if ($102.tag === "Right") {
                        return $Either("Right", $Maybe("Just", $102._1));
                      }
                      fail();
                    }
                    return $Either("Right", Nothing);
                  })();
                  return (() => {
                    if ($10.tag === "Left") {
                      const $11 = $10._1;
                      return (v) => $Either("Left", $11);
                    }
                    if ($10.tag === "Right") {
                      const $11 = $10._1;
                      return (f) => f($11);
                    }
                    fail();
                  })()((not) => {
                    const $11 = _lookup(Nothing, Just, "required", schemaObject);
                    const $12 = (() => {
                      if ($11.tag === "Nothing") {
                        return $Either("Right", Leaf2);
                      }
                      if ($11.tag === "Just") {
                        return parseRequiredKeywordSpec($11._1);
                      }
                      fail();
                    })();
                    return (() => {
                      if ($12.tag === "Left") {
                        const $13 = $12._1;
                        return (v) => $Either("Left", $13);
                      }
                      if ($12.tag === "Right") {
                        const $13 = $12._1;
                        return (f) => f($13);
                      }
                      fail();
                    })()((required2) => {
                      const $13 = _lookup(Nothing, Just, "type", schemaObject);
                      if ($13.tag === "Just") {
                        const $14 = parseTypeKeywordSpec($13._1);
                        if ($14.tag === "Left") {
                          return $Either("Left", $14._1);
                        }
                        if ($14.tag === "Right") {
                          const $15 = _lookup(Nothing, Just, "uniqueItems", schemaObject);
                          if ($15.tag === "Nothing") {
                            return $Either(
                              "Right",
                              $JsonSchema(
                                "ObjectSchema",
                                {
                                  exclusiveMaximum,
                                  exclusiveMinimum,
                                  items: items2,
                                  maximum,
                                  minimum,
                                  multipleOf,
                                  not,
                                  required: required2,
                                  typeKeyword: $Maybe("Just", $14._1),
                                  uniqueItems: false
                                }
                              )
                            );
                          }
                          if ($15.tag === "Just") {
                            if (_caseJson((v) => true, (v) => false, (v) => false, (v) => false, (v) => false, (v) => false, $15._1)) {
                              return $Either(
                                "Right",
                                $JsonSchema(
                                  "ObjectSchema",
                                  {
                                    exclusiveMaximum,
                                    exclusiveMinimum,
                                    items: items2,
                                    maximum,
                                    minimum,
                                    multipleOf,
                                    not,
                                    required: required2,
                                    typeKeyword: $Maybe("Just", $14._1),
                                    uniqueItems: false
                                  }
                                )
                              );
                            }
                            const $16 = _caseJson(
                              (v) => Nothing,
                              Just,
                              (v) => Nothing,
                              (v) => Nothing,
                              (v) => Nothing,
                              (v) => Nothing,
                              $15._1
                            );
                            if ($16.tag === "Nothing") {
                              return $Either("Left", "Unique items is not a boolean.");
                            }
                            if ($16.tag === "Just") {
                              return $Either(
                                "Right",
                                $JsonSchema(
                                  "ObjectSchema",
                                  {
                                    exclusiveMaximum,
                                    exclusiveMinimum,
                                    items: items2,
                                    maximum,
                                    minimum,
                                    multipleOf,
                                    not,
                                    required: required2,
                                    typeKeyword: $Maybe("Just", $14._1),
                                    uniqueItems: $16._1
                                  }
                                )
                              );
                            }
                          }
                        }
                        fail();
                      }
                      if ($13.tag === "Nothing") {
                        const $14 = _lookup(Nothing, Just, "uniqueItems", schemaObject);
                        if ($14.tag === "Nothing") {
                          return $Either(
                            "Right",
                            $JsonSchema(
                              "ObjectSchema",
                              { exclusiveMaximum, exclusiveMinimum, items: items2, maximum, minimum, multipleOf, not, required: required2, typeKeyword: Nothing, uniqueItems: false }
                            )
                          );
                        }
                        if ($14.tag === "Just") {
                          if (_caseJson((v) => true, (v) => false, (v) => false, (v) => false, (v) => false, (v) => false, $14._1)) {
                            return $Either(
                              "Right",
                              $JsonSchema(
                                "ObjectSchema",
                                { exclusiveMaximum, exclusiveMinimum, items: items2, maximum, minimum, multipleOf, not, required: required2, typeKeyword: Nothing, uniqueItems: false }
                              )
                            );
                          }
                          const $15 = _caseJson(
                            (v) => Nothing,
                            Just,
                            (v) => Nothing,
                            (v) => Nothing,
                            (v) => Nothing,
                            (v) => Nothing,
                            $14._1
                          );
                          if ($15.tag === "Nothing") {
                            return $Either("Left", "Unique items is not a boolean.");
                          }
                          if ($15.tag === "Just") {
                            return $Either(
                              "Right",
                              $JsonSchema(
                                "ObjectSchema",
                                { exclusiveMaximum, exclusiveMinimum, items: items2, maximum, minimum, multipleOf, not, required: required2, typeKeyword: Nothing, uniqueItems: $15._1 }
                              )
                            );
                          }
                        }
                      }
                      fail();
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

  // output-es/Data.Array.NonEmpty/index.js
  var singleton3 = (x) => [x];
  var fromFoldable5 = (dictFoldable) => {
    const $0 = dictFoldable.foldr;
    return (x) => {
      const $1 = fromFoldableImpl($0, x);
      if ($1.length > 0) {
        return $Maybe("Just", $1);
      }
      return Nothing;
    };
  };

  // output-es/Data.Set.NonEmpty/index.js
  var toUnfoldable1 = (dictUnfoldable1) => {
    const stepNext = stepAscCps((k, v, next) => $Maybe("Just", $Tuple(k, next)))((v) => Nothing);
    const $0 = dictUnfoldable1.unfoldr1((v) => $Tuple(v._1, stepNext(v._2)));
    const $1 = stepAscCps((k, v, next) => $Tuple(k, next))((v) => _crashWith("toUnfoldable1: impossible"));
    return (x) => $0($1($MapIter("IterNode", x, IterLeaf)));
  };
  var toUnfoldable12 = /* @__PURE__ */ toUnfoldable1(unfoldable1NonEmptyList);
  var foldable1NonEmptySet = {
    foldMap1: (dictSemigroup) => {
      const foldMap11 = foldable1NonEmptyList.foldMap1(dictSemigroup);
      return (f) => {
        const $0 = foldMap11(f);
        return (x) => $0(toUnfoldable12(x));
      };
    },
    foldr1: (f) => {
      const $0 = foldable1NonEmptyList.foldr1(f);
      return (x) => $0(toUnfoldable12(x));
    },
    foldl1: (f) => {
      const $0 = foldable1NonEmptyList.foldl1(f);
      return (x) => $0(toUnfoldable12(x));
    },
    Foldable0: () => foldableSet
  };

  // output-es/Docs.Document/index.js
  var orderedList2 = /* @__PURE__ */ orderedList(foldable1NonEmptyArray);
  var unorderedList2 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var documentFoldable = (dictDocument) => (dictFoldable) => {
    const fromFoldable13 = fromFoldable5(dictFoldable);
    return (isOrdered) => {
      const $0 = $FlowContentNode("Paragraph", fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "\u2205")]));
      const $1 = isOrdered ? orderedList2 : unorderedList2;
      const $2 = (() => {
        const $22 = (() => {
          const $23 = (() => {
            const $24 = arrayMap((x) => {
              const $25 = dictDocument.document(x);
              return [$25._1, ...$25._2];
            });
            return (x) => $1($24(x));
          })();
          return (v2) => {
            if (v2.tag === "Nothing") {
              return $0;
            }
            if (v2.tag === "Just") {
              return $23(v2._1);
            }
            fail();
          };
        })();
        return (x) => $22(fromFoldable13(x));
      })();
      return (x) => $NonEmpty($2(x), []);
    };
  };

  // output-es/JsonSchema.Range/index.js
  var $Boundary = (tag, _1) => ({ tag, _1 });
  var eqBoundary = {
    eq: (x) => (y) => {
      if (x.tag === "Closed") {
        return y.tag === "Closed" && x._1 === y._1;
      }
      return x.tag === "Open" && y.tag === "Open" && x._1 === y._1;
    }
  };
  var ordBoundary = {
    compare: (x) => (y) => {
      if (x.tag === "Closed") {
        if (y.tag === "Closed") {
          return ordNumber.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Closed") {
        return GT;
      }
      if (x.tag === "Open" && y.tag === "Open") {
        return ordNumber.compare(x._1)(y._1);
      }
      fail();
    },
    Eq0: () => eqBoundary
  };
  var encodeJsonBoundary = {
    encodeJson: (v) => {
      if (v.tag === "Closed") {
        return id(showNumberImpl(v._1) + " (inclusively)");
      }
      if (v.tag === "Open") {
        return id(showNumberImpl(v._1) + " (exclusively)");
      }
      fail();
    }
  };
  var renderRange = (range2) => $PhrasingContentNode(
    "InlineCode",
    (() => {
      if (range2.from.tag === "Closed") {
        return "[" + showNumberImpl(range2.from._1) + ",";
      }
      if (range2.from.tag === "Open") {
        return "(" + showNumberImpl(range2.from._1) + ",";
      }
      fail();
    })() + (() => {
      if (range2.to.tag === "Closed") {
        return showNumberImpl(range2.to._1) + "]";
      }
      if (range2.to.tag === "Open") {
        return showNumberImpl(range2.to._1) + ")";
      }
      fail();
    })()
  );

  // output-es/JsonSchema.SchemaPath/index.js
  var $SchemaPathSegment = (tag, _1) => ({ tag, _1 });
  var ExclusiveMaximum = /* @__PURE__ */ $SchemaPathSegment("ExclusiveMaximum");
  var ExclusiveMinimum = /* @__PURE__ */ $SchemaPathSegment("ExclusiveMinimum");
  var Items = /* @__PURE__ */ $SchemaPathSegment("Items");
  var Maximum = /* @__PURE__ */ $SchemaPathSegment("Maximum");
  var Minimum = /* @__PURE__ */ $SchemaPathSegment("Minimum");
  var MultipleOf = /* @__PURE__ */ $SchemaPathSegment("MultipleOf");
  var TypeKeyword = /* @__PURE__ */ $SchemaPathSegment("TypeKeyword");
  var UniqueItems = /* @__PURE__ */ $SchemaPathSegment("UniqueItems");
  var eqSchemaPathSegment = {
    eq: (x) => (y) => {
      if (x.tag === "ExclusiveMaximum") {
        return y.tag === "ExclusiveMaximum";
      }
      if (x.tag === "ExclusiveMinimum") {
        return y.tag === "ExclusiveMinimum";
      }
      if (x.tag === "Items") {
        return y.tag === "Items";
      }
      if (x.tag === "Maximum") {
        return y.tag === "Maximum";
      }
      if (x.tag === "Minimum") {
        return y.tag === "Minimum";
      }
      if (x.tag === "MultipleOf") {
        return y.tag === "MultipleOf";
      }
      if (x.tag === "Properties") {
        return y.tag === "Properties" && x._1 === y._1;
      }
      if (x.tag === "TypeKeyword") {
        return y.tag === "TypeKeyword";
      }
      return x.tag === "UniqueItems" && y.tag === "UniqueItems";
    }
  };
  var ordSchemaPathSegment = {
    compare: (x) => (y) => {
      if (x.tag === "ExclusiveMaximum") {
        if (y.tag === "ExclusiveMaximum") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "ExclusiveMaximum") {
        return GT;
      }
      if (x.tag === "ExclusiveMinimum") {
        if (y.tag === "ExclusiveMinimum") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "ExclusiveMinimum") {
        return GT;
      }
      if (x.tag === "Items") {
        if (y.tag === "Items") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "Items") {
        return GT;
      }
      if (x.tag === "Maximum") {
        if (y.tag === "Maximum") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "Maximum") {
        return GT;
      }
      if (x.tag === "Minimum") {
        if (y.tag === "Minimum") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "Minimum") {
        return GT;
      }
      if (x.tag === "MultipleOf") {
        if (y.tag === "MultipleOf") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "MultipleOf") {
        return GT;
      }
      if (x.tag === "Properties") {
        if (y.tag === "Properties") {
          return ordString.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "Properties") {
        return GT;
      }
      if (x.tag === "TypeKeyword") {
        if (y.tag === "TypeKeyword") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "TypeKeyword") {
        return GT;
      }
      if (x.tag === "UniqueItems" && y.tag === "UniqueItems") {
        return EQ;
      }
      fail();
    },
    Eq0: () => eqSchemaPathSegment
  };
  var encodeJsonSchemaPathSegme = {
    encodeJson: (v) => {
      if (v.tag === "ExclusiveMinimum") {
        return id("exclusiveMinimum");
      }
      if (v.tag === "ExclusiveMaximum") {
        return id("exclusiveMaximum");
      }
      if (v.tag === "Items") {
        return id("items");
      }
      if (v.tag === "Maximum") {
        return id("maximum");
      }
      if (v.tag === "Minimum") {
        return id("minimum");
      }
      if (v.tag === "MultipleOf") {
        return id("multipleOf");
      }
      if (v.tag === "Properties") {
        return id("(NonEmptyString.unsafeFromString " + showStringImpl(v._1) + ")");
      }
      if (v.tag === "TypeKeyword") {
        return id("type");
      }
      if (v.tag === "UniqueItems") {
        return id("uniqueItems");
      }
      fail();
    }
  };
  var render3 = /* @__PURE__ */ (() => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = b + "/" + (() => {
            if (v._1.tag === "ExclusiveMinimum") {
              return "exclusiveMinimum";
            }
            if (v._1.tag === "ExclusiveMaximum") {
              return "exclusiveMaximum";
            }
            if (v._1.tag === "Items") {
              return "items";
            }
            if (v._1.tag === "Maximum") {
              return "maximum";
            }
            if (v._1.tag === "Minimum") {
              return "minimum";
            }
            if (v._1.tag === "MultipleOf") {
              return "multipleOf";
            }
            if (v._1.tag === "Properties") {
              return "properties/" + v._1._1;
            }
            if (v._1.tag === "TypeKeyword") {
              return "type";
            }
            if (v._1.tag === "UniqueItems") {
              return "uniqueItems";
            }
            fail();
          })();
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    const $0 = go("#");
    return (x) => $0(reverse2(x));
  })();

  // output-es/JsonSchema.Compatibility/index.js
  var $BackwardIncompatibilityType = (tag, _1) => ({ tag, _1 });
  var $Compatibility = (tag, _1) => ({ tag, _1 });
  var $ForwardIncompatibilityType = (tag, _1) => ({ tag, _1 });
  var $NumberRangeChange = (tag, _1, _2) => ({ tag, _1, _2 });
  var fromIsSymbol = { reflectSymbol: () => "from" };
  var toIsSymbol = { reflectSymbol: () => "to" };
  var newIsSymbol = { reflectSymbol: () => "new" };
  var oldIsSymbol = { reflectSymbol: () => "old" };
  var incompatibilityTypeIsSymbol = { reflectSymbol: () => "incompatibilityType" };
  var pathIsSymbol = { reflectSymbol: () => "path" };
  var compare22 = /* @__PURE__ */ (() => ordSet(ordJsonValueType).compare)();
  var encodeJson = /* @__PURE__ */ (() => {
    const $0 = gEncodeJsonCons(encodeJsonBoundary)(gEncodeJsonCons(encodeJsonBoundary)(gEncodeJsonNil)(toIsSymbol)())(fromIsSymbol)();
    return (rec) => id($0.gEncodeJson(rec)($$Proxy));
  })();
  var encodeJson1 = /* @__PURE__ */ (() => {
    const $0 = gEncodeJsonCons(encodeJsonJNumber)(gEncodeJsonCons(encodeJsonJNumber)(gEncodeJsonNil)(oldIsSymbol)())(newIsSymbol)();
    return (rec) => id($0.gEncodeJson(rec)($$Proxy));
  })();
  var encodeJson2 = /* @__PURE__ */ (() => encodeSet(ordJsonValueType)(encodeJsonJsonValueType).encodeJson)();
  var gEncodeJsonCons2 = /* @__PURE__ */ (() => gEncodeJsonCons({
    encodeJson: encodeList(encodeJsonSchemaPathSegme.encodeJson)
  })(gEncodeJsonNil)(pathIsSymbol)())();
  var join1With = (splice) => (xs) => foldlArray((v) => (v1) => {
    if (v.init) {
      return { init: false, acc: v1 };
    }
    return { init: false, acc: v.acc + splice + v1 };
  })({ init: true, acc: "" })(xs).acc;
  var fromFoldable6 = ($0) => fromFoldableImpl((f) => (b) => (v) => f(v._1)(foldrArray(f)(b)(v._2)), $0);
  var unorderedList3 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var fromFoldable11 = /* @__PURE__ */ (() => {
    const $0 = foldable1NonEmpty(foldableArray).Foldable0().foldr;
    return (x) => fromFoldableImpl($0, x);
  })();
  var append1 = (v) => (v1) => $NonEmpty(v._1, [...v._2, v1._1, ...v1._2]);
  var member = (k) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Leaf") {
          go$c = false;
          go$r = false;
          continue;
        }
        if (v.tag === "Node") {
          const v1 = ordJsonValueType.compare(k)(v._3);
          if (v1 === "LT") {
            go$a0 = v._5;
            continue;
          }
          if (v1 === "GT") {
            go$a0 = v._6;
            continue;
          }
          if (v1 === "EQ") {
            go$c = false;
            go$r = true;
            continue;
          }
        }
        fail();
      }
      return go$r;
    };
    return go;
  };
  var fromFoldable23 = /* @__PURE__ */ foldlArray((m) => (a) => insert(ordJsonValueType)(a)()(m))(Leaf2);
  var Full = /* @__PURE__ */ $Compatibility("Full");
  var eqNumberRangeChange = {
    eq: (x) => (y) => {
      if (x.tag === "Lower") {
        return y.tag === "Lower" && (x._1.from.tag === "Closed" ? y._1.from.tag === "Closed" && x._1.from._1 === y._1.from._1 : x._1.from.tag === "Open" && y._1.from.tag === "Open" && x._1.from._1 === y._1.from._1) && (x._1.to.tag === "Closed" ? y._1.to.tag === "Closed" && x._1.to._1 === y._1.to._1 : x._1.to.tag === "Open" && y._1.to.tag === "Open" && x._1.to._1 === y._1.to._1);
      }
      if (x.tag === "LowerAndUpper") {
        return y.tag === "LowerAndUpper" && (x._1.from.tag === "Closed" ? y._1.from.tag === "Closed" && x._1.from._1 === y._1.from._1 : x._1.from.tag === "Open" && y._1.from.tag === "Open" && x._1.from._1 === y._1.from._1) && (x._1.to.tag === "Closed" ? y._1.to.tag === "Closed" && x._1.to._1 === y._1.to._1 : x._1.to.tag === "Open" && y._1.to.tag === "Open" && x._1.to._1 === y._1.to._1) && (x._2.from.tag === "Closed" ? y._2.from.tag === "Closed" && x._2.from._1 === y._2.from._1 : x._2.from.tag === "Open" && y._2.from.tag === "Open" && x._2.from._1 === y._2.from._1) && (x._2.to.tag === "Closed" ? y._2.to.tag === "Closed" && x._2.to._1 === y._2.to._1 : x._2.to.tag === "Open" && y._2.to.tag === "Open" && x._2.to._1 === y._2.to._1);
      }
      return x.tag === "Upper" && y.tag === "Upper" && (x._1.from.tag === "Closed" ? y._1.from.tag === "Closed" && x._1.from._1 === y._1.from._1 : x._1.from.tag === "Open" && y._1.from.tag === "Open" && x._1.from._1 === y._1.from._1) && (x._1.to.tag === "Closed" ? y._1.to.tag === "Closed" && x._1.to._1 === y._1.to._1 : x._1.to.tag === "Open" && y._1.to.tag === "Open" && x._1.to._1 === y._1.to._1);
    }
  };
  var ordNumberRangeChange = {
    compare: (x) => (y) => {
      if (x.tag === "Lower") {
        if (y.tag === "Lower") {
          const v = ordBoundary.compare(x._1.from)(y._1.from);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordBoundary.compare(x._1.to)(y._1.to);
        }
        return LT;
      }
      if (y.tag === "Lower") {
        return GT;
      }
      if (x.tag === "LowerAndUpper") {
        if (y.tag === "LowerAndUpper") {
          const v1 = ordBoundary.compare(x._1.from)(y._1.from);
          if (v1 === "LT") {
            return LT;
          }
          if (v1 === "GT") {
            return GT;
          }
          if (ordBoundary.compare(x._1.to)(y._1.to) === "LT") {
            return LT;
          }
          if (ordBoundary.compare(x._1.to)(y._1.to) === "GT") {
            return GT;
          }
          const v1$1 = ordBoundary.compare(x._2.from)(y._2.from);
          if (v1$1 === "LT") {
            return LT;
          }
          if (v1$1 === "GT") {
            return GT;
          }
          return ordBoundary.compare(x._2.to)(y._2.to);
        }
        return LT;
      }
      if (y.tag === "LowerAndUpper") {
        return GT;
      }
      if (x.tag === "Upper" && y.tag === "Upper") {
        const v = ordBoundary.compare(x._1.from)(y._1.from);
        if (v === "LT") {
          return LT;
        }
        if (v === "GT") {
          return GT;
        }
        return ordBoundary.compare(x._1.to)(y._1.to);
      }
      fail();
    },
    Eq0: () => eqNumberRangeChange
  };
  var eqForwardIncompatibilityT = {
    eq: (x) => (y) => {
      if (x.tag === "MultipleWithdrawn") {
        return y.tag === "MultipleWithdrawn" && x._1 === y._1;
      }
      if (x.tag === "NewMultipleIsNotFactorOfOldMultiple") {
        return y.tag === "NewMultipleIsNotFactorOfOldMultiple" && x._1.new === y._1.new && x._1.old === y._1.old;
      }
      if (x.tag === "RangeOfAllowedNumbersExtended") {
        return y.tag === "RangeOfAllowedNumbersExtended" && eqNumberRangeChange.eq(x._1)(y._1);
      }
      return x.tag === "SetOfAllowedTypesExtended" && y.tag === "SetOfAllowedTypesExtended" && eqMap(eqJsonValueType)(eqUnit).eq(x._1)(y._1);
    }
  };
  var ordForwardIncompatibility = {
    compare: (x) => (y) => {
      if (x.tag === "MultipleWithdrawn") {
        if (y.tag === "MultipleWithdrawn") {
          return ordNumber.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "MultipleWithdrawn") {
        return GT;
      }
      if (x.tag === "NewMultipleIsNotFactorOfOldMultiple") {
        if (y.tag === "NewMultipleIsNotFactorOfOldMultiple") {
          const v = ordNumber.compare(x._1.new)(y._1.new);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordNumber.compare(x._1.old)(y._1.old);
        }
        return LT;
      }
      if (y.tag === "NewMultipleIsNotFactorOfOldMultiple") {
        return GT;
      }
      if (x.tag === "RangeOfAllowedNumbersExtended") {
        if (y.tag === "RangeOfAllowedNumbersExtended") {
          return ordNumberRangeChange.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "RangeOfAllowedNumbersExtended") {
        return GT;
      }
      if (x.tag === "SetOfAllowedTypesExtended" && y.tag === "SetOfAllowedTypesExtended") {
        return compare22(x._1)(y._1);
      }
      fail();
    },
    Eq0: () => eqForwardIncompatibilityT
  };
  var ordForwardIncompatibility1 = /* @__PURE__ */ ordRecord()(/* @__PURE__ */ (() => {
    const $0 = ordList(ordSchemaPathSegment);
    const $1 = $0.Eq0();
    const eqRowCons2 = { eqRecord: (v) => (ra) => (rb) => eqForwardIncompatibilityT.eq(ra.incompatibilityType)(rb.incompatibilityType) && $1.eq(ra.path)(rb.path) };
    return {
      compareRecord: (v) => (ra) => (rb) => {
        const left = ordForwardIncompatibility.compare(ra.incompatibilityType)(rb.incompatibilityType);
        if (left === "LT" || left === "GT" || left !== "EQ") {
          return left;
        }
        const left$1 = $0.compare(ra.path)(rb.path);
        if (left$1 === "LT" || left$1 === "GT" || left$1 !== "EQ") {
          return left$1;
        }
        return EQ;
      },
      EqRecord0: () => eqRowCons2
    };
  })());
  var eqBackwardIncompatibility = {
    eq: (x) => (y) => {
      if (x.tag === "MultipleIntroduced") {
        return y.tag === "MultipleIntroduced" && x._1 === y._1;
      }
      if (x.tag === "OldMultipleIsNotFactorOfNewMultiple") {
        return y.tag === "OldMultipleIsNotFactorOfNewMultiple" && x._1.new === y._1.new && x._1.old === y._1.old;
      }
      if (x.tag === "RangeOfAllowedNumbersReduced") {
        return y.tag === "RangeOfAllowedNumbersReduced" && eqNumberRangeChange.eq(x._1)(y._1);
      }
      return x.tag === "SetOfAllowedTypesReduced" && y.tag === "SetOfAllowedTypesReduced" && eqMap(eqJsonValueType)(eqUnit).eq(x._1)(y._1);
    }
  };
  var ordBackwardIncompatibilit = {
    compare: (x) => (y) => {
      if (x.tag === "MultipleIntroduced") {
        if (y.tag === "MultipleIntroduced") {
          return ordNumber.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "MultipleIntroduced") {
        return GT;
      }
      if (x.tag === "OldMultipleIsNotFactorOfNewMultiple") {
        if (y.tag === "OldMultipleIsNotFactorOfNewMultiple") {
          const v = ordNumber.compare(x._1.new)(y._1.new);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordNumber.compare(x._1.old)(y._1.old);
        }
        return LT;
      }
      if (y.tag === "OldMultipleIsNotFactorOfNewMultiple") {
        return GT;
      }
      if (x.tag === "RangeOfAllowedNumbersReduced") {
        if (y.tag === "RangeOfAllowedNumbersReduced") {
          return ordNumberRangeChange.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "RangeOfAllowedNumbersReduced") {
        return GT;
      }
      if (x.tag === "SetOfAllowedTypesReduced" && y.tag === "SetOfAllowedTypesReduced") {
        return compare22(x._1)(y._1);
      }
      fail();
    },
    Eq0: () => eqBackwardIncompatibility
  };
  var ordBackwardIncompatibilit1 = /* @__PURE__ */ ordRecord()(/* @__PURE__ */ (() => {
    const $0 = ordList(ordSchemaPathSegment);
    const $1 = $0.Eq0();
    const eqRowCons2 = { eqRecord: (v) => (ra) => (rb) => eqBackwardIncompatibility.eq(ra.incompatibilityType)(rb.incompatibilityType) && $1.eq(ra.path)(rb.path) };
    return {
      compareRecord: (v) => (ra) => (rb) => {
        const left = ordBackwardIncompatibilit.compare(ra.incompatibilityType)(rb.incompatibilityType);
        if (left === "LT" || left === "GT" || left !== "EQ") {
          return left;
        }
        const left$1 = $0.compare(ra.path)(rb.path);
        if (left$1 === "LT" || left$1 === "GT" || left$1 !== "EQ") {
          return left$1;
        }
        return EQ;
      },
      EqRecord0: () => eqRowCons2
    };
  })());
  var encodeJsonNumberRangeChan = {
    encodeJson: (v) => {
      if (v.tag === "Lower") {
        return extend(encodeJsonJson.encodeJson)($Tuple("lower", encodeJson(v._1)))(jsonEmptyObject);
      }
      if (v.tag === "LowerAndUpper") {
        return extend(encodeJsonJson.encodeJson)($Tuple("lower", encodeJson(v._1)))(extend(encodeJsonJson.encodeJson)($Tuple(
          "upper",
          encodeJson(v._2)
        ))(jsonEmptyObject));
      }
      if (v.tag === "Upper") {
        return extend(encodeJsonJson.encodeJson)($Tuple("upper", encodeJson(v._1)))(jsonEmptyObject);
      }
      fail();
    }
  };
  var encodeJsonForwardIncompat = {
    encodeJson: (v) => {
      if (v.tag === "MultipleWithdrawn") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "multipleWithdrawn",
          id(v._1)
        ))(jsonEmptyObject);
      }
      if (v.tag === "NewMultipleIsNotFactorOfOldMultiple") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "newMultipleIsNotFactorOfOldMultiple",
          encodeJson1({ new: v._1.new, old: v._1.old })
        ))(jsonEmptyObject);
      }
      if (v.tag === "RangeOfAllowedNumbersExtended") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "rangeOfAllowedNumbersExtended",
          encodeJsonNumberRangeChan.encodeJson(v._1)
        ))(jsonEmptyObject);
      }
      if (v.tag === "SetOfAllowedTypesExtended") {
        return extend(encodeJsonJson.encodeJson)($Tuple("setOfAllowedTypesExtended", encodeJson2(v._1)))(jsonEmptyObject);
      }
      fail();
    }
  };
  var encodeJsonForwardIncompat1 = /* @__PURE__ */ (() => {
    const $0 = gEncodeJsonCons(encodeJsonForwardIncompat)(gEncodeJsonCons2)(incompatibilityTypeIsSymbol)();
    return { encodeJson: (rec) => id($0.gEncodeJson(rec)($$Proxy)) };
  })();
  var encodeJson4 = /* @__PURE__ */ (() => encodeSet(ordForwardIncompatibility1)(encodeJsonForwardIncompat1).encodeJson)();
  var encodeJsonBackwardIncompa = {
    encodeJson: (v) => {
      if (v.tag === "MultipleIntroduced") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "multipleIntroduced",
          id(v._1)
        ))(jsonEmptyObject);
      }
      if (v.tag === "OldMultipleIsNotFactorOfNewMultiple") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "oldMultipleIsNotFactorOfNewMultiple",
          encodeJson1({ new: v._1.new, old: v._1.old })
        ))(jsonEmptyObject);
      }
      if (v.tag === "RangeOfAllowedNumbersReduced") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "rangeOfAllowedNumbersReduced",
          encodeJsonNumberRangeChan.encodeJson(v._1)
        ))(jsonEmptyObject);
      }
      if (v.tag === "SetOfAllowedTypesReduced") {
        return extend(encodeJsonJson.encodeJson)($Tuple("setOfAllowedTypesReduced", encodeJson2(v._1)))(jsonEmptyObject);
      }
      fail();
    }
  };
  var encodeJsonBackwardIncompa1 = /* @__PURE__ */ (() => {
    const $0 = gEncodeJsonCons(encodeJsonBackwardIncompa)(gEncodeJsonCons2)(incompatibilityTypeIsSymbol)();
    return { encodeJson: (rec) => id($0.gEncodeJson(rec)($$Proxy)) };
  })();
  var encodeJson5 = /* @__PURE__ */ (() => encodeSet(ordBackwardIncompatibilit1)(encodeJsonBackwardIncompa1).encodeJson)();
  var encodeJsonCompatibility = {
    encodeJson: (v) => {
      if (v.tag === "Backward") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "compatibilityType",
          id("backward")
        ))(extend(encodeJsonJson.encodeJson)($Tuple(
          "incompabilities",
          extend(encodeJsonJson.encodeJson)($Tuple("forward", encodeJson4(v._1.forwardIncompatibilities)))(jsonEmptyString)
        ))(jsonEmptyObject));
      }
      if (v.tag === "Forward") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "compatibilityType",
          id("forward")
        ))(extend(encodeJsonJson.encodeJson)($Tuple(
          "incompabilities",
          extend(encodeJsonJson.encodeJson)($Tuple("backward", encodeJson5(v._1.backwardIncompatibilities)))(jsonEmptyString)
        ))(jsonEmptyObject));
      }
      if (v.tag === "Full") {
        return jsonEmptyObject;
      }
      if (v.tag === "None") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "compatibilityType",
          id("none")
        ))(extend(encodeJsonJson.encodeJson)($Tuple(
          "incompabilities",
          extend(encodeJsonJson.encodeJson)($Tuple("backward", encodeJson5(v._1.backwardIncompatibilities)))(extend(encodeJsonJson.encodeJson)($Tuple(
            "forward",
            encodeJson4(v._1.forwardIncompatibilities)
          ))(jsonEmptyString))
        ))(jsonEmptyObject));
      }
      fail();
    }
  };
  var renderNumberRangeChange = (v) => {
    if (v.tag === "Lower") {
      return [renderRange(v._1)];
    }
    if (v.tag === "LowerAndUpper") {
      return [renderRange(v._1), $PhrasingContentNode("Text", " and "), renderRange(v._2)];
    }
    if (v.tag === "Upper") {
      return [renderRange(v._1)];
    }
    fail();
  };
  var documentBackwardIncompati = {
    document: (x) => $NonEmpty(
      $FlowContentNode(
        "Paragraph",
        fromFoldableImpl(
          foldrArray,
          (() => {
            if (x.tag === "MultipleIntroduced") {
              return [
                $PhrasingContentNode("Text", "numerical values must be multiples of "),
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const $0 = showNumberImpl(x._1);
                    if ($0 === "") {
                      return "impossible";
                    }
                    return $0;
                  })()
                ),
                $PhrasingContentNode("Text", " now")
              ];
            }
            if (x.tag === "OldMultipleIsNotFactorOfNewMultiple") {
              return [
                $PhrasingContentNode("Text", "the old multiple constraint of "),
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const $0 = showNumberImpl(x._1.old);
                    if ($0 === "") {
                      return "impossible";
                    }
                    return $0;
                  })()
                ),
                $PhrasingContentNode("Text", " is not a factor of the new multiple constraint of "),
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const $0 = showNumberImpl(x._1.new);
                    if ($0 === "") {
                      return "impossible";
                    }
                    return $0;
                  })()
                )
              ];
            }
            if (x.tag === "RangeOfAllowedNumbersReduced") {
              return [$PhrasingContentNode("Text", "the range of allowed values has been reduced by "), ...renderNumberRangeChange(x._1)];
            }
            if (x.tag === "SetOfAllowedTypesReduced") {
              return [
                $PhrasingContentNode("Text", "the set of allowed JSON value types has been reduced by "),
                $PhrasingContentNode(
                  "Text",
                  join1With(", ")(arrayMap(renderJsonValueType)(fromFoldableImpl(foldableSet.foldr, x._1)))
                )
              ];
            }
            fail();
          })()
        )
      ),
      []
    )
  };
  var documentBackwardIncompati1 = {
    document: (v) => $NonEmpty(
      $FlowContentNode(
        "Paragraph",
        fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "schema path: " + render3(v.path))])
      ),
      fromFoldable6(documentBackwardIncompati.document(v.incompatibilityType))
    )
  };
  var documentForwardIncompatib = {
    document: (x) => $NonEmpty(
      $FlowContentNode(
        "Paragraph",
        fromFoldableImpl(
          foldrArray,
          (() => {
            if (x.tag === "MultipleWithdrawn") {
              return [
                $PhrasingContentNode("Text", "numerical values must not be multiples of "),
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const $0 = showNumberImpl(x._1);
                    if ($0 === "") {
                      return "impossible";
                    }
                    return $0;
                  })()
                ),
                $PhrasingContentNode("Text", "anymore")
              ];
            }
            if (x.tag === "NewMultipleIsNotFactorOfOldMultiple") {
              return [
                $PhrasingContentNode("Text", "the new multiple constraint of "),
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const $0 = showNumberImpl(x._1.new);
                    if ($0 === "") {
                      return "impossible";
                    }
                    return $0;
                  })()
                ),
                $PhrasingContentNode("Text", " is not a factor of the old multiple constraint of "),
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const $0 = showNumberImpl(x._1.old);
                    if ($0 === "") {
                      return "impossible";
                    }
                    return $0;
                  })()
                )
              ];
            }
            if (x.tag === "RangeOfAllowedNumbersExtended") {
              return [$PhrasingContentNode("Text", "the range of allowed values has been extended by "), ...renderNumberRangeChange(x._1)];
            }
            if (x.tag === "SetOfAllowedTypesExtended") {
              return [
                $PhrasingContentNode("Text", "the set of allowed JSON value types has been extended by "),
                $PhrasingContentNode(
                  "Text",
                  join1With(", ")(arrayMap(renderJsonValueType)(fromFoldableImpl(foldableSet.foldr, x._1)))
                )
              ];
            }
            fail();
          })()
        )
      ),
      []
    )
  };
  var documentForwardIncompatib1 = {
    document: (v) => $NonEmpty(
      $FlowContentNode(
        "Paragraph",
        fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "schema path: " + render3(v.path))])
      ),
      fromFoldable6(documentForwardIncompatib.document(v.incompatibilityType))
    )
  };
  var documentCompatibility = {
    document: (v) => {
      if (v.tag === "Backward") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "Reasons for breaking the forward compatibility:")])
          ),
          [
            unorderedList3(arrayMap(singleton3)(fromFoldable11(documentFoldable(documentForwardIncompatib1)(foldableSet)(false)(v._1.forwardIncompatibilities))))
          ]
        );
      }
      if (v.tag === "Forward") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "Reasons for breaking the backward compatibility:")])
          ),
          [
            unorderedList3(arrayMap(singleton3)(fromFoldable11(documentFoldable(documentBackwardIncompati1)(foldableSet)(false)(v._1.backwardIncompatibilities))))
          ]
        );
      }
      if (v.tag === "Full") {
        return $NonEmpty(
          $FlowContentNode("Paragraph", fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "\u2713")])),
          []
        );
      }
      if (v.tag === "None") {
        return append1(documentCompatibility.document($Compatibility("Backward", { forwardIncompatibilities: v._1.forwardIncompatibilities })))(documentCompatibility.document($Compatibility(
          "Forward",
          { backwardIncompatibilities: v._1.backwardIncompatibilities }
        )));
      }
      fail();
    }
  };
  var mergeCompatibility = (v) => (v1) => {
    if (v.tag === "Backward") {
      if (v1.tag === "Backward") {
        return $Compatibility(
          "Backward",
          {
            forwardIncompatibilities: unsafeUnionWith(
              ordForwardIncompatibility1.compare,
              $$const,
              v._1.forwardIncompatibilities,
              v1._1.forwardIncompatibilities
            )
          }
        );
      }
      if (v1.tag === "Forward") {
        return $Compatibility("None", { backwardIncompatibilities: v1._1.backwardIncompatibilities, forwardIncompatibilities: v._1.forwardIncompatibilities });
      }
      if (v1.tag === "Full") {
        return $Compatibility("Backward", v._1);
      }
      if (v1.tag === "None") {
        return $Compatibility(
          "None",
          {
            ...v1._1,
            forwardIncompatibilities: unsafeUnionWith(
              ordForwardIncompatibility1.compare,
              $$const,
              v._1.forwardIncompatibilities,
              v1._1.forwardIncompatibilities
            )
          }
        );
      }
      fail();
    }
    if (v.tag === "Forward") {
      if (v1.tag === "Backward") {
        return $Compatibility("None", { backwardIncompatibilities: v._1.backwardIncompatibilities, forwardIncompatibilities: v1._1.forwardIncompatibilities });
      }
      if (v1.tag === "Forward") {
        return $Compatibility(
          "Forward",
          {
            backwardIncompatibilities: unsafeUnionWith(
              ordBackwardIncompatibilit1.compare,
              $$const,
              v._1.backwardIncompatibilities,
              v1._1.backwardIncompatibilities
            )
          }
        );
      }
      if (v1.tag === "Full") {
        return $Compatibility("Forward", v._1);
      }
      if (v1.tag === "None") {
        return $Compatibility(
          "None",
          {
            ...v1._1,
            backwardIncompatibilities: unsafeUnionWith(
              ordBackwardIncompatibilit1.compare,
              $$const,
              v._1.backwardIncompatibilities,
              v1._1.backwardIncompatibilities
            )
          }
        );
      }
      fail();
    }
    if (v.tag === "Full") {
      return v1;
    }
    if (v.tag === "None") {
      if (v1.tag === "Backward") {
        return $Compatibility(
          "None",
          {
            ...v._1,
            forwardIncompatibilities: unsafeUnionWith(
              ordForwardIncompatibility1.compare,
              $$const,
              v1._1.forwardIncompatibilities,
              v._1.forwardIncompatibilities
            )
          }
        );
      }
      if (v1.tag === "Forward") {
        return $Compatibility(
          "None",
          {
            ...v._1,
            backwardIncompatibilities: unsafeUnionWith(
              ordBackwardIncompatibilit1.compare,
              $$const,
              v1._1.backwardIncompatibilities,
              v._1.backwardIncompatibilities
            )
          }
        );
      }
      if (v1.tag === "None") {
        return $Compatibility(
          "None",
          {
            backwardIncompatibilities: unsafeUnionWith(
              ordBackwardIncompatibilit1.compare,
              $$const,
              v._1.backwardIncompatibilities,
              v1._1.backwardIncompatibilities
            ),
            forwardIncompatibilities: unsafeUnionWith(
              ordForwardIncompatibility1.compare,
              $$const,
              v._1.forwardIncompatibilities,
              v1._1.forwardIncompatibilities
            )
          }
        );
      }
      if (v1.tag === "Full") {
        return $Compatibility("None", v._1);
      }
    }
    fail();
  };
  var effectiveTypes = (v) => {
    if (v.tag === "Just") {
      if (member(JsonNumber)(v._1)) {
        return insert(ordJsonValueType)(JsonInteger)()(v._1);
      }
      return v._1;
    }
    if (v.tag === "Nothing") {
      return fromFoldable23([
        JsonArray,
        JsonBoolean,
        JsonInteger,
        JsonNumber,
        JsonNull,
        JsonObject,
        JsonString
      ]);
    }
    fail();
  };
  var calculateTypeChange = (mbTypesBefore) => (mbTypesAfter) => {
    const acceptedTypesBefore = effectiveTypes(mbTypesBefore);
    const acceptedTypesAfter = effectiveTypes(mbTypesAfter);
    return mergeCompatibility((() => {
      const $0 = unsafeDifference(ordJsonValueType.compare, acceptedTypesAfter, acceptedTypesBefore);
      if ($0.tag === "Leaf") {
        return Full;
      }
      return $Compatibility(
        "Backward",
        {
          forwardIncompatibilities: $$$Map(
            "Node",
            1,
            1,
            { incompatibilityType: $ForwardIncompatibilityType("SetOfAllowedTypesExtended", $0), path: Nil },
            void 0,
            Leaf2,
            Leaf2
          )
        }
      );
    })())((() => {
      const $0 = unsafeDifference(ordJsonValueType.compare, acceptedTypesBefore, acceptedTypesAfter);
      if ($0.tag === "Leaf") {
        return Full;
      }
      return $Compatibility(
        "Forward",
        {
          backwardIncompatibilities: $$$Map(
            "Node",
            1,
            1,
            { incompatibilityType: $BackwardIncompatibilityType("SetOfAllowedTypesReduced", $0), path: Nil },
            void 0,
            Leaf2,
            Leaf2
          )
        }
      );
    })());
  };
  var calculateRangeChange = (differences) => {
    const mbReductionUpperRange = foldableSet.foldl((acc) => (v) => {
      if (v.differenceType.tag === "ExclusiveMaximumChange") {
        if (v.differenceType._2.tag === "Just") {
          if (v.differenceType._1.tag === "Nothing") {
            return $Maybe("Just", { from: $Boundary("Open", v.differenceType._2._1), to: $Boundary("Open", Infinity) });
          }
          if (v.differenceType._1.tag === "Just" && v.differenceType._2._1 < v.differenceType._1._1) {
            return $Maybe("Just", { from: $Boundary("Closed", v.differenceType._2._1), to: $Boundary("Open", v.differenceType._1._1) });
          }
        }
        return acc;
      }
      if (v.differenceType.tag === "MaximumChange" && v.differenceType._2.tag === "Just") {
        if (v.differenceType._1.tag === "Nothing") {
          return $Maybe("Just", { from: $Boundary("Open", v.differenceType._2._1), to: $Boundary("Open", Infinity) });
        }
        if (v.differenceType._1.tag === "Just" && v.differenceType._2._1 < v.differenceType._1._1) {
          return $Maybe("Just", { from: $Boundary("Open", v.differenceType._2._1), to: $Boundary("Closed", v.differenceType._1._1) });
        }
      }
      return acc;
    })(Nothing)(differences);
    const mbReductionLowerRange = foldableSet.foldl((acc) => (v) => {
      if (v.differenceType.tag === "ExclusiveMinimumChange") {
        if (v.differenceType._2.tag === "Just") {
          if (v.differenceType._1.tag === "Nothing") {
            return $Maybe("Just", { from: $Boundary("Open", -Infinity), to: $Boundary("Open", v.differenceType._2._1) });
          }
          if (v.differenceType._1.tag === "Just" && v.differenceType._2._1 > v.differenceType._1._1) {
            return $Maybe("Just", { from: $Boundary("Open", v.differenceType._1._1), to: $Boundary("Closed", v.differenceType._2._1) });
          }
        }
        return acc;
      }
      if (v.differenceType.tag === "MinimumChange" && v.differenceType._2.tag === "Just") {
        if (v.differenceType._1.tag === "Nothing") {
          return $Maybe("Just", { from: $Boundary("Open", -Infinity), to: $Boundary("Open", v.differenceType._2._1) });
        }
        if (v.differenceType._1.tag === "Just" && v.differenceType._2._1 > v.differenceType._1._1) {
          return $Maybe("Just", { from: $Boundary("Closed", v.differenceType._1._1), to: $Boundary("Open", v.differenceType._2._1) });
        }
      }
      return acc;
    })(Nothing)(differences);
    const mbExtensionUpperRange = foldableSet.foldl((acc) => (v) => {
      if (v.differenceType.tag === "ExclusiveMaximumChange") {
        if (v.differenceType._1.tag === "Just") {
          if (v.differenceType._2.tag === "Nothing") {
            return $Maybe("Just", { from: $Boundary("Closed", v.differenceType._1._1), to: $Boundary("Open", Infinity) });
          }
          if (v.differenceType._2.tag === "Just" && v.differenceType._2._1 > v.differenceType._1._1) {
            return $Maybe("Just", { from: $Boundary("Closed", v.differenceType._1._1), to: $Boundary("Open", v.differenceType._2._1) });
          }
        }
        return acc;
      }
      if (v.differenceType.tag === "MaximumChange" && v.differenceType._1.tag === "Just") {
        if (v.differenceType._2.tag === "Nothing") {
          return $Maybe("Just", { from: $Boundary("Open", v.differenceType._1._1), to: $Boundary("Open", Infinity) });
        }
        if (v.differenceType._2.tag === "Just" && v.differenceType._2._1 > v.differenceType._1._1) {
          return $Maybe("Just", { from: $Boundary("Open", v.differenceType._1._1), to: $Boundary("Closed", v.differenceType._2._1) });
        }
      }
      return acc;
    })(Nothing)(differences);
    const mbExtensionLowerRange = foldableSet.foldl((acc) => (v) => {
      if (v.differenceType.tag === "ExclusiveMinimumChange") {
        if (v.differenceType._1.tag === "Just") {
          if (v.differenceType._2.tag === "Nothing") {
            return $Maybe("Just", { from: $Boundary("Open", -Infinity), to: $Boundary("Closed", v.differenceType._1._1) });
          }
          if (v.differenceType._2.tag === "Just" && v.differenceType._2._1 < v.differenceType._1._1) {
            return $Maybe("Just", { from: $Boundary("Open", v.differenceType._2._1), to: $Boundary("Closed", v.differenceType._1._1) });
          }
        }
        return acc;
      }
      if (v.differenceType.tag === "MinimumChange" && v.differenceType._1.tag === "Just") {
        if (v.differenceType._2.tag === "Nothing") {
          return $Maybe("Just", { from: $Boundary("Open", -Infinity), to: $Boundary("Closed", v.differenceType._1._1) });
        }
        if (v.differenceType._2.tag === "Just" && v.differenceType._2._1 < v.differenceType._1._1) {
          return $Maybe("Just", { from: $Boundary("Closed", v.differenceType._2._1), to: $Boundary("Open", v.differenceType._1._1) });
        }
      }
      return acc;
    })(Nothing)(differences);
    return mergeCompatibility((() => {
      if (mbExtensionUpperRange.tag === "Nothing") {
        if (mbExtensionLowerRange.tag === "Nothing") {
          return Full;
        }
        if (mbExtensionLowerRange.tag === "Just") {
          return $Compatibility(
            "Backward",
            {
              forwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                {
                  incompatibilityType: $ForwardIncompatibilityType("RangeOfAllowedNumbersExtended", $NumberRangeChange("Lower", mbExtensionLowerRange._1)),
                  path: Nil
                },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
        fail();
      }
      if (mbExtensionUpperRange.tag === "Just") {
        if (mbExtensionLowerRange.tag === "Just") {
          return $Compatibility(
            "Backward",
            {
              forwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                {
                  incompatibilityType: $ForwardIncompatibilityType(
                    "RangeOfAllowedNumbersExtended",
                    $NumberRangeChange("LowerAndUpper", mbExtensionLowerRange._1, mbExtensionUpperRange._1)
                  ),
                  path: Nil
                },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
        if (mbExtensionLowerRange.tag === "Nothing") {
          return $Compatibility(
            "Backward",
            {
              forwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                {
                  incompatibilityType: $ForwardIncompatibilityType("RangeOfAllowedNumbersExtended", $NumberRangeChange("Upper", mbExtensionUpperRange._1)),
                  path: Nil
                },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
      }
      fail();
    })())((() => {
      if (mbReductionUpperRange.tag === "Nothing") {
        if (mbReductionLowerRange.tag === "Nothing") {
          return Full;
        }
        if (mbReductionLowerRange.tag === "Just") {
          return $Compatibility(
            "Forward",
            {
              backwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                {
                  incompatibilityType: $BackwardIncompatibilityType("RangeOfAllowedNumbersReduced", $NumberRangeChange("Lower", mbReductionLowerRange._1)),
                  path: Nil
                },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
        fail();
      }
      if (mbReductionUpperRange.tag === "Just") {
        if (mbReductionLowerRange.tag === "Just") {
          return $Compatibility(
            "Forward",
            {
              backwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                {
                  incompatibilityType: $BackwardIncompatibilityType(
                    "RangeOfAllowedNumbersReduced",
                    $NumberRangeChange("LowerAndUpper", mbReductionLowerRange._1, mbReductionUpperRange._1)
                  ),
                  path: Nil
                },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
        if (mbReductionLowerRange.tag === "Nothing") {
          return $Compatibility(
            "Forward",
            {
              backwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                {
                  incompatibilityType: $BackwardIncompatibilityType("RangeOfAllowedNumbersReduced", $NumberRangeChange("Upper", mbReductionUpperRange._1)),
                  path: Nil
                },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
      }
      fail();
    })());
  };
  var calculateMultipleOfChange = (v) => (v1) => {
    if (v.tag === "Just") {
      if (v1.tag === "Just") {
        if ((() => {
          const $0 = v._1 / v1._1;
          return toNumber(unsafeClamp(trunc($0))) === $0;
        })()) {
          return $Compatibility(
            "Forward",
            {
              backwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                { incompatibilityType: $BackwardIncompatibilityType("OldMultipleIsNotFactorOfNewMultiple", { new: v1._1, old: v._1 }), path: Nil },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
        if ((() => {
          const $0 = v1._1 / v._1;
          return toNumber(unsafeClamp(trunc($0))) === $0;
        })()) {
          return $Compatibility(
            "Backward",
            {
              forwardIncompatibilities: $$$Map(
                "Node",
                1,
                1,
                { incompatibilityType: $ForwardIncompatibilityType("NewMultipleIsNotFactorOfOldMultiple", { new: v1._1, old: v._1 }), path: Nil },
                void 0,
                Leaf2,
                Leaf2
              )
            }
          );
        }
        return $Compatibility(
          "None",
          {
            backwardIncompatibilities: $$$Map(
              "Node",
              1,
              1,
              { incompatibilityType: $BackwardIncompatibilityType("OldMultipleIsNotFactorOfNewMultiple", { new: v1._1, old: v._1 }), path: Nil },
              void 0,
              Leaf2,
              Leaf2
            ),
            forwardIncompatibilities: $$$Map(
              "Node",
              1,
              1,
              { incompatibilityType: $ForwardIncompatibilityType("NewMultipleIsNotFactorOfOldMultiple", { new: v1._1, old: v._1 }), path: Nil },
              void 0,
              Leaf2,
              Leaf2
            )
          }
        );
      }
      if (v1.tag === "Nothing") {
        return $Compatibility(
          "Backward",
          {
            forwardIncompatibilities: $$$Map(
              "Node",
              1,
              1,
              { incompatibilityType: $ForwardIncompatibilityType("MultipleWithdrawn", v._1), path: Nil },
              void 0,
              Leaf2,
              Leaf2
            )
          }
        );
      }
      fail();
    }
    if (v.tag === "Nothing") {
      if (v1.tag === "Just") {
        return $Compatibility(
          "Forward",
          {
            backwardIncompatibilities: $$$Map(
              "Node",
              1,
              1,
              { incompatibilityType: $BackwardIncompatibilityType("MultipleIntroduced", v1._1), path: Nil },
              void 0,
              Leaf2,
              Leaf2
            )
          }
        );
      }
      if (v1.tag === "Nothing") {
        return Full;
      }
    }
    fail();
  };
  var calculate = (differences) => foldableSet.foldl((acc) => (v) => mergeCompatibility(acc)((() => {
    if (v.differenceType.tag === "MultipleOfChange") {
      return calculateMultipleOfChange(v.differenceType._1)(v.differenceType._2);
    }
    if (v.differenceType.tag === "TypeChange") {
      return calculateTypeChange(v.differenceType._1)(v.differenceType._2);
    }
    return Full;
  })()))(calculateRangeChange(differences))(differences);

  // output-es/JsonSchema.Difference/index.js
  var $DifferenceType = (tag, _1, _2) => ({ tag, _1, _2 });
  var exclusiveMaximumIsSymbol = { reflectSymbol: () => "exclusiveMaximum" };
  var exclusiveMinimumIsSymbol = { reflectSymbol: () => "exclusiveMinimum" };
  var itemsIsSymbol = { reflectSymbol: () => "items" };
  var maximumIsSymbol = { reflectSymbol: () => "maximum" };
  var minimumIsSymbol = { reflectSymbol: () => "minimum" };
  var multipleOfIsSymbol = { reflectSymbol: () => "multipleOf" };
  var notIsSymbol = { reflectSymbol: () => "not" };
  var requiredIsSymbol = { reflectSymbol: () => "required" };
  var typeKeywordIsSymbol = { reflectSymbol: () => "typeKeyword" };
  var uniqueItemsIsSymbol = { reflectSymbol: () => "uniqueItems" };
  var differenceTypeIsSymbol = { reflectSymbol: () => "differenceType" };
  var pathIsSymbol2 = { reflectSymbol: () => "path" };
  var eq2 = (x) => (y) => {
    if (x.tag === "Nothing") {
      return y.tag === "Nothing";
    }
    return x.tag === "Just" && y.tag === "Just" && eqJsonSchema.eq(x._1)(y._1);
  };
  var eq4 = (x) => (y) => {
    if (x.tag === "Nothing") {
      return y.tag === "Nothing";
    }
    return x.tag === "Just" && y.tag === "Just" && eqMap(eqJsonValueType)(eqUnit).eq(x._1)(y._1);
  };
  var compare12 = /* @__PURE__ */ (() => ordMaybe(ordNumber).compare)();
  var compare23 = /* @__PURE__ */ (() => ordMaybe(ordJsonSchema).compare)();
  var compare32 = /* @__PURE__ */ (() => ordSet(ordString).compare)();
  var compare4 = /* @__PURE__ */ (() => ordMaybe(ordSet(ordJsonValueType)).compare)();
  var eq5 = (xs) => (ys) => {
    const go = (v) => (v1) => (v2) => {
      if (!v2) {
        return false;
      }
      if (v.tag === "Nil") {
        return v1.tag === "Nil" && v2;
      }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)((() => {
        if (v1._1.tag === "ExclusiveMaximum") {
          return v2 && v._1.tag === "ExclusiveMaximum";
        }
        if (v1._1.tag === "ExclusiveMinimum") {
          return v2 && v._1.tag === "ExclusiveMinimum";
        }
        if (v1._1.tag === "Items") {
          return v2 && v._1.tag === "Items";
        }
        if (v1._1.tag === "Maximum") {
          return v2 && v._1.tag === "Maximum";
        }
        if (v1._1.tag === "Minimum") {
          return v2 && v._1.tag === "Minimum";
        }
        if (v1._1.tag === "MultipleOf") {
          return v2 && v._1.tag === "MultipleOf";
        }
        if (v1._1.tag === "Properties") {
          return v2 && v._1.tag === "Properties" && v1._1._1 === v._1._1;
        }
        if (v1._1.tag === "TypeKeyword") {
          return v2 && v._1.tag === "TypeKeyword";
        }
        return v2 && v1._1.tag === "UniqueItems" && v._1.tag === "UniqueItems";
      })());
    };
    return go(xs)(ys)(true);
  };
  var compare5 = /* @__PURE__ */ (() => ordList(ordSchemaPathSegment).compare)();
  var encodeJsonMaybe = /* @__PURE__ */ (() => ({ encodeJson: encodeMaybe(encodeSet(ordJsonValueType)(encodeJsonJsonValueType).encodeJson) }))();
  var encodeJson3 = /* @__PURE__ */ (() => {
    const $0 = gEncodeJsonCons({ encodeJson: encodeMaybe(id) })(gEncodeJsonCons({
      encodeJson: encodeMaybe(id)
    })(gEncodeJsonCons({ encodeJson: encodeMaybe(encodeJsonJsonSchema.encodeJson) })(gEncodeJsonCons({
      encodeJson: encodeMaybe(id)
    })(gEncodeJsonCons({ encodeJson: encodeMaybe(id) })(gEncodeJsonCons({
      encodeJson: encodeMaybe(id)
    })(gEncodeJsonCons({ encodeJson: encodeMaybe(encodeJsonJsonSchema.encodeJson) })(gEncodeJsonCons(encodeSet(ordString)(encodeJsonJString))(gEncodeJsonCons(encodeJsonMaybe)(gEncodeJsonCons(encodeJsonJBoolean)(gEncodeJsonNil)(uniqueItemsIsSymbol)())(typeKeywordIsSymbol)())(requiredIsSymbol)())(notIsSymbol)())(multipleOfIsSymbol)())(minimumIsSymbol)())(maximumIsSymbol)())(itemsIsSymbol)())(exclusiveMinimumIsSymbol)())(exclusiveMaximumIsSymbol)();
    return (rec) => id($0.gEncodeJson(rec)($$Proxy));
  })();
  var unorderedList4 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var fromFoldable7 = ($0) => fromFoldableImpl((f) => (b) => (v) => f(v._1)(foldrArray(f)(b)(v._2)), $0);
  var eqDifferenceType = {
    eq: (x) => (y) => {
      if (x.tag === "BooleanSchemaChange") {
        return y.tag === "BooleanSchemaChange" && x._1 === y._1;
      }
      if (x.tag === "ExclusiveMaximumChange") {
        return y.tag === "ExclusiveMaximumChange" && (x._1.tag === "Nothing" ? y._1.tag === "Nothing" : x._1.tag === "Just" && y._1.tag === "Just" && x._1._1 === y._1._1) && (x._2.tag === "Nothing" ? y._2.tag === "Nothing" : x._2.tag === "Just" && y._2.tag === "Just" && x._2._1 === y._2._1);
      }
      if (x.tag === "ExclusiveMinimumChange") {
        return y.tag === "ExclusiveMinimumChange" && (x._1.tag === "Nothing" ? y._1.tag === "Nothing" : x._1.tag === "Just" && y._1.tag === "Just" && x._1._1 === y._1._1) && (x._2.tag === "Nothing" ? y._2.tag === "Nothing" : x._2.tag === "Just" && y._2.tag === "Just" && x._2._1 === y._2._1);
      }
      if (x.tag === "MaximumChange") {
        return y.tag === "MaximumChange" && (x._1.tag === "Nothing" ? y._1.tag === "Nothing" : x._1.tag === "Just" && y._1.tag === "Just" && x._1._1 === y._1._1) && (x._2.tag === "Nothing" ? y._2.tag === "Nothing" : x._2.tag === "Just" && y._2.tag === "Just" && x._2._1 === y._2._1);
      }
      if (x.tag === "MinimumChange") {
        return y.tag === "MinimumChange" && (x._1.tag === "Nothing" ? y._1.tag === "Nothing" : x._1.tag === "Just" && y._1.tag === "Just" && x._1._1 === y._1._1) && (x._2.tag === "Nothing" ? y._2.tag === "Nothing" : x._2.tag === "Just" && y._2.tag === "Just" && x._2._1 === y._2._1);
      }
      if (x.tag === "MultipleOfChange") {
        return y.tag === "MultipleOfChange" && (x._1.tag === "Nothing" ? y._1.tag === "Nothing" : x._1.tag === "Just" && y._1.tag === "Just" && x._1._1 === y._1._1) && (x._2.tag === "Nothing" ? y._2.tag === "Nothing" : x._2.tag === "Just" && y._2.tag === "Just" && x._2._1 === y._2._1);
      }
      if (x.tag === "SchemaChangeFromBooleanToObject") {
        return y.tag === "SchemaChangeFromBooleanToObject" && x._1 === y._1 && (x._2.exclusiveMaximum.tag === "Nothing" ? y._2.exclusiveMaximum.tag === "Nothing" : x._2.exclusiveMaximum.tag === "Just" && y._2.exclusiveMaximum.tag === "Just" && x._2.exclusiveMaximum._1 === y._2.exclusiveMaximum._1) && (x._2.exclusiveMinimum.tag === "Nothing" ? y._2.exclusiveMinimum.tag === "Nothing" : x._2.exclusiveMinimum.tag === "Just" && y._2.exclusiveMinimum.tag === "Just" && x._2.exclusiveMinimum._1 === y._2.exclusiveMinimum._1) && eq2(x._2.items)(y._2.items) && (x._2.maximum.tag === "Nothing" ? y._2.maximum.tag === "Nothing" : x._2.maximum.tag === "Just" && y._2.maximum.tag === "Just" && x._2.maximum._1 === y._2.maximum._1) && (x._2.minimum.tag === "Nothing" ? y._2.minimum.tag === "Nothing" : x._2.minimum.tag === "Just" && y._2.minimum.tag === "Just" && x._2.minimum._1 === y._2.minimum._1) && (x._2.multipleOf.tag === "Nothing" ? y._2.multipleOf.tag === "Nothing" : x._2.multipleOf.tag === "Just" && y._2.multipleOf.tag === "Just" && x._2.multipleOf._1 === y._2.multipleOf._1) && eq2(x._2.not)(y._2.not) && eqMap(eqString)(eqUnit).eq(x._2.required)(y._2.required) && eq4(x._2.typeKeyword)(y._2.typeKeyword) && x._2.uniqueItems === y._2.uniqueItems;
      }
      if (x.tag === "SchemaChangeFromObjectToBoolean") {
        return y.tag === "SchemaChangeFromObjectToBoolean" && (x._1.exclusiveMaximum.tag === "Nothing" ? y._1.exclusiveMaximum.tag === "Nothing" : x._1.exclusiveMaximum.tag === "Just" && y._1.exclusiveMaximum.tag === "Just" && x._1.exclusiveMaximum._1 === y._1.exclusiveMaximum._1) && (x._1.exclusiveMinimum.tag === "Nothing" ? y._1.exclusiveMinimum.tag === "Nothing" : x._1.exclusiveMinimum.tag === "Just" && y._1.exclusiveMinimum.tag === "Just" && x._1.exclusiveMinimum._1 === y._1.exclusiveMinimum._1) && eq2(x._1.items)(y._1.items) && (x._1.maximum.tag === "Nothing" ? y._1.maximum.tag === "Nothing" : x._1.maximum.tag === "Just" && y._1.maximum.tag === "Just" && x._1.maximum._1 === y._1.maximum._1) && (x._1.minimum.tag === "Nothing" ? y._1.minimum.tag === "Nothing" : x._1.minimum.tag === "Just" && y._1.minimum.tag === "Just" && x._1.minimum._1 === y._1.minimum._1) && (x._1.multipleOf.tag === "Nothing" ? y._1.multipleOf.tag === "Nothing" : x._1.multipleOf.tag === "Just" && y._1.multipleOf.tag === "Just" && x._1.multipleOf._1 === y._1.multipleOf._1) && eq2(x._1.not)(y._1.not) && eqMap(eqString)(eqUnit).eq(x._1.required)(y._1.required) && eq4(x._1.typeKeyword)(y._1.typeKeyword) && x._1.uniqueItems === y._1.uniqueItems && x._2 === y._2;
      }
      return x.tag === "TypeChange" && y.tag === "TypeChange" && eq4(x._1)(y._1) && eq4(x._2)(y._2);
    }
  };
  var ordDifferenceType = {
    compare: (x) => (y) => {
      if (x.tag === "BooleanSchemaChange") {
        if (y.tag === "BooleanSchemaChange") {
          return ordBoolean.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "BooleanSchemaChange") {
        return GT;
      }
      if (x.tag === "ExclusiveMaximumChange") {
        if (y.tag === "ExclusiveMaximumChange") {
          const v = compare12(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return compare12(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "ExclusiveMaximumChange") {
        return GT;
      }
      if (x.tag === "ExclusiveMinimumChange") {
        if (y.tag === "ExclusiveMinimumChange") {
          const v = compare12(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return compare12(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "ExclusiveMinimumChange") {
        return GT;
      }
      if (x.tag === "MaximumChange") {
        if (y.tag === "MaximumChange") {
          const v = compare12(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return compare12(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "MaximumChange") {
        return GT;
      }
      if (x.tag === "MinimumChange") {
        if (y.tag === "MinimumChange") {
          const v = compare12(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return compare12(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "MinimumChange") {
        return GT;
      }
      if (x.tag === "MultipleOfChange") {
        if (y.tag === "MultipleOfChange") {
          const v = compare12(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return compare12(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "MultipleOfChange") {
        return GT;
      }
      if (x.tag === "SchemaChangeFromBooleanToObject") {
        if (y.tag === "SchemaChangeFromBooleanToObject") {
          const v = ordBoolean.compare(x._1)(y._1);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          const v1 = compare12(x._2.exclusiveMaximum)(y._2.exclusiveMaximum);
          if (v1 === "LT") {
            return LT;
          }
          if (v1 === "GT") {
            return GT;
          }
          const v2 = compare12(x._2.exclusiveMinimum)(y._2.exclusiveMinimum);
          if (v2 === "LT") {
            return LT;
          }
          if (v2 === "GT") {
            return GT;
          }
          const v3 = compare23(x._2.items)(y._2.items);
          if (v3 === "LT") {
            return LT;
          }
          if (v3 === "GT") {
            return GT;
          }
          const v4 = compare12(x._2.maximum)(y._2.maximum);
          if (v4 === "LT") {
            return LT;
          }
          if (v4 === "GT") {
            return GT;
          }
          const v5 = compare12(x._2.minimum)(y._2.minimum);
          if (v5 === "LT") {
            return LT;
          }
          if (v5 === "GT") {
            return GT;
          }
          const v6 = compare12(x._2.multipleOf)(y._2.multipleOf);
          if (v6 === "LT") {
            return LT;
          }
          if (v6 === "GT") {
            return GT;
          }
          const v7 = compare23(x._2.not)(y._2.not);
          if (v7 === "LT") {
            return LT;
          }
          if (v7 === "GT") {
            return GT;
          }
          const v8 = compare32(x._2.required)(y._2.required);
          if (v8 === "LT") {
            return LT;
          }
          if (v8 === "GT") {
            return GT;
          }
          const v9 = compare4(x._2.typeKeyword)(y._2.typeKeyword);
          if (v9 === "LT") {
            return LT;
          }
          if (v9 === "GT") {
            return GT;
          }
          return ordBoolean.compare(x._2.uniqueItems)(y._2.uniqueItems);
        }
        return LT;
      }
      if (y.tag === "SchemaChangeFromBooleanToObject") {
        return GT;
      }
      if (x.tag === "SchemaChangeFromObjectToBoolean") {
        if (y.tag === "SchemaChangeFromObjectToBoolean") {
          const v1 = compare12(x._1.exclusiveMaximum)(y._1.exclusiveMaximum);
          if (v1 === "LT") {
            return LT;
          }
          if (v1 === "GT") {
            return GT;
          }
          const v2 = compare12(x._1.exclusiveMinimum)(y._1.exclusiveMinimum);
          if (v2 === "LT") {
            return LT;
          }
          if (v2 === "GT") {
            return GT;
          }
          const v3 = compare23(x._1.items)(y._1.items);
          if (v3 === "LT") {
            return LT;
          }
          if (v3 === "GT") {
            return GT;
          }
          const v4 = compare12(x._1.maximum)(y._1.maximum);
          if (v4 === "LT") {
            return LT;
          }
          if (v4 === "GT") {
            return GT;
          }
          const v5 = compare12(x._1.minimum)(y._1.minimum);
          if (v5 === "LT") {
            return LT;
          }
          if (v5 === "GT") {
            return GT;
          }
          const v6 = compare12(x._1.multipleOf)(y._1.multipleOf);
          if (v6 === "LT") {
            return LT;
          }
          if (v6 === "GT") {
            return GT;
          }
          const v7 = compare23(x._1.not)(y._1.not);
          if (v7 === "LT") {
            return LT;
          }
          if (v7 === "GT") {
            return GT;
          }
          const v8 = compare32(x._1.required)(y._1.required);
          if (v8 === "LT") {
            return LT;
          }
          if (v8 === "GT") {
            return GT;
          }
          const v9 = compare4(x._1.typeKeyword)(y._1.typeKeyword);
          if (v9 === "LT") {
            return LT;
          }
          if (v9 === "GT") {
            return GT;
          }
          if (x._1.uniqueItems < y._1.uniqueItems) {
            return LT;
          }
          if (x._1.uniqueItems > y._1.uniqueItems) {
            return GT;
          }
          return ordBoolean.compare(x._2)(y._2);
        }
        return LT;
      }
      if (y.tag === "SchemaChangeFromObjectToBoolean") {
        return GT;
      }
      if (x.tag === "TypeChange" && y.tag === "TypeChange") {
        const v = compare4(x._1)(y._1);
        if (v === "LT") {
          return LT;
        }
        if (v === "GT") {
          return GT;
        }
        return compare4(x._2)(y._2);
      }
      fail();
    },
    Eq0: () => eqDifferenceType
  };
  var eqDifference = { eq: (x) => (y) => eqDifferenceType.eq(x.differenceType)(y.differenceType) && eq5(x.path)(y.path) };
  var ordDifference = {
    compare: (x) => (y) => {
      const v = ordDifferenceType.compare(x.differenceType)(y.differenceType);
      if (v === "LT") {
        return LT;
      }
      if (v === "GT") {
        return GT;
      }
      return compare5(x.path)(y.path);
    },
    Eq0: () => eqDifference
  };
  var foldMap = /* @__PURE__ */ (() => foldableArray.foldMap(monoidSet(ordDifference)))();
  var encodeJsonDifferenceType = {
    encodeJson: (v) => {
      if (v.tag === "BooleanSchemaChange") {
        return id(v._1 ? "boolean schema changed to true" : "boolean schema changed to false");
      }
      if (v.tag === "ExclusiveMaximumChange") {
        return id((() => {
          if (v._1.tag === "Nothing") {
            return "exclusive maximum value change from not set to ";
          }
          if (v._1.tag === "Just") {
            return "exclusive maximum value change from " + showNumberImpl(v._1._1) + " to ";
          }
          fail();
        })() + (() => {
          if (v._2.tag === "Nothing") {
            return "not set";
          }
          if (v._2.tag === "Just") {
            return showNumberImpl(v._2._1);
          }
          fail();
        })());
      }
      if (v.tag === "ExclusiveMinimumChange") {
        return id((() => {
          if (v._1.tag === "Nothing") {
            return "exclusive minimum value change from not set to ";
          }
          if (v._1.tag === "Just") {
            return "exclusive minimum value change from " + showNumberImpl(v._1._1) + " to ";
          }
          fail();
        })() + (() => {
          if (v._2.tag === "Nothing") {
            return "not set";
          }
          if (v._2.tag === "Just") {
            return showNumberImpl(v._2._1);
          }
          fail();
        })());
      }
      if (v.tag === "MaximumChange") {
        return id((() => {
          if (v._1.tag === "Nothing") {
            return "maximum value change from not set to ";
          }
          if (v._1.tag === "Just") {
            return "maximum value change from " + showNumberImpl(v._1._1) + " to ";
          }
          fail();
        })() + (() => {
          if (v._2.tag === "Nothing") {
            return "not set";
          }
          if (v._2.tag === "Just") {
            return showNumberImpl(v._2._1);
          }
          fail();
        })());
      }
      if (v.tag === "MinimumChange") {
        return id((() => {
          if (v._1.tag === "Nothing") {
            return "minimum value change from not set to ";
          }
          if (v._1.tag === "Just") {
            return "minimum value change from " + showNumberImpl(v._1._1) + " to ";
          }
          fail();
        })() + (() => {
          if (v._2.tag === "Nothing") {
            return "not set";
          }
          if (v._2.tag === "Just") {
            return showNumberImpl(v._2._1);
          }
          fail();
        })());
      }
      if (v.tag === "MultipleOfChange") {
        return id((() => {
          if (v._1.tag === "Nothing") {
            return "multiple of change from not set to ";
          }
          if (v._1.tag === "Just") {
            return "multiple of change from " + showNumberImpl(v._1._1) + " to ";
          }
          fail();
        })() + (() => {
          if (v._2.tag === "Nothing") {
            return "not set";
          }
          if (v._2.tag === "Just") {
            return showNumberImpl(v._2._1);
          }
          fail();
        })());
      }
      if (v.tag === "SchemaChangeFromBooleanToObject") {
        return extend(encodeJsonJson.encodeJson)($Tuple("newObjectSchema", encodeJson3(v._2)))(extend(encodeJsonJson.encodeJson)($Tuple(
          "oldBooleanSchema",
          id(v._1)
        ))(jsonEmptyObject));
      }
      if (v.tag === "SchemaChangeFromObjectToBoolean") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "newSchema",
          id(v._2 ? "boolean schema of true" : "boolean schema of false")
        ))(extend(encodeJsonJson.encodeJson)($Tuple("oldObjectSchema", encodeJson3(v._1)))(jsonEmptyObject));
      }
      if (v.tag === "TypeChange") {
        return extend(encodeJsonJson.encodeJson)((() => {
          const $0 = encodeSet(ordJsonValueType)(encodeJsonJsonValueType).encodeJson;
          return $Tuple(
            "newAcceptableTypes",
            (() => {
              if (v._2.tag === "Nothing") {
                return jsonNull;
              }
              if (v._2.tag === "Just") {
                return $0(v._2._1);
              }
              fail();
            })()
          );
        })())(extend(encodeJsonJson.encodeJson)((() => {
          const $0 = encodeSet(ordJsonValueType)(encodeJsonJsonValueType).encodeJson;
          return $Tuple(
            "oldAcceptableTypes",
            (() => {
              if (v._1.tag === "Nothing") {
                return jsonNull;
              }
              if (v._1.tag === "Just") {
                return $0(v._1._1);
              }
              fail();
            })()
          );
        })())(jsonEmptyObject));
      }
      fail();
    }
  };
  var encodeJsonDifference = /* @__PURE__ */ (() => {
    const $0 = gEncodeJsonCons(encodeJsonDifferenceType)(gEncodeJsonCons({
      encodeJson: encodeList(encodeJsonSchemaPathSegme.encodeJson)
    })(gEncodeJsonNil)(pathIsSymbol2)())(differenceTypeIsSymbol)();
    return { encodeJson: (rec) => id($0.gEncodeJson(rec)($$Proxy)) };
  })();
  var renderOptionalNumber = (v2) => {
    if (v2.tag === "Nothing") {
      return "unspecified";
    }
    if (v2.tag === "Just") {
      const $0 = showNumberImpl(v2._1);
      if ($0 === "") {
        return "impossible";
      }
      return $0;
    }
    fail();
  };
  var renderJsonValueType2 = (x) => $FlowContentNode(
    "Paragraph",
    fromFoldableImpl(
      foldrArray,
      [
        $PhrasingContentNode(
          "Text",
          (() => {
            if (x === "JsonArray") {
              return "array";
            }
            if (x === "JsonBoolean") {
              return "boolean";
            }
            if (x === "JsonInteger") {
              return "integer";
            }
            if (x === "JsonNull") {
              return "null";
            }
            if (x === "JsonNumber") {
              return "number";
            }
            if (x === "JsonObject") {
              return "object";
            }
            if (x === "JsonString") {
              return "string";
            }
            fail();
          })()
        )
      ]
    )
  );
  var renderJsonValueTypes = /* @__PURE__ */ (() => {
    const $0 = $FlowContentNode("Paragraph", fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "unspecified")]));
    const $1 = fromFoldable5(foldableSet);
    return (v2) => {
      if (v2.tag === "Nothing") {
        return $0;
      }
      if (v2.tag === "Just") {
        const $2 = $1(v2._1);
        if ($2.tag === "Just") {
          return unorderedList4(arrayMap((x) => [renderJsonValueType2(x)])($2._1));
        }
        if ($2.tag === "Nothing") {
          return $FlowContentNode("Paragraph", fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "none")]));
        }
      }
      fail();
    };
  })();
  var documentDifferenceType = {
    document: (v) => {
      if (v.tag === "BooleanSchemaChange") {
        if (!v._1) {
          return $NonEmpty(
            $FlowContentNode(
              "Paragraph",
              fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "change of boolean schema from allow-all to reject-all")])
            ),
            []
          );
        }
        if (v._1) {
          return $NonEmpty(
            $FlowContentNode(
              "Paragraph",
              fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "change of boolean schema from reject-all to allow-all")])
            ),
            []
          );
        }
        fail();
      }
      if (v.tag === "ExclusiveMaximumChange") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [$PhrasingContentNode("Text", "change of exclusiveMaximum from " + renderOptionalNumber(v._1) + " to " + renderOptionalNumber(v._2))]
            )
          ),
          []
        );
      }
      if (v.tag === "ExclusiveMinimumChange") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [$PhrasingContentNode("Text", "change of exclusiveMinimum from " + renderOptionalNumber(v._1) + " to " + renderOptionalNumber(v._2))]
            )
          ),
          []
        );
      }
      if (v.tag === "MaximumChange") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [$PhrasingContentNode("Text", "change of maximum from " + renderOptionalNumber(v._1) + " to " + renderOptionalNumber(v._2))]
            )
          ),
          []
        );
      }
      if (v.tag === "MinimumChange") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [$PhrasingContentNode("Text", "change of minimum from " + renderOptionalNumber(v._1) + " to " + renderOptionalNumber(v._2))]
            )
          ),
          []
        );
      }
      if (v.tag === "MultipleOfChange") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [$PhrasingContentNode("Text", "change of multipleOf from " + renderOptionalNumber(v._1) + " to " + renderOptionalNumber(v._2))]
            )
          ),
          []
        );
      }
      if (v.tag === "SchemaChangeFromBooleanToObject") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "change of boolean schema to object schema")])
          ),
          []
        );
      }
      if (v.tag === "SchemaChangeFromObjectToBoolean") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "change of object schema to boolean schema")])
          ),
          []
        );
      }
      if (v.tag === "TypeChange") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "change of accepted JSON value types from")])
          ),
          [
            renderJsonValueTypes(v._1),
            $FlowContentNode("Paragraph", fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "to")])),
            renderJsonValueTypes(v._2)
          ]
        );
      }
      fail();
    }
  };
  var documentDifference = {
    document: (v) => $NonEmpty(
      $FlowContentNode(
        "Paragraph",
        fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "JSON schema path: " + render3(v.path))])
      ),
      fromFoldable7(documentDifferenceType.document(v.differenceType))
    )
  };
  var calculateTypeKeywordDiff = (path) => (previousKeywords) => (nextKeywords) => {
    if (eq4(previousKeywords.typeKeyword)(nextKeywords.typeKeyword)) {
      return Leaf2;
    }
    return $$$Map(
      "Node",
      1,
      1,
      {
        differenceType: $DifferenceType("TypeChange", previousKeywords.typeKeyword, nextKeywords.typeKeyword),
        path: $List("Cons", TypeKeyword, path)
      },
      void 0,
      Leaf2,
      Leaf2
    );
  };
  var calculateRangeDiff = (path) => (previousKeywords) => (nextKeywords) => unsafeUnionWith(
    ordDifference.compare,
    $$const,
    (previousKeywords.exclusiveMaximum.tag === "Nothing" ? nextKeywords.exclusiveMaximum.tag === "Nothing" : previousKeywords.exclusiveMaximum.tag === "Just" && nextKeywords.exclusiveMaximum.tag === "Just" && previousKeywords.exclusiveMaximum._1 === nextKeywords.exclusiveMaximum._1) ? Leaf2 : $$$Map(
      "Node",
      1,
      1,
      {
        differenceType: $DifferenceType("ExclusiveMaximumChange", previousKeywords.exclusiveMaximum, nextKeywords.exclusiveMaximum),
        path: $List("Cons", ExclusiveMaximum, path)
      },
      void 0,
      Leaf2,
      Leaf2
    ),
    unsafeUnionWith(
      ordDifference.compare,
      $$const,
      (previousKeywords.exclusiveMinimum.tag === "Nothing" ? nextKeywords.exclusiveMinimum.tag === "Nothing" : previousKeywords.exclusiveMinimum.tag === "Just" && nextKeywords.exclusiveMinimum.tag === "Just" && previousKeywords.exclusiveMinimum._1 === nextKeywords.exclusiveMinimum._1) ? Leaf2 : $$$Map(
        "Node",
        1,
        1,
        {
          differenceType: $DifferenceType("ExclusiveMinimumChange", previousKeywords.exclusiveMinimum, nextKeywords.exclusiveMinimum),
          path: $List("Cons", ExclusiveMinimum, path)
        },
        void 0,
        Leaf2,
        Leaf2
      ),
      unsafeUnionWith(
        ordDifference.compare,
        $$const,
        (previousKeywords.maximum.tag === "Nothing" ? nextKeywords.maximum.tag === "Nothing" : previousKeywords.maximum.tag === "Just" && nextKeywords.maximum.tag === "Just" && previousKeywords.maximum._1 === nextKeywords.maximum._1) ? Leaf2 : $$$Map(
          "Node",
          1,
          1,
          {
            differenceType: $DifferenceType("MaximumChange", previousKeywords.maximum, nextKeywords.maximum),
            path: $List("Cons", Maximum, path)
          },
          void 0,
          Leaf2,
          Leaf2
        ),
        (previousKeywords.minimum.tag === "Nothing" ? nextKeywords.minimum.tag === "Nothing" : previousKeywords.minimum.tag === "Just" && nextKeywords.minimum.tag === "Just" && previousKeywords.minimum._1 === nextKeywords.minimum._1) ? Leaf2 : $$$Map(
          "Node",
          1,
          1,
          {
            differenceType: $DifferenceType("MinimumChange", previousKeywords.minimum, nextKeywords.minimum),
            path: $List("Cons", Minimum, path)
          },
          void 0,
          Leaf2,
          Leaf2
        )
      )
    )
  );
  var calculateMultipleOfDiff = (path) => (previousKeywords) => (nextKeywords) => {
    if (previousKeywords.multipleOf.tag === "Nothing" ? nextKeywords.multipleOf.tag === "Nothing" : previousKeywords.multipleOf.tag === "Just" && nextKeywords.multipleOf.tag === "Just" && previousKeywords.multipleOf._1 === nextKeywords.multipleOf._1) {
      return Leaf2;
    }
    return $$$Map(
      "Node",
      1,
      1,
      {
        differenceType: $DifferenceType("MultipleOfChange", previousKeywords.multipleOf, nextKeywords.multipleOf),
        path: $List("Cons", MultipleOf, path)
      },
      void 0,
      Leaf2,
      Leaf2
    );
  };
  var calculate2 = (previousSchema) => (nextSchema) => {
    if (previousSchema.tag === "BooleanSchema") {
      if (nextSchema.tag === "BooleanSchema") {
        if (!previousSchema._1) {
          if (nextSchema._1) {
            return insert(ordDifference)({ differenceType: $DifferenceType("BooleanSchemaChange", true), path: Nil })()(Leaf2);
          }
          return Leaf2;
        }
        if (previousSchema._1 && !nextSchema._1) {
          return insert(ordDifference)({ differenceType: $DifferenceType("BooleanSchemaChange", false), path: Nil })()(Leaf2);
        }
        return Leaf2;
      }
      if (nextSchema.tag === "ObjectSchema") {
        return $$$Map(
          "Node",
          1,
          1,
          { differenceType: $DifferenceType("SchemaChangeFromBooleanToObject", previousSchema._1, nextSchema._1), path: Nil },
          void 0,
          Leaf2,
          Leaf2
        );
      }
      fail();
    }
    if (previousSchema.tag === "ObjectSchema") {
      if (nextSchema.tag === "BooleanSchema") {
        return $$$Map(
          "Node",
          1,
          1,
          { differenceType: $DifferenceType("SchemaChangeFromObjectToBoolean", previousSchema._1, nextSchema._1), path: Nil },
          void 0,
          Leaf2,
          Leaf2
        );
      }
      if (nextSchema.tag === "ObjectSchema") {
        const $0 = nextSchema._1;
        const $1 = previousSchema._1;
        return foldMap((f) => f(Nil)($1)($0))([calculateMultipleOfDiff, calculateRangeDiff, calculateTypeKeywordDiff]);
      }
    }
    fail();
  };

  // output-es/CLI.Command.Compat/index.js
  var program = (dictFileAccess) => {
    const Monad0 = dictFileAccess.Monad0();
    const Applicative0 = Monad0.Applicative0();
    const $0 = Monad0.Bind1();
    return (dictMonadError) => {
      const $1 = dictMonadError.MonadThrow0();
      const $2 = $1.Monad0().Applicative0().pure;
      const liftEither = (v2) => {
        if (v2.tag === "Left") {
          return $1.throwError(v2._1);
        }
        if (v2.tag === "Right") {
          return $2(v2._1);
        }
        fail();
      };
      return commandProgram(Applicative0)(documentCompatibility)(encodeJsonCompatibility)(dictMonadError)((v) => {
        const $3 = v.rightSchemaFilePath;
        return $0.bind(dictFileAccess.readFileContent(v.leftSchemaFilePath))((leftSchemaText) => $0.bind(dictFileAccess.readFileContent($3))((rightSchemaText) => $0.bind((() => {
          const $4 = _jsonParser(Left, Right, leftSchemaText);
          return liftEither((() => {
            const $5 = (() => {
              if ($4.tag === "Left") {
                return $Either("Left", $4._1);
              }
              if ($4.tag === "Right") {
                return parseSchema($4._1);
              }
              fail();
            })();
            if ($5.tag === "Left") {
              return $Either("Left", error($5._1));
            }
            if ($5.tag === "Right") {
              return $Either("Right", $5._1);
            }
            fail();
          })());
        })())((leftSchema) => $0.bind((() => {
          const $4 = _jsonParser(Left, Right, rightSchemaText);
          return liftEither((() => {
            const $5 = (() => {
              if ($4.tag === "Left") {
                return $Either("Left", $4._1);
              }
              if ($4.tag === "Right") {
                return parseSchema($4._1);
              }
              fail();
            })();
            if ($5.tag === "Left") {
              return $Either("Left", error($5._1));
            }
            if ($5.tag === "Right") {
              return $Either("Right", $5._1);
            }
            fail();
          })());
        })())((rightSchema) => Applicative0.pure((() => {
          const v1 = calculate(calculate2(leftSchema)(rightSchema));
          if (v1.tag === "Full") {
            return $Either("Right", Full);
          }
          return $Either("Left", v1);
        })())))));
      });
    };
  };

  // output-es/CLI.Command.Diff/index.js
  var documentSet = { document: /* @__PURE__ */ documentFoldable(documentDifference)(foldableSet)(false) };
  var encodeSet2 = /* @__PURE__ */ encodeSet(ordDifference)(encodeJsonDifference);
  var program2 = (dictFileAccess) => {
    const Monad0 = dictFileAccess.Monad0();
    const Applicative0 = Monad0.Applicative0();
    const $0 = Monad0.Bind1();
    return (dictMonadError) => {
      const $1 = dictMonadError.MonadThrow0();
      const $2 = $1.Monad0().Applicative0().pure;
      const liftEither = (v2) => {
        if (v2.tag === "Left") {
          return $1.throwError(v2._1);
        }
        if (v2.tag === "Right") {
          return $2(v2._1);
        }
        fail();
      };
      return commandProgram(Applicative0)(documentSet)(encodeSet2)(dictMonadError)((v) => {
        const $3 = v.rightSchemaFilePath;
        return $0.bind(dictFileAccess.readFileContent(v.leftSchemaFilePath))((leftSchemaText) => $0.bind(dictFileAccess.readFileContent($3))((rightSchemaText) => $0.bind((() => {
          const $4 = _jsonParser(Left, Right, leftSchemaText);
          return liftEither((() => {
            const $5 = (() => {
              if ($4.tag === "Left") {
                return $Either("Left", $4._1);
              }
              if ($4.tag === "Right") {
                return parseSchema($4._1);
              }
              fail();
            })();
            if ($5.tag === "Left") {
              return $Either("Left", error($5._1));
            }
            if ($5.tag === "Right") {
              return $Either("Right", $5._1);
            }
            fail();
          })());
        })())((leftSchema) => $0.bind((() => {
          const $4 = _jsonParser(Left, Right, rightSchemaText);
          return liftEither((() => {
            const $5 = (() => {
              if ($4.tag === "Left") {
                return $Either("Left", $4._1);
              }
              if ($4.tag === "Right") {
                return parseSchema($4._1);
              }
              fail();
            })();
            if ($5.tag === "Left") {
              return $Either("Left", error($5._1));
            }
            if ($5.tag === "Right") {
              return $Either("Right", $5._1);
            }
            fail();
          })());
        })())((rightSchema) => {
          const differences = calculate2(leftSchema)(rightSchema);
          return Applicative0.pure(differences.tag === "Leaf" ? $Either("Right", differences) : $Either("Left", differences));
        }))));
      });
    };
  };

  // output-es/Data.Map/index.js
  var keys3 = /* @__PURE__ */ (() => functorMap.map((v) => {
  }))();

  // output-es/JsonSchema.JsonPath/index.js
  var $JsonPathSegment = (tag, _1) => ({ tag, _1 });
  var eqJsonPathSegment = {
    eq: (x) => (y) => {
      if (x.tag === "ItemIndex") {
        return y.tag === "ItemIndex" && x._1 === y._1;
      }
      return x.tag === "Property" && y.tag === "Property" && x._1 === y._1;
    }
  };
  var ordJsonPathSegment = {
    compare: (x) => (y) => {
      if (x.tag === "ItemIndex") {
        if (y.tag === "ItemIndex") {
          return ordInt.compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "ItemIndex") {
        return GT;
      }
      if (x.tag === "Property" && y.tag === "Property") {
        return ordString.compare(x._1)(y._1);
      }
      fail();
    },
    Eq0: () => eqJsonPathSegment
  };
  var encodeJsonJsonPathSegment = {
    encodeJson: (v) => {
      if (v.tag === "ItemIndex") {
        return id(toNumber(v._1));
      }
      if (v.tag === "Property") {
        return id(v._1);
      }
      fail();
    }
  };
  var render4 = /* @__PURE__ */ (() => {
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const b = go$a0, v = go$a1;
        if (v.tag === "Nil") {
          go$c = false;
          go$r = b;
          continue;
        }
        if (v.tag === "Cons") {
          go$a0 = (() => {
            if (v._1.tag === "ItemIndex") {
              return b + "[" + showIntImpl(v._1._1) + "]";
            }
            if (v._1.tag === "Property") {
              return b + "/" + v._1._1;
            }
            fail();
          })();
          go$a1 = v._2;
          continue;
        }
        fail();
      }
      return go$r;
    };
    const $0 = go("$");
    return (x) => $0(reverse2(x));
  })();

  // output-es/JsonSchema.Validation/index.js
  var $ViolationReason = (tag, _1) => ({ tag, _1 });
  var fromIsSymbol2 = { reflectSymbol: () => "from" };
  var toIsSymbol2 = { reflectSymbol: () => "to" };
  var jsonPathIsSymbol = { reflectSymbol: () => "jsonPath" };
  var reasonIsSymbol = { reflectSymbol: () => "reason" };
  var schemaPathIsSymbol = { reflectSymbol: () => "schemaPath" };
  var eq42 = (xs) => (ys) => {
    const go = (v) => (v1) => (v2) => {
      if (!v2) {
        return false;
      }
      if (v.tag === "Nil") {
        return v1.tag === "Nil" && v2;
      }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)(v1._1.tag === "ItemIndex" ? v2 && v._1.tag === "ItemIndex" && v1._1._1 === v._1._1 : v2 && v1._1.tag === "Property" && v._1.tag === "Property" && v1._1._1 === v._1._1);
    };
    return go(xs)(ys)(true);
  };
  var eq52 = (xs) => (ys) => {
    const go = (v) => (v1) => (v2) => {
      if (!v2) {
        return false;
      }
      if (v.tag === "Nil") {
        return v1.tag === "Nil" && v2;
      }
      return v.tag === "Cons" && v1.tag === "Cons" && go(v._2)(v1._2)((() => {
        if (v1._1.tag === "ExclusiveMaximum") {
          return v2 && v._1.tag === "ExclusiveMaximum";
        }
        if (v1._1.tag === "ExclusiveMinimum") {
          return v2 && v._1.tag === "ExclusiveMinimum";
        }
        if (v1._1.tag === "Items") {
          return v2 && v._1.tag === "Items";
        }
        if (v1._1.tag === "Maximum") {
          return v2 && v._1.tag === "Maximum";
        }
        if (v1._1.tag === "Minimum") {
          return v2 && v._1.tag === "Minimum";
        }
        if (v1._1.tag === "MultipleOf") {
          return v2 && v._1.tag === "MultipleOf";
        }
        if (v1._1.tag === "Properties") {
          return v2 && v._1.tag === "Properties" && v1._1._1 === v._1._1;
        }
        if (v1._1.tag === "TypeKeyword") {
          return v2 && v._1.tag === "TypeKeyword";
        }
        return v2 && v1._1.tag === "UniqueItems" && v._1.tag === "UniqueItems";
      })());
    };
    return go(xs)(ys)(true);
  };
  var compare33 = /* @__PURE__ */ (() => ordSet(ordJsonValueType).compare)();
  var compare42 = /* @__PURE__ */ (() => ordList(ordJsonPathSegment).compare)();
  var compare52 = /* @__PURE__ */ (() => ordList(ordSchemaPathSegment).compare)();
  var encodeJson6 = /* @__PURE__ */ (() => {
    const $0 = gEncodeJsonCons(encodeJsonBoundary)(gEncodeJsonCons(encodeJsonBoundary)(gEncodeJsonNil)(toIsSymbol2)())(fromIsSymbol2)();
    return (rec) => id($0.gEncodeJson(rec)($$Proxy));
  })();
  var encodeJson22 = /* @__PURE__ */ (() => encodeSet(ordJsonValueType)(encodeJsonJsonValueType).encodeJson)();
  var unorderedList5 = /* @__PURE__ */ unorderedList(foldable1NonEmptyArray);
  var foldMap12 = /* @__PURE__ */ (() => foldable1NonEmptySet.foldMap1(semigroupArray))();
  var fromFoldable12 = /* @__PURE__ */ fromFoldable5(foldableSet);
  var join1With2 = (splice) => (xs) => foldlArray((v) => (v1) => {
    if (v.init) {
      return { init: false, acc: v1 };
    }
    return { init: false, acc: v.acc + splice + v1 };
  })({ init: true, acc: "" })(xs).acc;
  var fromFoldable24 = ($0) => fromFoldableImpl((f) => (b) => (v) => f(v._1)(foldrArray(f)(b)(v._2)), $0);
  var member2 = (k) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Leaf") {
          go$c = false;
          go$r = false;
          continue;
        }
        if (v.tag === "Node") {
          const v1 = _compare(EQ, GT, LT, k, v._3);
          if (v1 === "LT") {
            go$a0 = v._5;
            continue;
          }
          if (v1 === "GT") {
            go$a0 = v._6;
            continue;
          }
          if (v1 === "EQ") {
            go$c = false;
            go$r = true;
            continue;
          }
        }
        fail();
      }
      return go$r;
    };
    return go;
  };
  var member1 = (k) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v.tag === "Leaf") {
          go$c = false;
          go$r = false;
          continue;
        }
        if (v.tag === "Node") {
          const v1 = ordJsonValueType.compare(k)(v._3);
          if (v1 === "LT") {
            go$a0 = v._5;
            continue;
          }
          if (v1 === "GT") {
            go$a0 = v._6;
            continue;
          }
          if (v1 === "EQ") {
            go$c = false;
            go$r = true;
            continue;
          }
        }
        fail();
      }
      return go$r;
    };
    return go;
  };
  var AlwaysFailingSchema = /* @__PURE__ */ $ViolationReason("AlwaysFailingSchema");
  var NonUniqueArrayItem = /* @__PURE__ */ $ViolationReason("NonUniqueArrayItem");
  var ValidAgainstNotSchema = /* @__PURE__ */ $ViolationReason("ValidAgainstNotSchema");
  var eqViolationReason = {
    eq: (x) => (y) => {
      if (x.tag === "AlwaysFailingSchema") {
        return y.tag === "AlwaysFailingSchema";
      }
      if (x.tag === "InvalidArray") {
        return y.tag === "InvalidArray" && eqMap(eqViolation)(eqUnit).eq(x._1)(y._1);
      }
      if (x.tag === "InvalidMultiple") {
        return y.tag === "InvalidMultiple" && x._1.expectedMultiple === y._1.expectedMultiple && x._1.value === y._1.value;
      }
      if (x.tag === "InvalidRange") {
        return y.tag === "InvalidRange" && (x._1.validRange.from.tag === "Closed" ? y._1.validRange.from.tag === "Closed" && x._1.validRange.from._1 === y._1.validRange.from._1 : x._1.validRange.from.tag === "Open" && y._1.validRange.from.tag === "Open" && x._1.validRange.from._1 === y._1.validRange.from._1) && (x._1.validRange.to.tag === "Closed" ? y._1.validRange.to.tag === "Closed" && x._1.validRange.to._1 === y._1.validRange.to._1 : x._1.validRange.to.tag === "Open" && y._1.validRange.to.tag === "Open" && x._1.validRange.to._1 === y._1.validRange.to._1) && x._1.value === y._1.value;
      }
      if (x.tag === "NonUniqueArrayItem") {
        return y.tag === "NonUniqueArrayItem";
      }
      if (x.tag === "TypeMismatch") {
        return y.tag === "TypeMismatch" && (() => {
          if (x._1.actualJsonValueType === "JsonArray") {
            return y._1.actualJsonValueType === "JsonArray";
          }
          if (x._1.actualJsonValueType === "JsonBoolean") {
            return y._1.actualJsonValueType === "JsonBoolean";
          }
          if (x._1.actualJsonValueType === "JsonInteger") {
            return y._1.actualJsonValueType === "JsonInteger";
          }
          if (x._1.actualJsonValueType === "JsonNull") {
            return y._1.actualJsonValueType === "JsonNull";
          }
          if (x._1.actualJsonValueType === "JsonNumber") {
            return y._1.actualJsonValueType === "JsonNumber";
          }
          if (x._1.actualJsonValueType === "JsonObject") {
            return y._1.actualJsonValueType === "JsonObject";
          }
          return x._1.actualJsonValueType === "JsonString" && y._1.actualJsonValueType === "JsonString";
        })() && eqMap(eqJsonValueType)(eqUnit).eq(x._1.allowedJsonValueTypes)(y._1.allowedJsonValueTypes);
      }
      return x.tag === "ValidAgainstNotSchema" && y.tag === "ValidAgainstNotSchema";
    }
  };
  var eqViolation = { eq: (x) => (y) => eq42(x.jsonPath)(y.jsonPath) && eqViolationReason.eq(x.reason)(y.reason) && eq52(x.schemaPath)(y.schemaPath) };
  var ordViolationReason = {
    compare: (x) => (y) => {
      if (x.tag === "AlwaysFailingSchema") {
        if (y.tag === "AlwaysFailingSchema") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "AlwaysFailingSchema") {
        return GT;
      }
      if (x.tag === "InvalidArray") {
        if (y.tag === "InvalidArray") {
          return ordSet(ordViolation).compare(x._1)(y._1);
        }
        return LT;
      }
      if (y.tag === "InvalidArray") {
        return GT;
      }
      if (x.tag === "InvalidMultiple") {
        if (y.tag === "InvalidMultiple") {
          const v = ordNumber.compare(x._1.expectedMultiple)(y._1.expectedMultiple);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return ordNumber.compare(x._1.value)(y._1.value);
        }
        return LT;
      }
      if (y.tag === "InvalidMultiple") {
        return GT;
      }
      if (x.tag === "InvalidRange") {
        if (y.tag === "InvalidRange") {
          const v1 = ordBoundary.compare(x._1.validRange.from)(y._1.validRange.from);
          if (v1 === "LT") {
            return LT;
          }
          if (v1 === "GT") {
            return GT;
          }
          if (ordBoundary.compare(x._1.validRange.to)(y._1.validRange.to) === "LT") {
            return LT;
          }
          if (ordBoundary.compare(x._1.validRange.to)(y._1.validRange.to) === "GT") {
            return GT;
          }
          return ordNumber.compare(x._1.value)(y._1.value);
        }
        return LT;
      }
      if (y.tag === "InvalidRange") {
        return GT;
      }
      if (x.tag === "NonUniqueArrayItem") {
        if (y.tag === "NonUniqueArrayItem") {
          return EQ;
        }
        return LT;
      }
      if (y.tag === "NonUniqueArrayItem") {
        return GT;
      }
      if (x.tag === "TypeMismatch") {
        if (y.tag === "TypeMismatch") {
          const v = ordJsonValueType.compare(x._1.actualJsonValueType)(y._1.actualJsonValueType);
          if (v === "LT") {
            return LT;
          }
          if (v === "GT") {
            return GT;
          }
          return compare33(x._1.allowedJsonValueTypes)(y._1.allowedJsonValueTypes);
        }
        return LT;
      }
      if (y.tag === "TypeMismatch") {
        return GT;
      }
      if (x.tag === "ValidAgainstNotSchema" && y.tag === "ValidAgainstNotSchema") {
        return EQ;
      }
      fail();
    },
    Eq0: () => eqViolationReason
  };
  var ordViolation = {
    compare: (x) => (y) => {
      const v = compare42(x.jsonPath)(y.jsonPath);
      if (v === "LT") {
        return LT;
      }
      if (v === "GT") {
        return GT;
      }
      const v1 = ordViolationReason.compare(x.reason)(y.reason);
      if (v1 === "LT") {
        return LT;
      }
      if (v1 === "GT") {
        return GT;
      }
      return compare52(x.schemaPath)(y.schemaPath);
    },
    Eq0: () => eqViolation
  };
  var foldMapWithIndex = /* @__PURE__ */ (() => foldableWithIndexArray.foldMapWithIndex(monoidSet(ordViolation)))();
  var encodeJsonViolationReason = {
    encodeJson: (v) => {
      if (v.tag === "AlwaysFailingSchema") {
        return id("always failing schema");
      }
      if (v.tag === "InvalidArray") {
        return id(arrayMap(encodeJsonViolation$lazy().encodeJson)(fromFoldableImpl(foldableSet.foldr, v._1)));
      }
      if (v.tag === "InvalidMultiple") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "expectedMultiple",
          id(v._1.expectedMultiple)
        ))(extend(encodeJsonJson.encodeJson)($Tuple("value", id(v._1.value)))(jsonEmptyObject));
      }
      if (v.tag === "InvalidRange") {
        return extend(encodeJsonJson.encodeJson)($Tuple("validRange", encodeJson6(v._1.validRange)))(extend(encodeJsonJson.encodeJson)($Tuple(
          "value",
          id(v._1.value)
        ))(jsonEmptyObject));
      }
      if (v.tag === "NonUniqueArrayItem") {
        return id("non-unique array item");
      }
      if (v.tag === "TypeMismatch") {
        return extend(encodeJsonJson.encodeJson)($Tuple(
          "actualJsonValueType",
          encodeJsonJsonValueType.encodeJson(v._1.actualJsonValueType)
        ))(extend(encodeJsonJson.encodeJson)($Tuple(
          "allowedJsonValueTypes",
          encodeJson22(v._1.allowedJsonValueTypes)
        ))(jsonEmptyObject));
      }
      if (v.tag === "ValidAgainstNotSchema") {
        return id("valid against 'not' schema");
      }
      fail();
    }
  };
  var encodeJsonViolation$lazy = /* @__PURE__ */ binding(() => {
    const $0 = gEncodeJsonCons({ encodeJson: encodeList(encodeJsonJsonPathSegment.encodeJson) })(gEncodeJsonCons(encodeJsonViolationReason)(gEncodeJsonCons({
      encodeJson: encodeList(encodeJsonSchemaPathSegme.encodeJson)
    })(gEncodeJsonNil)(schemaPathIsSymbol)())(reasonIsSymbol)())(jsonPathIsSymbol)();
    return { encodeJson: (rec) => id($0.gEncodeJson(rec)($$Proxy)) };
  });
  var encodeJsonViolation = /* @__PURE__ */ encodeJsonViolation$lazy();
  var documentViolationReason = {
    document: (v) => {
      if (v.tag === "AlwaysFailingSchema") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "Schema always fails validation.")])
          ),
          []
        );
      }
      if (v.tag === "InvalidArray") {
        return $NonEmpty(
          $FlowContentNode("Paragraph", fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "Invalid array:")])),
          [
            unorderedList5(foldMap12((x) => [
              (() => {
                const $0 = documentViolation.document(x);
                return [$0._1, ...$0._2];
              })()
            ])(v._1))
          ]
        );
      }
      if (v.tag === "InvalidMultiple") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const $0 = showNumberImpl(v._1.value);
                    return ($0 === "" ? "impossible is not a multiple of " : $0 + " is not a multiple of ") + showNumberImpl(v._1.expectedMultiple);
                  })()
                )
              ]
            )
          ),
          []
        );
      }
      if (v.tag === "InvalidRange") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [
                $PhrasingContentNode("Text", showNumberImpl(v._1.value) + " is outside of the valid range of "),
                renderRange(v._1.validRange)
              ]
            )
          ),
          []
        );
      }
      if (v.tag === "NonUniqueArrayItem") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "Non-unique array item.")])
          ),
          []
        );
      }
      if (v.tag === "TypeMismatch") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(
              foldrArray,
              [
                $PhrasingContentNode(
                  "Text",
                  (() => {
                    const v1 = fromFoldable12(v._1.allowedJsonValueTypes);
                    return (() => {
                      if (v1.tag === "Nothing") {
                        return "Invalid type. Expected none but got ";
                      }
                      if (v1.tag === "Just") {
                        return "Invalid type. Expected " + join1With2(" or ")(arrayMap(renderJsonValueType)(v1._1)) + " but got ";
                      }
                      fail();
                    })() + (() => {
                      if (v._1.actualJsonValueType === "JsonArray") {
                        return "array";
                      }
                      if (v._1.actualJsonValueType === "JsonBoolean") {
                        return "boolean";
                      }
                      if (v._1.actualJsonValueType === "JsonInteger") {
                        return "integer";
                      }
                      if (v._1.actualJsonValueType === "JsonNull") {
                        return "null";
                      }
                      if (v._1.actualJsonValueType === "JsonNumber") {
                        return "number";
                      }
                      if (v._1.actualJsonValueType === "JsonObject") {
                        return "object";
                      }
                      if (v._1.actualJsonValueType === "JsonString") {
                        return "string";
                      }
                      fail();
                    })() + ".";
                  })()
                )
              ]
            )
          ),
          []
        );
      }
      if (v.tag === "ValidAgainstNotSchema") {
        return $NonEmpty(
          $FlowContentNode(
            "Paragraph",
            fromFoldableImpl(foldrArray, [$PhrasingContentNode("Text", "JSON is valid against schema from 'not'.")])
          ),
          []
        );
      }
      fail();
    }
  };
  var documentViolation = {
    document: (v) => $NonEmpty(
      $FlowContentNode(
        "Paragraph",
        fromFoldableImpl(
          foldrArray,
          [
            $PhrasingContentNode("Text", "JSON value path: "),
            $PhrasingContentNode("InlineCode", render4(v.jsonPath)),
            LineBreak,
            $PhrasingContentNode("Text", "JSON schema path: "),
            $PhrasingContentNode("InlineCode", render3(v.schemaPath))
          ]
        )
      ),
      fromFoldable24(documentViolationReason.document(v.reason))
    )
  };
  var validateUniqueItems = (schemaPath) => (jsonPath) => (itemJsons) => {
    const duplicates = keys3(filterWithKey(ordJson)((v) => (v$1) => v$1 > 1)(foldlArray((acc) => (json) => insertWith(ordJson)(intAdd)(json)(1)(acc))(Leaf2)(itemJsons)));
    return foldMapWithIndex((itemIndex) => (itemJson) => {
      if (member2(itemJson)(duplicates)) {
        return $$$Map(
          "Node",
          1,
          1,
          { jsonPath: $List("Cons", $JsonPathSegment("ItemIndex", itemIndex), jsonPath), reason: NonUniqueArrayItem, schemaPath },
          void 0,
          Leaf2,
          Leaf2
        );
      }
      return Leaf2;
    })(itemJsons);
  };
  var validateTypeKeyword = (schemaPath) => (jsonPath) => (json) => (allowedJsonValueTypes) => {
    const jsonValueType = _caseJson(
      (v) => JsonNull,
      (v) => JsonBoolean,
      (x) => {
        if (toNumber(unsafeClamp(trunc(x))) === x) {
          return JsonInteger;
        }
        return JsonNumber;
      },
      (v) => JsonString,
      (v) => JsonArray,
      (v) => JsonObject,
      json
    );
    if (jsonValueType === "JsonInteger" && (member1(JsonInteger)(allowedJsonValueTypes) || member1(JsonNumber)(allowedJsonValueTypes))) {
      return Leaf2;
    }
    if (member1(jsonValueType)(allowedJsonValueTypes)) {
      return Leaf2;
    }
    return $$$Map(
      "Node",
      1,
      1,
      {
        jsonPath,
        reason: $ViolationReason("TypeMismatch", { actualJsonValueType: jsonValueType, allowedJsonValueTypes }),
        schemaPath: $List("Cons", TypeKeyword, schemaPath)
      },
      void 0,
      Leaf2,
      Leaf2
    );
  };
  var validateMultipleOf = (schemaPath) => (jsonPath) => (x) => (v) => {
    if (v.tag === "Just") {
      if ((() => {
        const $0 = x / v._1;
        return toNumber(unsafeClamp(trunc($0))) === $0;
      })()) {
        return Leaf2;
      }
      return $$$Map(
        "Node",
        1,
        1,
        {
          jsonPath,
          reason: $ViolationReason("InvalidMultiple", { expectedMultiple: v._1, value: x }),
          schemaPath: $List("Cons", MultipleOf, schemaPath)
        },
        void 0,
        Leaf2,
        Leaf2
      );
    }
    if (v.tag === "Nothing") {
      return Leaf2;
    }
    fail();
  };
  var validateNumber = (schemaPath) => (jsonPath) => (constraints) => (x) => {
    const validRange = {
      from: (() => {
        if (constraints.exclusiveMinimum.tag === "Nothing") {
          if (constraints.minimum.tag === "Nothing") {
            return $Boundary("Open", -Infinity);
          }
          if (constraints.minimum.tag === "Just") {
            return $Boundary("Closed", constraints.minimum._1);
          }
          fail();
        }
        if (constraints.exclusiveMinimum.tag === "Just") {
          if (constraints.minimum.tag === "Nothing") {
            return $Boundary("Open", constraints.exclusiveMinimum._1);
          }
          if (constraints.minimum.tag === "Just") {
            if (constraints.minimum._1 < constraints.exclusiveMinimum._1) {
              return $Boundary("Closed", constraints.minimum._1);
            }
            return $Boundary("Open", constraints.exclusiveMinimum._1);
          }
        }
        fail();
      })(),
      to: (() => {
        if (constraints.exclusiveMaximum.tag === "Nothing") {
          if (constraints.maximum.tag === "Nothing") {
            return $Boundary("Open", Infinity);
          }
          if (constraints.maximum.tag === "Just") {
            return $Boundary("Closed", constraints.maximum._1);
          }
          fail();
        }
        if (constraints.exclusiveMaximum.tag === "Just") {
          if (constraints.maximum.tag === "Nothing") {
            return $Boundary("Open", constraints.exclusiveMaximum._1);
          }
          if (constraints.maximum.tag === "Just") {
            if (constraints.maximum._1 > constraints.exclusiveMaximum._1) {
              return $Boundary("Closed", constraints.maximum._1);
            }
            return $Boundary("Open", constraints.exclusiveMaximum._1);
          }
        }
        fail();
      })()
    };
    return unsafeUnionWith(
      ordViolation.compare,
      $$const,
      validateMultipleOf(schemaPath)(jsonPath)(x)(constraints.multipleOf),
      unsafeUnionWith(
        ordViolation.compare,
        $$const,
        (() => {
          if (constraints.exclusiveMaximum.tag === "Nothing") {
            return Leaf2;
          }
          if (constraints.exclusiveMaximum.tag === "Just") {
            if (x < constraints.exclusiveMaximum._1) {
              return Leaf2;
            }
            return $$$Map(
              "Node",
              1,
              1,
              {
                jsonPath,
                reason: $ViolationReason("InvalidRange", { validRange, value: x }),
                schemaPath: $List("Cons", ExclusiveMaximum, schemaPath)
              },
              void 0,
              Leaf2,
              Leaf2
            );
          }
          fail();
        })(),
        unsafeUnionWith(
          ordViolation.compare,
          $$const,
          (() => {
            if (constraints.exclusiveMinimum.tag === "Nothing") {
              return Leaf2;
            }
            if (constraints.exclusiveMinimum.tag === "Just") {
              if (x > constraints.exclusiveMinimum._1) {
                return Leaf2;
              }
              return $$$Map(
                "Node",
                1,
                1,
                {
                  jsonPath,
                  reason: $ViolationReason("InvalidRange", { validRange, value: x }),
                  schemaPath: $List("Cons", ExclusiveMinimum, schemaPath)
                },
                void 0,
                Leaf2,
                Leaf2
              );
            }
            fail();
          })(),
          unsafeUnionWith(
            ordViolation.compare,
            $$const,
            (() => {
              if (constraints.maximum.tag === "Nothing") {
                return Leaf2;
              }
              if (constraints.maximum.tag === "Just") {
                if (x <= constraints.maximum._1) {
                  return Leaf2;
                }
                return $$$Map(
                  "Node",
                  1,
                  1,
                  {
                    jsonPath,
                    reason: $ViolationReason("InvalidRange", { validRange, value: x }),
                    schemaPath: $List("Cons", Maximum, schemaPath)
                  },
                  void 0,
                  Leaf2,
                  Leaf2
                );
              }
              fail();
            })(),
            (() => {
              if (constraints.minimum.tag === "Nothing") {
                return Leaf2;
              }
              if (constraints.minimum.tag === "Just") {
                if (x >= constraints.minimum._1) {
                  return Leaf2;
                }
                return $$$Map(
                  "Node",
                  1,
                  1,
                  {
                    jsonPath,
                    reason: $ViolationReason("InvalidRange", { validRange, value: x }),
                    schemaPath: $List("Cons", Minimum, schemaPath)
                  },
                  void 0,
                  Leaf2,
                  Leaf2
                );
              }
              fail();
            })()
          )
        )
      )
    );
  };
  var validateAgainst$lazy = /* @__PURE__ */ binding(() => {
    const validateItems = (schemaPath) => (jsonPath) => (itemJsons) => (schema) => foldMapWithIndex((itemIndex) => (itemJson) => go(schemaPath)($List(
      "Cons",
      $JsonPathSegment("ItemIndex", itemIndex),
      jsonPath
    ))(itemJson)(schema))(itemJsons);
    const validateArray = (schemaPath) => (jsonPath) => (array) => (constraints) => unsafeUnionWith(
      ordViolation.compare,
      $$const,
      (() => {
        const $0 = validateItems($List("Cons", Items, schemaPath))(jsonPath)(array);
        if (constraints.items.tag === "Nothing") {
          return Leaf2;
        }
        if (constraints.items.tag === "Just") {
          return $0(constraints.items._1);
        }
        fail();
      })(),
      constraints.uniqueItems ? validateUniqueItems($List("Cons", UniqueItems, schemaPath))(jsonPath)(array) : Leaf2
    );
    const validateAgainstObjectSchema = (schemaPath) => (jsonPath) => (json) => (keywords) => unsafeUnionWith(
      ordViolation.compare,
      $$const,
      (() => {
        if (keywords.not.tag === "Just") {
          if (validateAgainst$lazy()(json)(keywords.not._1).tag === "Leaf") {
            return $$$Map("Node", 1, 1, { jsonPath, reason: ValidAgainstNotSchema, schemaPath }, void 0, Leaf2, Leaf2);
          }
          return Leaf2;
        }
        if (keywords.not.tag === "Nothing") {
          return Leaf2;
        }
        fail();
      })(),
      unsafeUnionWith(
        ordViolation.compare,
        $$const,
        (() => {
          if (keywords.typeKeyword.tag === "Nothing") {
            return Leaf2;
          }
          if (keywords.typeKeyword.tag === "Just") {
            return validateTypeKeyword(schemaPath)(jsonPath)(json)(keywords.typeKeyword._1);
          }
          fail();
        })(),
        _caseJson(
          (v) => Leaf2,
          (v) => Leaf2,
          validateNumber(schemaPath)(jsonPath)(keywords),
          (v) => Leaf2,
          (array) => {
            if ((() => {
              if (keywords.typeKeyword.tag === "Just") {
                return member1(JsonArray)(keywords.typeKeyword._1);
              }
              if (keywords.typeKeyword.tag === "Nothing") {
                return true;
              }
              fail();
            })()) {
              const $0 = validateArray(schemaPath)(jsonPath)(arrayMap(unsafeCoerce)(array))(keywords);
              if ($0.tag === "Leaf") {
                return Leaf2;
              }
              return $$$Map(
                "Node",
                1,
                1,
                { jsonPath, reason: $ViolationReason("InvalidArray", $0), schemaPath },
                void 0,
                Leaf2,
                Leaf2
              );
            }
            return Leaf2;
          },
          (v) => Leaf2,
          json
        )
      )
    );
    const go = (schemaPath) => (jsonPath) => (json) => (schema) => {
      if (schema.tag === "BooleanSchema") {
        if (schema._1) {
          return Leaf2;
        }
        return $$$Map("Node", 1, 1, { jsonPath, reason: AlwaysFailingSchema, schemaPath }, void 0, Leaf2, Leaf2);
      }
      if (schema.tag === "ObjectSchema") {
        return validateAgainstObjectSchema(schemaPath)(jsonPath)(json)(schema._1);
      }
      fail();
    };
    return go(Nil)(Nil);
  });
  var validateAgainst = /* @__PURE__ */ validateAgainst$lazy();

  // output-es/CLI.Command.Validate/index.js
  var documentSet2 = { document: /* @__PURE__ */ documentFoldable(documentViolation)(foldableSet)(false) };
  var encodeSet3 = /* @__PURE__ */ encodeSet(ordViolation)(encodeJsonViolation);
  var program3 = (dictFileAccess) => {
    const Monad0 = dictFileAccess.Monad0();
    const Applicative0 = Monad0.Applicative0();
    const $0 = Monad0.Bind1();
    return (dictMonadError) => {
      const $1 = dictMonadError.MonadThrow0();
      const $2 = $1.Monad0().Applicative0().pure;
      const liftEither = (v2) => {
        if (v2.tag === "Left") {
          return $1.throwError(v2._1);
        }
        if (v2.tag === "Right") {
          return $2(v2._1);
        }
        fail();
      };
      return commandProgram(Applicative0)(documentSet2)(encodeSet3)(dictMonadError)((v) => {
        const $3 = v.jsonFilePath;
        return $0.bind(dictFileAccess.readFileContent(v.schemaFilePath))((schemaText) => $0.bind(dictFileAccess.readFileContent($3))((jsonText) => $0.bind((() => {
          const $4 = _jsonParser(Left, Right, schemaText);
          return liftEither((() => {
            const $5 = (() => {
              if ($4.tag === "Left") {
                return $Either("Left", $4._1);
              }
              if ($4.tag === "Right") {
                return parseSchema($4._1);
              }
              fail();
            })();
            if ($5.tag === "Left") {
              return $Either("Left", error($5._1));
            }
            if ($5.tag === "Right") {
              return $Either("Right", $5._1);
            }
            fail();
          })());
        })())((schema) => $0.bind((() => {
          const $4 = _jsonParser(Left, Right, jsonText);
          return liftEither((() => {
            if ($4.tag === "Left") {
              return $Either("Left", error($4._1));
            }
            if ($4.tag === "Right") {
              return $Either("Right", $4._1);
            }
            fail();
          })());
        })())((json) => {
          const violations = validateAgainst(json)(schema);
          return Applicative0.pure(violations.tag === "Leaf" ? $Either("Right", violations) : $Either("Left", violations));
        }))));
      });
    };
  };

  // output-es/Data.CatQueue/index.js
  var $CatQueue = (_1, _2) => ({ tag: "CatQueue", _1, _2 });
  var uncons2 = (uncons$a0$copy) => {
    let uncons$a0 = uncons$a0$copy, uncons$c = true, uncons$r;
    while (uncons$c) {
      const v = uncons$a0;
      if (v._1.tag === "Nil") {
        if (v._2.tag === "Nil") {
          uncons$c = false;
          uncons$r = Nothing;
          continue;
        }
        uncons$a0 = $CatQueue(reverse2(v._2), Nil);
        continue;
      }
      if (v._1.tag === "Cons") {
        uncons$c = false;
        uncons$r = $Maybe("Just", $Tuple(v._1._1, $CatQueue(v._1._2, v._2)));
        continue;
      }
      fail();
    }
    return uncons$r;
  };

  // output-es/Data.CatList/index.js
  var $CatList = (tag, _1, _2) => ({ tag, _1, _2 });
  var CatNil = /* @__PURE__ */ $CatList("CatNil");
  var link = (v) => (v1) => {
    if (v.tag === "CatNil") {
      return v1;
    }
    if (v1.tag === "CatNil") {
      return v;
    }
    if (v.tag === "CatCons") {
      return $CatList("CatCons", v._1, $CatQueue(v._2._1, $List("Cons", v1, v._2._2)));
    }
    fail();
  };
  var foldr = (k) => (b) => (q) => {
    const foldl = (foldl$a0$copy) => (foldl$a1$copy) => (foldl$a2$copy) => {
      let foldl$a0 = foldl$a0$copy, foldl$a1 = foldl$a1$copy, foldl$a2 = foldl$a2$copy, foldl$c = true, foldl$r;
      while (foldl$c) {
        const v = foldl$a0, v1 = foldl$a1, v2 = foldl$a2;
        if (v2.tag === "Nil") {
          foldl$c = false;
          foldl$r = v1;
          continue;
        }
        if (v2.tag === "Cons") {
          foldl$a0 = v;
          foldl$a1 = v(v1)(v2._1);
          foldl$a2 = v2._2;
          continue;
        }
        fail();
      }
      return foldl$r;
    };
    const go = (go$a0$copy) => (go$a1$copy) => {
      let go$a0 = go$a0$copy, go$a1 = go$a1$copy, go$c = true, go$r;
      while (go$c) {
        const xs = go$a0, ys = go$a1;
        const v = uncons2(xs);
        if (v.tag === "Nothing") {
          go$c = false;
          go$r = foldl((x) => (i) => i(x))(b)(ys);
          continue;
        }
        if (v.tag === "Just") {
          go$a0 = v._1._2;
          go$a1 = $List("Cons", k(v._1._1), ys);
          continue;
        }
        fail();
      }
      return go$r;
    };
    return go(q)(Nil);
  };
  var uncons3 = (v) => {
    if (v.tag === "CatNil") {
      return Nothing;
    }
    if (v.tag === "CatCons") {
      return $Maybe("Just", $Tuple(v._1, v._2._1.tag === "Nil" && v._2._2.tag === "Nil" ? CatNil : foldr(link)(CatNil)(v._2)));
    }
    fail();
  };
  var snoc2 = (cat) => (a) => {
    if (cat.tag === "CatNil") {
      return $CatList("CatCons", a, $CatQueue(Nil, Nil));
    }
    if (cat.tag === "CatCons") {
      return $CatList(
        "CatCons",
        cat._1,
        $CatQueue(
          cat._2._1,
          $List("Cons", $CatList("CatCons", a, $CatQueue(Nil, Nil)), cat._2._2)
        )
      );
    }
    fail();
  };

  // output-es/Control.Monad.Free/index.js
  var $Free = (_1, _2) => ({ tag: "Free", _1, _2 });
  var $FreeView = (tag, _1, _2) => ({ tag, _1, _2 });
  var toView = (toView$a0$copy) => {
    let toView$a0 = toView$a0$copy, toView$c = true, toView$r;
    while (toView$c) {
      const v = toView$a0;
      if (v._1.tag === "Return") {
        const v2 = uncons3(v._2);
        if (v2.tag === "Nothing") {
          toView$c = false;
          toView$r = $FreeView("Return", v._1._1);
          continue;
        }
        if (v2.tag === "Just") {
          toView$a0 = (() => {
            const $0 = v2._1._1(v._1._1);
            return $Free(
              $0._1,
              (() => {
                if ($0._2.tag === "CatNil") {
                  return v2._1._2;
                }
                if (v2._1._2.tag === "CatNil") {
                  return $0._2;
                }
                if ($0._2.tag === "CatCons") {
                  return $CatList("CatCons", $0._2._1, $CatQueue($0._2._2._1, $List("Cons", v2._1._2, $0._2._2._2)));
                }
                fail();
              })()
            );
          })();
          continue;
        }
        fail();
      }
      if (v._1.tag === "Bind") {
        toView$c = false;
        toView$r = $FreeView(
          "Bind",
          v._1._1,
          (a) => {
            const $0 = v._1._2(a);
            return $Free(
              $0._1,
              (() => {
                if ($0._2.tag === "CatNil") {
                  return v._2;
                }
                if (v._2.tag === "CatNil") {
                  return $0._2;
                }
                if ($0._2.tag === "CatCons") {
                  return $CatList("CatCons", $0._2._1, $CatQueue($0._2._2._1, $List("Cons", v._2, $0._2._2._2)));
                }
                fail();
              })()
            );
          }
        );
        continue;
      }
      fail();
    }
    return toView$r;
  };
  var freeMonad = { Applicative0: () => freeApplicative, Bind1: () => freeBind };
  var freeFunctor = { map: (k) => (f) => freeBind.bind(f)((x) => freeApplicative.pure(k(x))) };
  var freeBind = { bind: (v) => (k) => $Free(v._1, snoc2(v._2)(k)), Apply0: () => freeApply };
  var freeApply = {
    apply: (f) => (a) => $Free(f._1, snoc2(f._2)((f$p) => $Free(a._1, snoc2(a._2)((a$p) => freeApplicative.pure(f$p(a$p)))))),
    Functor0: () => freeFunctor
  };
  var freeApplicative = { pure: (x) => $Free($FreeView("Return", x), CatNil), Apply0: () => freeApply };
  var foldFree = (dictMonadRec) => {
    const Monad0 = dictMonadRec.Monad0();
    const $0 = Monad0.Bind1().Apply0().Functor0();
    return (k) => dictMonadRec.tailRecM((f) => {
      const v = toView(f);
      if (v.tag === "Return") {
        return $0.map(Done)(Monad0.Applicative0().pure(v._1));
      }
      if (v.tag === "Bind") {
        return $0.map((x) => $Step("Loop", v._2(x)))(k(v._1));
      }
      fail();
    });
  };

  // output-es/Control.Monad.Except.Trans/index.js
  var bindExceptT = (dictMonad) => ({
    bind: (v) => (k) => dictMonad.Bind1().bind(v)((v2) => {
      if (v2.tag === "Left") {
        return dictMonad.Applicative0().pure($Either("Left", v2._1));
      }
      if (v2.tag === "Right") {
        return k(v2._1);
      }
      fail();
    }),
    Apply0: () => applyExceptT(dictMonad)
  });
  var applyExceptT = (dictMonad) => {
    const $0 = dictMonad.Bind1().Apply0().Functor0();
    const functorExceptT1 = {
      map: (f) => $0.map((m) => {
        if (m.tag === "Left") {
          return $Either("Left", m._1);
        }
        if (m.tag === "Right") {
          return $Either("Right", f(m._1));
        }
        fail();
      })
    };
    return {
      apply: (() => {
        const $1 = bindExceptT(dictMonad);
        return (f) => (a) => $1.bind(f)((f$p) => $1.bind(a)((a$p) => applicativeExceptT(dictMonad).pure(f$p(a$p))));
      })(),
      Functor0: () => functorExceptT1
    };
  };
  var applicativeExceptT = (dictMonad) => ({ pure: (x) => dictMonad.Applicative0().pure($Either("Right", x)), Apply0: () => applyExceptT(dictMonad) });
  var monadThrowExceptT = (dictMonad) => {
    const monadExceptT1 = { Applicative0: () => applicativeExceptT(dictMonad), Bind1: () => bindExceptT(dictMonad) };
    return { throwError: (x) => dictMonad.Applicative0().pure($Either("Left", x)), Monad0: () => monadExceptT1 };
  };

  // output-es/Foreign/foreign.js
  function typeOf(value4) {
    return typeof value4;
  }
  function tagOf(value4) {
    return Object.prototype.toString.call(value4).slice(8, -1);
  }
  var isArray2 = Array.isArray || function(value4) {
    return Object.prototype.toString.call(value4) === "[object Array]";
  };

  // output-es/Foreign/index.js
  var $ForeignError = (tag, _1, _2) => ({ tag, _1, _2 });
  var unsafeReadTagged = (dictMonad) => (tag) => (value4) => {
    if (tagOf(value4) === tag) {
      return applicativeExceptT(dictMonad).pure(value4);
    }
    return monadThrowExceptT(dictMonad).throwError($NonEmpty($ForeignError("TypeMismatch", tag, tagOf(value4)), Nil));
  };
  var readInt = (dictMonad) => (value4) => {
    const error3 = $Either("Left", $NonEmpty($ForeignError("TypeMismatch", "Int", tagOf(value4)), Nil));
    return dictMonad.Bind1().Apply0().Functor0().map((v2) => {
      if (v2.tag === "Left") {
        return error3;
      }
      if (v2.tag === "Right") {
        const $0 = fromNumber(v2._1);
        if ($0.tag === "Nothing") {
          return error3;
        }
        if ($0.tag === "Just") {
          return $Either("Right", $0._1);
        }
      }
      fail();
    })(unsafeReadTagged(dictMonad)("Number")(value4));
  };
  var readString = (dictMonad) => unsafeReadTagged(dictMonad)("String");

  // output-es/Data.Nullable/foreign.js
  var nullImpl = null;
  function nullable(a, r, f) {
    return a == null ? r : f(a);
  }
  function notNull(x) {
    return x;
  }

  // output-es/Foreign.Index/foreign.js
  function unsafeReadPropImpl(f, s, key, value4) {
    return value4 == null ? f : s(value4[key]);
  }

  // output-es/Foreign.Index/index.js
  var unsafeReadProp = (dictMonad) => {
    const pure = applicativeExceptT(dictMonad).pure;
    return (k) => (value4) => unsafeReadPropImpl(
      monadThrowExceptT(dictMonad).throwError($NonEmpty(
        $ForeignError("TypeMismatch", "object", typeOf(value4)),
        Nil
      )),
      pure,
      k,
      value4
    );
  };

  // output-es/Halogen.Query.Input/index.js
  var $Input = (tag, _1, _2) => ({ tag, _1, _2 });

  // output-es/Halogen.VDom.Machine/index.js
  var $Step$p = (_1, _2, _3, _4) => ({ tag: "Step", _1, _2, _3, _4 });
  var step = (v, $0) => {
    const $1 = v._2;
    return v._3($1, $0);
  };
  var halt = (v) => {
    const $0 = v._2;
    return v._4($0);
  };

  // output-es/Halogen.VDom.Util/foreign.js
  function unsafeGetAny(key, obj) {
    return obj[key];
  }
  function unsafeHasAny(key, obj) {
    return obj.hasOwnProperty(key);
  }
  function unsafeSetAny(key, val, obj) {
    obj[key] = val;
  }
  function forE2(a, f) {
    var b = [];
    for (var i = 0; i < a.length; i++) {
      b.push(f(i, a[i]));
    }
    return b;
  }
  function forEachE(a, f) {
    for (var i = 0; i < a.length; i++) {
      f(a[i]);
    }
  }
  function forInE(o, f) {
    var ks = Object.keys(o);
    for (var i = 0; i < ks.length; i++) {
      var k = ks[i];
      f(k, o[k]);
    }
  }
  function diffWithIxE(a1, a2, f1, f2, f3) {
    var a3 = [];
    var l1 = a1.length;
    var l2 = a2.length;
    var i = 0;
    while (1) {
      if (i < l1) {
        if (i < l2) {
          a3.push(f1(i, a1[i], a2[i]));
        } else {
          f2(i, a1[i]);
        }
      } else if (i < l2) {
        a3.push(f3(i, a2[i]));
      } else {
        break;
      }
      i++;
    }
    return a3;
  }
  function strMapWithIxE(as, fk, f) {
    var o = {};
    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);
      o[k] = f(k, i, a);
    }
    return o;
  }
  function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
    var o2 = {};
    for (var i = 0; i < as.length; i++) {
      var a = as[i];
      var k = fk(a);
      if (o1.hasOwnProperty(k)) {
        o2[k] = f1(k, i, o1[k], a);
      } else {
        o2[k] = f3(k, i, a);
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
  function refEq2(a, b) {
    return a === b;
  }
  function createTextNode(s, doc) {
    return doc.createTextNode(s);
  }
  function setTextContent(s, n) {
    n.textContent = s;
  }
  function createElement(ns, name4, doc) {
    if (ns != null) {
      return doc.createElementNS(ns, name4);
    } else {
      return doc.createElement(name4);
    }
  }
  function insertChildIx(i, a, b) {
    var n = b.childNodes.item(i) || null;
    if (n !== a) {
      b.insertBefore(a, n);
    }
  }
  function removeChild(a, b) {
    if (b && a.parentNode === b) {
      b.removeChild(a);
    }
  }
  function parentNode(a) {
    return a.parentNode;
  }
  function setAttribute(ns, attr2, val, el) {
    if (ns != null) {
      el.setAttributeNS(ns, attr2, val);
    } else {
      el.setAttribute(attr2, val);
    }
  }
  function removeAttribute(ns, attr2, el) {
    if (ns != null) {
      el.removeAttributeNS(ns, attr2);
    } else {
      el.removeAttribute(attr2);
    }
  }
  function hasAttribute(ns, attr2, el) {
    if (ns != null) {
      return el.hasAttributeNS(ns, attr2);
    } else {
      return el.hasAttribute(attr2);
    }
  }
  function addEventListener(ev, listener, el) {
    el.addEventListener(ev, listener, false);
  }
  function removeEventListener(ev, listener, el) {
    el.removeEventListener(ev, listener, false);
  }
  var jsUndefined = void 0;

  // output-es/Web.Event.EventTarget/foreign.js
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
        return function(target) {
          return function() {
            return target.addEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }
  function removeEventListener2(type) {
    return function(listener) {
      return function(useCapture) {
        return function(target) {
          return function() {
            return target.removeEventListener(type, listener, useCapture);
          };
        };
      };
    };
  }

  // output-es/Halogen.VDom.DOM.Prop/index.js
  var $ElemRef = (tag, _1) => ({ tag, _1 });
  var $Prop = (tag, _1, _2, _3) => ({ tag, _1, _2, _3 });
  var Attribute = (value0) => (value1) => (value22) => $Prop("Attribute", value0, value1, value22);
  var Property = (value0) => (value1) => $Prop("Property", value0, value1);
  var removeProperty = (key, el) => {
    const v = hasAttribute(nullImpl, key, el);
    if (v) {
      return removeAttribute(nullImpl, key, el);
    }
    if (typeOf(unsafeGetAny(key, el)) === "string") {
      return unsafeSetAny(key, "", el);
    }
    if (key === "rowSpan") {
      return unsafeSetAny(key, 1, el);
    }
    if (key === "colSpan") {
      return unsafeSetAny(key, 1, el);
    }
    return unsafeSetAny(key, jsUndefined, el);
  };
  var propToStrKey = (v) => {
    if (v.tag === "Attribute") {
      if (v._1.tag === "Just") {
        return "attr/" + v._1._1 + ":" + v._2;
      }
      return "attr/:" + v._2;
    }
    if (v.tag === "Property") {
      return "prop/" + v._1;
    }
    if (v.tag === "Handler") {
      return "handler/" + v._1;
    }
    if (v.tag === "Ref") {
      return "ref";
    }
    fail();
  };
  var buildProp = (emit) => (el) => {
    const haltProp = (state) => {
      const v = _lookup(Nothing, Just, "ref", state.props);
      if (v.tag === "Just" && v._1.tag === "Ref") {
        const $0 = v._1._1($ElemRef("Removed", el));
        if ($0.tag === "Just") {
          return emit($0._1)();
        }
      }
    };
    const applyProp = (events) => (v, v1, v2) => {
      if (v2.tag === "Attribute") {
        const $0 = v2._2;
        const $1 = v2._3;
        const $2 = (() => {
          if (v2._1.tag === "Nothing") {
            return nullImpl;
          }
          if (v2._1.tag === "Just") {
            return notNull(v2._1._1);
          }
          fail();
        })();
        setAttribute($2, $0, $1, el);
        return v2;
      }
      if (v2.tag === "Property") {
        const $0 = v2._1;
        const $1 = v2._2;
        unsafeSetAny($0, $1, el);
        return v2;
      }
      if (v2.tag === "Handler") {
        if (unsafeHasAny(v2._1, events)) {
          const $0 = unsafeGetAny(v2._1, events)._2;
          $0.value = v2._2;
          return v2;
        }
        const ref = { value: v2._2 };
        const listener = eventListener((ev) => () => {
          const f$p = ref.value;
          const $0 = f$p(ev);
          if ($0.tag === "Just") {
            return emit($0._1)();
          }
        })();
        unsafeSetAny(v2._1, $Tuple(listener, ref), events);
        addEventListener(v2._1, listener, el);
        return v2;
      }
      if (v2.tag === "Ref") {
        const $0 = v2._1($ElemRef("Created", el));
        if ($0.tag === "Just") {
          emit($0._1)();
        }
        return v2;
      }
      fail();
    };
    const patchProp = (state, ps2) => {
      const events = {};
      const $0 = state.events;
      const props = diffWithKeyAndIxE(
        state.props,
        ps2,
        propToStrKey,
        (v, v1, v11, v2) => {
          if (v11.tag === "Attribute") {
            if (v2.tag === "Attribute") {
              if (v11._3 === v2._3) {
                return v2;
              }
              const $1 = (() => {
                if (v2._1.tag === "Nothing") {
                  return nullImpl;
                }
                if (v2._1.tag === "Just") {
                  return notNull(v2._1._1);
                }
                fail();
              })();
              setAttribute($1, v2._2, v2._3, el);
            }
            return v2;
          }
          if (v11.tag === "Property") {
            if (v2.tag === "Property") {
              if (refEq2(v11._2, v2._2)) {
                return v2;
              }
              if (v2._1 === "value" && refEq2(unsafeGetAny("value", el), v2._2)) {
                return v2;
              }
              unsafeSetAny(v2._1, v2._2, el);
            }
            return v2;
          }
          if (v11.tag === "Handler" && v2.tag === "Handler") {
            const $1 = v2._2;
            const $2 = v2._1;
            const handler = unsafeGetAny($2, $0);
            const $3 = handler._2;
            $3.value = $1;
            unsafeSetAny($2, handler, events);
          }
          return v2;
        },
        (v, v1) => {
          if (v1.tag === "Attribute") {
            const $1 = v1._2;
            const $2 = (() => {
              if (v1._1.tag === "Nothing") {
                return nullImpl;
              }
              if (v1._1.tag === "Just") {
                return notNull(v1._1._1);
              }
              fail();
            })();
            return removeAttribute($2, $1, el);
          }
          if (v1.tag === "Property") {
            const $1 = v1._1;
            return removeProperty($1, el);
          }
          if (v1.tag === "Handler") {
            const $1 = v1._1;
            const $2 = unsafeGetAny($1, $0)._1;
            return removeEventListener($1, $2, el);
          }
          if (v1.tag === "Ref") {
            return;
          }
          fail();
        },
        applyProp(events)
      );
      return $Step$p(void 0, { events, props }, patchProp, haltProp);
    };
    return (ps1) => {
      const events = {};
      const ps1$p = strMapWithIxE(ps1, propToStrKey, applyProp(events));
      return $Step$p(void 0, { events, props: ps1$p }, patchProp, haltProp);
    };
  };

  // output-es/Web.Event.Event/foreign.js
  function _currentTarget(e) {
    return e.currentTarget;
  }

  // output-es/Halogen.HTML.Events/index.js
  var readProp = /* @__PURE__ */ unsafeReadProp(monadIdentity);
  var handler$p = (et) => (f) => $Prop(
    "Handler",
    et,
    (ev) => {
      const $0 = f(ev);
      if ($0.tag === "Just") {
        return $Maybe("Just", $Input("Action", $0._1));
      }
      return Nothing;
    }
  );
  var addForeignPropHandler = (key) => (prop2) => (reader) => (f) => handler$p(key)((a) => {
    const $0 = nullable(_currentTarget(a), Nothing, Just);
    if ($0.tag === "Just") {
      const $1 = bindExceptT(monadIdentity).bind(readProp(prop2)($0._1))(reader);
      if ($1.tag === "Left") {
        return Nothing;
      }
      if ($1.tag === "Right") {
        return $Maybe("Just", f($1._1));
      }
      fail();
    }
    if ($0.tag === "Nothing") {
      return Nothing;
    }
    fail();
  });

  // output-es/DOM.HTML.Indexed.InputType/index.js
  var $InputType = (tag) => tag;
  var InputRadio = /* @__PURE__ */ $InputType("InputRadio");
  var renderInputType = (v) => {
    if (v === "InputButton") {
      return "button";
    }
    if (v === "InputCheckbox") {
      return "checkbox";
    }
    if (v === "InputColor") {
      return "color";
    }
    if (v === "InputDate") {
      return "date";
    }
    if (v === "InputDatetimeLocal") {
      return "datetime-local";
    }
    if (v === "InputEmail") {
      return "email";
    }
    if (v === "InputFile") {
      return "file";
    }
    if (v === "InputHidden") {
      return "hidden";
    }
    if (v === "InputImage") {
      return "image";
    }
    if (v === "InputMonth") {
      return "month";
    }
    if (v === "InputNumber") {
      return "number";
    }
    if (v === "InputPassword") {
      return "password";
    }
    if (v === "InputRadio") {
      return "radio";
    }
    if (v === "InputRange") {
      return "range";
    }
    if (v === "InputReset") {
      return "reset";
    }
    if (v === "InputSearch") {
      return "search";
    }
    if (v === "InputSubmit") {
      return "submit";
    }
    if (v === "InputTel") {
      return "tel";
    }
    if (v === "InputText") {
      return "text";
    }
    if (v === "InputTime") {
      return "time";
    }
    if (v === "InputUrl") {
      return "url";
    }
    if (v === "InputWeek") {
      return "week";
    }
    fail();
  };

  // output-es/Halogen.VDom.Types/index.js
  var $GraftX = (_1, _2, _3) => ({ tag: "Graft", _1, _2, _3 });
  var $VDom = (tag, _1, _2, _3, _4) => ({ tag, _1, _2, _3, _4 });
  var runGraft = (x) => {
    const go = (v2) => {
      if (v2.tag === "Text") {
        return $VDom("Text", v2._1);
      }
      if (v2.tag === "Elem") {
        return $VDom("Elem", v2._1, v2._2, x._1(v2._3), arrayMap(go)(v2._4));
      }
      if (v2.tag === "Keyed") {
        return $VDom("Keyed", v2._1, v2._2, x._1(v2._3), arrayMap((m) => $Tuple(m._1, go(m._2)))(v2._4));
      }
      if (v2.tag === "Widget") {
        return $VDom("Widget", x._2(v2._1));
      }
      if (v2.tag === "Grafted") {
        const $0 = v2._1;
        return $VDom("Grafted", $GraftX((x$1) => x._1($0._1(x$1)), (x$1) => x._2($0._2(x$1)), $0._3));
      }
      fail();
    };
    return go(x._3);
  };

  // output-es/Halogen.HTML.Properties/index.js
  var rows = /* @__PURE__ */ Property("rows");
  var selected = /* @__PURE__ */ Property("selected");
  var name3 = /* @__PURE__ */ Property("name");
  var id2 = /* @__PURE__ */ Property("id");
  var $$for = /* @__PURE__ */ Property("htmlFor");
  var checked2 = /* @__PURE__ */ Property("checked");
  var style = /* @__PURE__ */ Attribute(Nothing)("style");

  // output-es/Control.Applicative.Free/index.js
  var $FreeAp = (tag, _1, _2) => ({ tag, _1, _2 });
  var identity10 = (x) => x;
  var Pure = (value0) => $FreeAp("Pure", value0);
  var goLeft = (goLeft$a0$copy) => (goLeft$a1$copy) => (goLeft$a2$copy) => (goLeft$a3$copy) => (goLeft$a4$copy) => (goLeft$a5$copy) => {
    let goLeft$a0 = goLeft$a0$copy;
    let goLeft$a1 = goLeft$a1$copy;
    let goLeft$a2 = goLeft$a2$copy;
    let goLeft$a3 = goLeft$a3$copy;
    let goLeft$a4 = goLeft$a4$copy;
    let goLeft$a5 = goLeft$a5$copy;
    let goLeft$c = true;
    let goLeft$r;
    while (goLeft$c) {
      const dictApplicative = goLeft$a0, fStack = goLeft$a1, valStack = goLeft$a2, nat = goLeft$a3, func = goLeft$a4, count = goLeft$a5;
      if (func.tag === "Pure") {
        goLeft$c = false;
        goLeft$r = $Tuple($List("Cons", { func: dictApplicative.pure(func._1), count }, fStack), valStack);
        continue;
      }
      if (func.tag === "Lift") {
        goLeft$c = false;
        goLeft$r = $Tuple($List("Cons", { func: nat(func._1), count }, fStack), valStack);
        continue;
      }
      if (func.tag === "Ap") {
        goLeft$a0 = dictApplicative;
        goLeft$a1 = fStack;
        goLeft$a2 = $NonEmpty(func._2, $List("Cons", valStack._1, valStack._2));
        goLeft$a3 = nat;
        goLeft$a4 = func._1;
        goLeft$a5 = count + 1 | 0;
        continue;
      }
      fail();
    }
    return goLeft$r;
  };
  var goApply = (goApply$a0$copy) => (goApply$a1$copy) => (goApply$a2$copy) => (goApply$a3$copy) => {
    let goApply$a0 = goApply$a0$copy, goApply$a1 = goApply$a1$copy, goApply$a2 = goApply$a2$copy, goApply$a3 = goApply$a3$copy, goApply$c = true, goApply$r;
    while (goApply$c) {
      const dictApplicative = goApply$a0, fStack = goApply$a1, vals = goApply$a2, gVal = goApply$a3;
      if (fStack.tag === "Nil") {
        goApply$c = false;
        goApply$r = $Either("Left", gVal);
        continue;
      }
      if (fStack.tag === "Cons") {
        const gRes = dictApplicative.Apply0().apply(fStack._1.func)(gVal);
        if (fStack._1.count === 1) {
          if (fStack._2.tag === "Nil") {
            goApply$c = false;
            goApply$r = $Either("Left", gRes);
            continue;
          }
          goApply$a0 = dictApplicative;
          goApply$a1 = fStack._2;
          goApply$a2 = vals;
          goApply$a3 = gRes;
          continue;
        }
        if (vals.tag === "Nil") {
          goApply$c = false;
          goApply$r = $Either("Left", gRes);
          continue;
        }
        if (vals.tag === "Cons") {
          goApply$c = false;
          goApply$r = $Either(
            "Right",
            $Tuple($List("Cons", { func: gRes, count: fStack._1.count - 1 | 0 }, fStack._2), $NonEmpty(vals._1, vals._2))
          );
          continue;
        }
      }
      fail();
    }
    return goApply$r;
  };
  var functorFreeAp = { map: (f) => (x) => $FreeAp("Ap", $FreeAp("Pure", f), x) };
  var foldFreeAp = (dictApplicative) => (nat) => (z) => {
    const go = (go$a0$copy) => {
      let go$a0 = go$a0$copy, go$c = true, go$r;
      while (go$c) {
        const v = go$a0;
        if (v._2._1.tag === "Pure") {
          const v1 = goApply(dictApplicative)(v._1)(v._2._2)(dictApplicative.pure(v._2._1._1));
          if (v1.tag === "Left") {
            go$c = false;
            go$r = v1._1;
            continue;
          }
          if (v1.tag === "Right") {
            go$a0 = v1._1;
            continue;
          }
          fail();
        }
        if (v._2._1.tag === "Lift") {
          const v1 = goApply(dictApplicative)(v._1)(v._2._2)(nat(v._2._1._1));
          if (v1.tag === "Left") {
            go$c = false;
            go$r = v1._1;
            continue;
          }
          if (v1.tag === "Right") {
            go$a0 = v1._1;
            continue;
          }
          fail();
        }
        if (v._2._1.tag === "Ap") {
          go$a0 = goLeft(dictApplicative)(v._1)($NonEmpty(v._2._1._2, v._2._2))(nat)(v._2._1._1)(1);
          continue;
        }
        fail();
      }
      return go$r;
    };
    return go($Tuple(Nil, $NonEmpty(z, Nil)));
  };
  var applyFreeAp = { apply: (fba) => (fb) => $FreeAp("Ap", fba, fb), Functor0: () => functorFreeAp };
  var applicativeFreeAp = { pure: Pure, Apply0: () => applyFreeAp };

  // output-es/Halogen.Data.OrdBox/index.js
  var $OrdBox = (_1, _2, _3) => ({ tag: "OrdBox", _1, _2, _3 });
  var OrdBox = (value0) => (value1) => (value22) => $OrdBox(value0, value1, value22);
  var eqOrdBox = { eq: (v) => (v1) => v._1(v._3)(v1._3) };
  var ordOrdBox = { compare: (v) => (v1) => v._2(v._3)(v1._3), Eq0: () => eqOrdBox };

  // output-es/Halogen.Data.Slot/index.js
  var ordTuple2 = /* @__PURE__ */ ordTuple(ordString)(ordOrdBox);
  var pop1 = /* @__PURE__ */ pop(ordTuple2);
  var pop2 = () => (dictIsSymbol) => (dictOrd) => {
    const mkOrdBox = OrdBox(dictOrd.Eq0().eq)(dictOrd.compare);
    return (sym) => (key) => (v) => pop1($Tuple(dictIsSymbol.reflectSymbol(sym), mkOrdBox(key)))(v);
  };
  var lookup2 = () => (dictIsSymbol) => (dictOrd) => {
    const mkOrdBox = OrdBox(dictOrd.Eq0().eq)(dictOrd.compare);
    return (sym) => (key) => (v) => lookup(ordTuple2)($Tuple(dictIsSymbol.reflectSymbol(sym), mkOrdBox(key)))(v);
  };
  var insert3 = () => (dictIsSymbol) => (dictOrd) => {
    const mkOrdBox = OrdBox(dictOrd.Eq0().eq)(dictOrd.compare);
    return (sym) => (key) => (val) => (v) => insert(ordTuple2)($Tuple(dictIsSymbol.reflectSymbol(sym), mkOrdBox(key)))(val)(v);
  };
  var foreachSlot = (dictApplicative) => {
    const traverse_7 = traverse_(dictApplicative)(foldableMap);
    return (v) => (k) => traverse_7((x) => k(x))(v);
  };

  // output-es/Halogen.Query.ChildQuery/index.js
  var $ChildQuery = (_1, _2, _3) => ({ tag: "ChildQuery", _1, _2, _3 });

  // output-es/Halogen.Query.HalogenM/index.js
  var $HalogenF = (tag, _1, _2) => ({ tag, _1, _2 });
  var identity11 = (x) => x;
  var SubscriptionId = (x) => x;
  var ForkId = (x) => x;
  var query = () => (dictIsSymbol) => (dictOrd) => {
    const lookup22 = lookup2()(dictIsSymbol)(dictOrd);
    return (label) => (p) => (q) => $Free(
      $FreeView(
        "Bind",
        $HalogenF(
          "ChildQuery",
          $ChildQuery(
            (dictApplicative) => (k) => {
              const $0 = dictApplicative.pure(Nothing);
              const $1 = lookup22(label)(p);
              return (x) => {
                const $2 = $1(x);
                if ($2.tag === "Nothing") {
                  return $0;
                }
                if ($2.tag === "Just") {
                  return k($2._1);
                }
                fail();
              };
            },
            q,
            identity11
          )
        ),
        (x) => $Free($FreeView("Return", x), CatNil)
      ),
      CatNil
    );
  };
  var monadEffectHalogenM = (dictMonadEffect) => ({
    liftEffect: (x) => $Free(
      $FreeView(
        "Bind",
        $HalogenF("Lift", dictMonadEffect.liftEffect(x)),
        (x$1) => $Free($FreeView("Return", x$1), CatNil)
      ),
      CatNil
    ),
    Monad0: () => freeMonad
  });
  var monadAffHalogenM = (dictMonadAff) => {
    const monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
    return {
      liftAff: (x) => $Free(
        $FreeView(
          "Bind",
          $HalogenF("Lift", dictMonadAff.liftAff(x)),
          (x$1) => $Free($FreeView("Return", x$1), CatNil)
        ),
        CatNil
      ),
      MonadEffect0: () => monadEffectHalogenM1
    };
  };

  // output-es/CLI.Halogen.Command/index.js
  var $Action = (tag, _1, _2) => ({ tag, _1, _2 });
  var $Query = (_1) => ({ tag: "GetInput", _1 });
  var value2 = /* @__PURE__ */ Property("value");
  var $$get = /* @__PURE__ */ $Free(
    /* @__PURE__ */ $FreeView(
      "Bind",
      /* @__PURE__ */ $HalogenF("State", (s) => $Tuple(s, s)),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var modify_ = (f) => $Free(
    $FreeView(
      "Bind",
      $HalogenF("State", (s) => $Tuple(void 0, f(s))),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var GetInput = (value0) => $Query(value0);
  var UpdateFileSystem = (value0) => (value1) => $Action("UpdateFileSystem", value0, value1);
  var renderFilesystemFieldset = /* @__PURE__ */ (() => {
    const go = (z$p, m$p) => {
      if (m$p.tag === "Leaf") {
        return z$p;
      }
      if (m$p.tag === "Node") {
        return go(
          [
            ...go(z$p, m$p._5),
            $VDom("Elem", Nothing, "label", [$$for(m$p._3)], [$VDom("Text", m$p._3)]),
            $VDom(
              "Elem",
              Nothing,
              "textarea",
              [
                addForeignPropHandler("input")("value")(readString(monadIdentity))(UpdateFileSystem(m$p._3)),
                id2(m$p._3),
                rows(10),
                value2(m$p._4)
              ],
              []
            )
          ],
          m$p._6
        );
      }
      fail();
    };
    return (x) => $VDom(
      "Elem",
      Nothing,
      "fieldset",
      [],
      go([$VDom("Elem", Nothing, "legend", [], [$VDom("Text", "Filesystem")])], x)
    );
  })();
  var renderField = (name4) => (value1) => (toAction) => $VDom(
    "Elem",
    Nothing,
    "div",
    [],
    [
      $VDom("Elem", Nothing, "label", [$$for(name4)], [$VDom("Text", name4)]),
      $VDom(
        "Elem",
        Nothing,
        "input",
        [
          id2(name4),
          name3(name4),
          value2(value1),
          addForeignPropHandler("change")("value")(readString(monadIdentity))((x) => $Action("UpdateOption", toAction(x)))
        ],
        []
      )
    ]
  );
  var handleQuery = (v) => $Free(
    $$get._1,
    snoc2($$get._2)((x) => $Free($FreeView("Return", $Maybe("Just", v._1(x))), CatNil))
  );
  var handleAction = (handleOptionAction4) => (v) => {
    if (v.tag === "UpdateFileSystem") {
      const $0 = v._2;
      const $1 = v._1;
      return modify_((st) => ({ ...st, filesystem: insert(ordString)($1)($0)(st.filesystem) }));
    }
    if (v.tag === "UpdateOption") {
      return handleOptionAction4(v._1);
    }
    fail();
  };

  // output-es/Halogen.VDom.DOM/index.js
  var haltWidget = (v) => {
    const $0 = v.widget;
    return halt($0);
  };
  var patchWidget = (state, vdom) => {
    if (vdom.tag === "Grafted") {
      const $0 = runGraft(vdom._1);
      return patchWidget(state, $0);
    }
    if (vdom.tag === "Widget") {
      const $0 = vdom._1;
      const res = step(state.widget, $0);
      return $Step$p(res._1, { build: state.build, widget: res }, patchWidget, haltWidget);
    }
    haltWidget(state);
    return state.build(vdom);
  };
  var haltText = (v) => {
    const $0 = v.node;
    const parent2 = parentNode($0);
    return removeChild($0, parent2);
  };
  var patchText = (state, vdom) => {
    if (vdom.tag === "Grafted") {
      const $0 = runGraft(vdom._1);
      return patchText(state, $0);
    }
    if (vdom.tag === "Text") {
      if (state.value === vdom._1) {
        return $Step$p(state.node, state, patchText, haltText);
      }
      const $0 = vdom._1;
      setTextContent($0, state.node);
      return $Step$p(state.node, { build: state.build, node: state.node, value: $0 }, patchText, haltText);
    }
    haltText(state);
    return state.build(vdom);
  };
  var haltKeyed = (v) => {
    const $0 = v.attrs;
    const $1 = v.children;
    const $2 = v.node;
    const parent2 = parentNode($2);
    removeChild($2, parent2);
    forInE($1, (v1, s) => halt(s));
    return halt($0);
  };
  var haltElem = (v) => {
    const $0 = v.attrs;
    const $1 = v.children;
    const $2 = v.node;
    const parent2 = parentNode($2);
    removeChild($2, parent2);
    forEachE($1, halt);
    return halt($0);
  };
  var eqElemSpec = (ns1, v, ns2, v1) => v === v1 && (ns1.tag === "Just" ? ns2.tag === "Just" && ns1._1 === ns2._1 : ns1.tag === "Nothing" && ns2.tag === "Nothing");
  var patchElem = (state, vdom) => {
    if (vdom.tag === "Grafted") {
      const $0 = runGraft(vdom._1);
      return patchElem(state, $0);
    }
    if (vdom.tag === "Elem" && eqElemSpec(state.ns, state.name, vdom._1, vdom._2)) {
      if (state.children.length === 0 && vdom._4.length === 0) {
        const attrs22 = step(state.attrs, vdom._3);
        return $Step$p(
          state.node,
          { build: state.build, node: state.node, attrs: attrs22, ns: vdom._1, name: vdom._2, children: state.children },
          patchElem,
          haltElem
        );
      }
      const children2 = diffWithIxE(
        state.children,
        vdom._4,
        (ix, s, v2) => {
          const res = step(s, v2);
          insertChildIx(ix, res._1, state.node);
          return res;
        },
        (v2, s) => halt(s),
        (ix, v2) => {
          const res = state.build(v2);
          insertChildIx(ix, res._1, state.node);
          return res;
        }
      );
      const attrs2 = step(state.attrs, vdom._3);
      return $Step$p(state.node, { build: state.build, node: state.node, attrs: attrs2, ns: vdom._1, name: vdom._2, children: children2 }, patchElem, haltElem);
    }
    haltElem(state);
    return state.build(vdom);
  };
  var patchKeyed = (state, vdom) => {
    if (vdom.tag === "Grafted") {
      const $0 = runGraft(vdom._1);
      return patchKeyed(state, $0);
    }
    if (vdom.tag === "Keyed" && eqElemSpec(state.ns, state.name, vdom._1, vdom._2)) {
      const v = vdom._4.length;
      if (state.length === 0 && v === 0) {
        const attrs22 = step(state.attrs, vdom._3);
        return $Step$p(
          state.node,
          { build: state.build, node: state.node, attrs: attrs22, ns: vdom._1, name: vdom._2, children: state.children, length: 0 },
          patchKeyed,
          haltKeyed
        );
      }
      const children2 = diffWithKeyAndIxE(
        state.children,
        vdom._4,
        fst,
        (v2, ix$p, s, v3) => {
          const $0 = v3._2;
          const res = step(s, $0);
          insertChildIx(ix$p, res._1, state.node);
          return res;
        },
        (v2, s) => halt(s),
        (v2, ix, v3) => {
          const $0 = v3._2;
          const res = state.build($0);
          insertChildIx(ix, res._1, state.node);
          return res;
        }
      );
      const attrs2 = step(state.attrs, vdom._3);
      return $Step$p(
        state.node,
        { build: state.build, node: state.node, attrs: attrs2, ns: vdom._1, name: vdom._2, children: children2, length: v },
        patchKeyed,
        haltKeyed
      );
    }
    haltKeyed(state);
    return state.build(vdom);
  };
  var buildWidget = (v, build, w) => {
    const res = v.buildWidget(v)(w);
    return $Step$p(res._1, { build, widget: res }, patchWidget, haltWidget);
  };
  var buildText = (v, build, s) => {
    const $0 = v.document;
    const node = createTextNode(s, $0);
    return $Step$p(node, { build, node, value: s }, patchText, haltText);
  };
  var buildKeyed = (v, build, ns1, name1, as1, ch1) => {
    const $0 = (() => {
      if (ns1.tag === "Nothing") {
        return nullImpl;
      }
      if (ns1.tag === "Just") {
        return notNull(ns1._1);
      }
      fail();
    })();
    const $1 = v.document;
    const el = createElement($0, name1, $1);
    const children2 = strMapWithIxE(
      ch1,
      fst,
      (v1, ix, v2) => {
        const $2 = v2._2;
        const res = build($2);
        insertChildIx(ix, res._1, el);
        return res;
      }
    );
    const attrs = v.buildAttributes(el)(as1);
    return $Step$p(el, { build, node: el, attrs, ns: ns1, name: name1, children: children2, length: ch1.length }, patchKeyed, haltKeyed);
  };
  var buildElem = (v, build, ns1, name1, as1, ch1) => {
    const $0 = (() => {
      if (ns1.tag === "Nothing") {
        return nullImpl;
      }
      if (ns1.tag === "Just") {
        return notNull(ns1._1);
      }
      fail();
    })();
    const $1 = v.document;
    const el = createElement($0, name1, $1);
    const children2 = forE2(
      ch1,
      (ix, child) => {
        const res = build(child);
        insertChildIx(ix, res._1, el);
        return res;
      }
    );
    const attrs = v.buildAttributes(el)(as1);
    return $Step$p(el, { build, node: el, attrs, ns: ns1, name: name1, children: children2 }, patchElem, haltElem);
  };
  var buildVDom = (spec) => {
    const build = (v) => {
      if (v.tag === "Text") {
        const $0 = v._1;
        return buildText(spec, build, $0);
      }
      if (v.tag === "Elem") {
        const $0 = v._3;
        const $1 = v._4;
        const $2 = v._2;
        const $3 = v._1;
        return buildElem(spec, build, $3, $2, $0, $1);
      }
      if (v.tag === "Keyed") {
        const $0 = v._3;
        const $1 = v._4;
        const $2 = v._2;
        const $3 = v._1;
        return buildKeyed(spec, build, $3, $2, $0, $1);
      }
      if (v.tag === "Widget") {
        const $0 = v._1;
        return buildWidget(spec, build, $0);
      }
      if (v.tag === "Grafted") {
        const $0 = runGraft(v._1);
        return build($0);
      }
      fail();
    };
    return build;
  };

  // output-es/Halogen.VDom.Thunk/index.js
  var unsafeEqThunk = (v, $0) => refEq2(v._1, $0._1) && refEq2(v._2, $0._2) && v._2(v._4, $0._4);
  var buildThunk = (toVDom) => {
    const patchThunk = (state, t2) => {
      if (unsafeEqThunk(state.thunk, t2)) {
        const $02 = $Step$p(
          state.vdom._1,
          state,
          patchThunk,
          (state$1) => {
            const $03 = state$1.vdom;
            return halt($03);
          }
        );
        return $02;
      }
      const $0 = toVDom(t2._3(t2._4));
      const vdom = step(state.vdom, $0);
      return $Step$p(
        vdom._1,
        { vdom, thunk: t2 },
        patchThunk,
        (state$1) => {
          const $1 = state$1.vdom;
          return halt($1);
        }
      );
    };
    return (spec) => (t) => {
      const $0 = toVDom(t._3(t._4));
      const vdom = buildVDom(spec)($0);
      return $Step$p(
        vdom._1,
        { thunk: t, vdom },
        patchThunk,
        (state) => {
          const $1 = state.vdom;
          return halt($1);
        }
      );
    };
  };

  // output-es/Halogen.Component/index.js
  var $ComponentSlot = (tag, _1) => ({ tag, _1 });
  var traverse_2 = /* @__PURE__ */ traverse_(freeApplicative)(foldableMaybe);
  var mkEval = (args) => (v) => {
    if (v.tag === "Initialize") {
      const $0 = v._1;
      const $1 = traverse_2(args.handleAction)(args.initialize);
      return $Free($1._1, snoc2($1._2)((x) => $Free($FreeView("Return", $0), CatNil)));
    }
    if (v.tag === "Finalize") {
      const $0 = v._1;
      const $1 = traverse_2(args.handleAction)(args.finalize);
      return $Free($1._1, snoc2($1._2)((x) => $Free($FreeView("Return", $0), CatNil)));
    }
    if (v.tag === "Receive") {
      const $0 = v._2;
      const $1 = traverse_2(args.handleAction)(args.receive(v._1));
      return $Free($1._1, snoc2($1._2)((x) => $Free($FreeView("Return", $0), CatNil)));
    }
    if (v.tag === "Action") {
      const $0 = v._2;
      const $1 = args.handleAction(v._1);
      return $Free($1._1, snoc2($1._2)((x) => $Free($FreeView("Return", $0), CatNil)));
    }
    if (v.tag === "Query") {
      const $0 = v._2();
      const $1 = args.handleQuery(v._1._2);
      return $Free(
        $1._1,
        snoc2($1._2)((x) => $Free(
          $FreeView(
            "Return",
            (() => {
              if (x.tag === "Nothing") {
                return $0;
              }
              if (x.tag === "Just") {
                return v._1._1(x._1);
              }
              fail();
            })()
          ),
          CatNil
        ))
      );
    }
    fail();
  };
  var defaultEval = {
    handleAction: (v) => $Free($FreeView("Return", void 0), CatNil),
    handleQuery: (v) => $Free($FreeView("Return", Nothing), CatNil),
    receive: (v) => Nothing,
    initialize: Nothing,
    finalize: Nothing
  };
  var componentSlot = () => (dictIsSymbol) => (dictOrd) => {
    const lookup22 = lookup2()(dictIsSymbol)(dictOrd);
    const pop22 = pop2()(dictIsSymbol)(dictOrd);
    const insert22 = insert3()(dictIsSymbol)(dictOrd);
    return (label) => (p) => (comp) => (input) => (output) => ({ get: lookup22(label)(p), pop: pop22(label)(p), set: insert22(label)(p), component: comp, input, output });
  };

  // output-es/CLI.Halogen.CompatOptions/index.js
  var $OptionAction = (tag, _1) => ({ tag, _1 });
  var fromFoldable8 = /* @__PURE__ */ fromFoldable3(ordString)(foldableArray);
  var modify_2 = (f) => $Free(
    $FreeView(
      "Bind",
      $HalogenF("State", (s) => $Tuple(void 0, f(s))),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var UpdateLeftSchemaFilePath = (value0) => $OptionAction("UpdateLeftSchemaFilePath", value0);
  var UpdateRightSchemaFilePath = (value0) => $OptionAction("UpdateRightSchemaFilePath", value0);
  var render5 = (v) => $VDom(
    "Elem",
    Nothing,
    "div",
    [],
    [
      $VDom(
        "Elem",
        Nothing,
        "fieldset",
        [],
        [
          $VDom("Elem", Nothing, "legend", [], [$VDom("Text", "Command Options")]),
          renderField("left schema file path")(v.options.leftSchemaFilePath)(UpdateLeftSchemaFilePath),
          renderField("right schema file path")(v.options.rightSchemaFilePath)(UpdateRightSchemaFilePath)
        ]
      ),
      renderFilesystemFieldset(v.filesystem)
    ]
  );
  var initialState = (v) => ({
    filesystem: fromFoldable8([
      $Tuple(
        "schemata/left.json",
        stringifyWithIndent(2)(print($JsonSchema(
          "ObjectSchema",
          {
            ...defaultKeywords,
            typeKeyword: $Maybe("Just", $$$Map("Node", 1, 1, JsonInteger, void 0, Leaf2, Leaf2))
          }
        )))
      ),
      $Tuple(
        "schemata/right.json",
        stringifyWithIndent(2)(print($JsonSchema(
          "ObjectSchema",
          {
            ...defaultKeywords,
            typeKeyword: $Maybe("Just", $$$Map("Node", 1, 1, JsonString, void 0, Leaf2, Leaf2))
          }
        )))
      )
    ]),
    options: { leftSchemaFilePath: "schemata/left.json", rightSchemaFilePath: "schemata/right.json" }
  });
  var handleOptionAction = (v) => {
    if (v.tag === "UpdateLeftSchemaFilePath") {
      const $0 = v._1;
      return modify_2((st) => ({ ...st, options: { ...st.options, leftSchemaFilePath: $0 } }));
    }
    if (v.tag === "UpdateRightSchemaFilePath") {
      const $0 = v._1;
      return modify_2((st) => ({ ...st, options: { ...st.options, rightSchemaFilePath: $0 } }));
    }
    fail();
  };
  var component = {
    eval: /* @__PURE__ */ mkEval({
      ...defaultEval,
      handleAction: /* @__PURE__ */ handleAction(handleOptionAction),
      handleQuery
    }),
    initialState,
    render: render5
  };

  // output-es/CLI.Halogen.DiffOptions/index.js
  var $OptionAction2 = (tag, _1) => ({ tag, _1 });
  var fromFoldable9 = /* @__PURE__ */ fromFoldable3(ordString)(foldableArray);
  var modify_3 = (f) => $Free(
    $FreeView(
      "Bind",
      $HalogenF("State", (s) => $Tuple(void 0, f(s))),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var UpdateLeftSchemaFilePath2 = (value0) => $OptionAction2("UpdateLeftSchemaFilePath", value0);
  var UpdateRightSchemaFilePath2 = (value0) => $OptionAction2("UpdateRightSchemaFilePath", value0);
  var render6 = (v) => $VDom(
    "Elem",
    Nothing,
    "div",
    [],
    [
      $VDom(
        "Elem",
        Nothing,
        "fieldset",
        [],
        [
          $VDom("Elem", Nothing, "legend", [], [$VDom("Text", "Command Options")]),
          renderField("left schema file path")(v.options.leftSchemaFilePath)(UpdateLeftSchemaFilePath2),
          renderField("right schema file path")(v.options.rightSchemaFilePath)(UpdateRightSchemaFilePath2)
        ]
      ),
      renderFilesystemFieldset(v.filesystem)
    ]
  );
  var initialState2 = (v) => ({
    filesystem: fromFoldable9([
      $Tuple(
        "schemata/left.json",
        stringifyWithIndent(2)(print($JsonSchema(
          "ObjectSchema",
          {
            ...defaultKeywords,
            typeKeyword: $Maybe("Just", $$$Map("Node", 1, 1, JsonInteger, void 0, Leaf2, Leaf2))
          }
        )))
      ),
      $Tuple(
        "schemata/right.json",
        stringifyWithIndent(2)(print($JsonSchema(
          "ObjectSchema",
          {
            ...defaultKeywords,
            typeKeyword: $Maybe("Just", $$$Map("Node", 1, 1, JsonString, void 0, Leaf2, Leaf2))
          }
        )))
      )
    ]),
    options: { leftSchemaFilePath: "schemata/left.json", rightSchemaFilePath: "schemata/right.json" }
  });
  var handleOptionAction2 = (v) => {
    if (v.tag === "UpdateLeftSchemaFilePath") {
      const $0 = v._1;
      return modify_3((st) => ({ ...st, options: { ...st.options, leftSchemaFilePath: $0 } }));
    }
    if (v.tag === "UpdateRightSchemaFilePath") {
      const $0 = v._1;
      return modify_3((st) => ({ ...st, options: { ...st.options, rightSchemaFilePath: $0 } }));
    }
    fail();
  };
  var component2 = {
    eval: /* @__PURE__ */ mkEval({
      ...defaultEval,
      handleAction: /* @__PURE__ */ handleAction(handleOptionAction2),
      handleQuery
    }),
    initialState: initialState2,
    render: render6
  };

  // output-es/CLI.Program/index.js
  var $OutputFormat = (tag) => tag;
  var Json2 = /* @__PURE__ */ $OutputFormat("Json");
  var Markdown = /* @__PURE__ */ $OutputFormat("Markdown");

  // output-es/CLI.Halogen.OutputFormat/index.js
  var $Action2 = (_1) => ({ tag: "OutputFormatUpdated", _1 });
  var $Query2 = (_1) => ({ tag: "GetOutputFormat", _1 });
  var type_3 = /* @__PURE__ */ (() => {
    const $0 = Property("type");
    return (x) => $0(renderInputType(x));
  })();
  var value3 = /* @__PURE__ */ Property("value");
  var $$get2 = /* @__PURE__ */ $Free(
    /* @__PURE__ */ $FreeView(
      "Bind",
      /* @__PURE__ */ $HalogenF("State", (s) => $Tuple(s, s)),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var modify_4 = (f) => $Free(
    $FreeView(
      "Bind",
      $HalogenF("State", (s) => $Tuple(void 0, f(s))),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var GetOutputFormat = (value0) => $Query2(value0);
  var render7 = (v) => {
    const $0 = v.outputFormat;
    const renderOutputFormatFieldFor = (format) => $VDom(
      "Elem",
      Nothing,
      "div",
      [],
      [
        $VDom(
          "Elem",
          Nothing,
          "label",
          [
            $$for((() => {
              if (format === "Json") {
                return "JSON";
              }
              if (format === "Markdown") {
                return "Markdown";
              }
              fail();
            })())
          ],
          [
            $VDom(
              "Text",
              (() => {
                if (format === "Json") {
                  return "JSON";
                }
                if (format === "Markdown") {
                  return "Markdown";
                }
                fail();
              })()
            )
          ]
        ),
        $VDom(
          "Elem",
          Nothing,
          "input",
          [
            type_3(InputRadio),
            name3("outputFormat"),
            id2((() => {
              if (format === "Json") {
                return "JSON";
              }
              if (format === "Markdown") {
                return "Markdown";
              }
              fail();
            })()),
            value3((() => {
              if (format === "Json") {
                return "JSON";
              }
              if (format === "Markdown") {
                return "Markdown";
              }
              fail();
            })()),
            checked2(format === "Json" ? $0 === "Json" : format === "Markdown" && $0 === "Markdown"),
            $Prop("Handler", "click", (ev) => $Maybe("Just", $Input("Action", $Action2(format))))
          ],
          []
        )
      ]
    );
    return $VDom(
      "Elem",
      Nothing,
      "fieldset",
      [],
      [
        $VDom("Elem", Nothing, "legend", [], [$VDom("Text", "Output Format")]),
        renderOutputFormatFieldFor(Json2),
        renderOutputFormatFieldFor(Markdown)
      ]
    );
  };
  var handleQuery2 = (v) => $Free(
    $$get2._1,
    snoc2($$get2._2)((state) => $Free(
      $FreeView("Return", $Maybe("Just", v._1(state.outputFormat))),
      CatNil
    ))
  );
  var handleAction2 = (v) => {
    const $0 = v._1;
    return modify_4((st) => ({ ...st, outputFormat: $0 }));
  };
  var component3 = {
    eval: /* @__PURE__ */ mkEval({ ...defaultEval, handleAction: handleAction2, handleQuery: handleQuery2 }),
    initialState: (v) => ({ outputFormat: Json2 }),
    render: render7
  };

  // output-es/CLI.Halogen.ValidateOptions/index.js
  var $OptionAction3 = (tag, _1) => ({ tag, _1 });
  var fromFoldable10 = /* @__PURE__ */ fromFoldable3(ordString)(foldableArray);
  var modify_5 = (f) => $Free(
    $FreeView(
      "Bind",
      $HalogenF("State", (s) => $Tuple(void 0, f(s))),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var UpdateJsonFilePath = (value0) => $OptionAction3("UpdateJsonFilePath", value0);
  var UpdateSchemaFilePath = (value0) => $OptionAction3("UpdateSchemaFilePath", value0);
  var render8 = (v) => $VDom(
    "Elem",
    Nothing,
    "div",
    [],
    [
      $VDom(
        "Elem",
        Nothing,
        "fieldset",
        [],
        [
          $VDom("Elem", Nothing, "legend", [], [$VDom("Text", "Command Options")]),
          renderField("schema file path")(v.options.schemaFilePath)(UpdateSchemaFilePath),
          renderField("json file path")(v.options.jsonFilePath)(UpdateJsonFilePath)
        ]
      ),
      renderFilesystemFieldset(v.filesystem)
    ]
  );
  var initialState3 = (v) => ({
    filesystem: fromFoldable10([
      $Tuple("value.json", stringifyWithIndent(2)(id(123))),
      $Tuple(
        "schema.json",
        stringifyWithIndent(2)(print($JsonSchema(
          "ObjectSchema",
          {
            ...defaultKeywords,
            typeKeyword: $Maybe("Just", $$$Map("Node", 1, 1, JsonString, void 0, Leaf2, Leaf2))
          }
        )))
      )
    ]),
    options: { jsonFilePath: "value.json", schemaFilePath: "schema.json" }
  });
  var handleOptionAction3 = (v) => {
    if (v.tag === "UpdateJsonFilePath") {
      const $0 = v._1;
      return modify_5((st) => ({ ...st, options: { ...st.options, jsonFilePath: $0 } }));
    }
    if (v.tag === "UpdateSchemaFilePath") {
      const $0 = v._1;
      return modify_5((st) => ({ ...st, options: { ...st.options, schemaFilePath: $0 } }));
    }
    fail();
  };
  var component4 = {
    eval: /* @__PURE__ */ mkEval({
      ...defaultEval,
      handleAction: /* @__PURE__ */ handleAction(handleOptionAction3),
      handleQuery
    }),
    initialState: initialState3,
    render: render8
  };

  // output-es/Halogen.HTML/index.js
  var slot_ = () => (dictIsSymbol) => (dictOrd) => {
    const componentSlot2 = componentSlot()(dictIsSymbol)(dictOrd);
    return (label) => (p) => (component6) => (input) => $VDom(
      "Widget",
      $ComponentSlot("ComponentSlot", componentSlot2(label)(p)(component6)(input)((v) => Nothing))
    );
  };
  var slot = () => (dictIsSymbol) => (dictOrd) => {
    const componentSlot2 = componentSlot()(dictIsSymbol)(dictOrd);
    return (label) => (p) => (component6) => (input) => (outputQuery) => $VDom(
      "Widget",
      $ComponentSlot("ComponentSlot", componentSlot2(label)(p)(component6)(input)((x) => $Maybe("Just", outputQuery(x))))
    );
  };

  // output-es/Web.HTML.HTMLElement/foreign.js
  function _read(nothing, just, value4) {
    var tag = Object.prototype.toString.call(value4);
    if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
      return just(value4);
    } else {
      return nothing;
    }
  }

  // output-es/Halogen.Query/index.js
  var identity12 = (x) => x;
  var request = () => (dictIsSymbol) => (dictOrd) => {
    const query2 = query()(dictIsSymbol)(dictOrd);
    return (slot3) => (label) => (req) => query2(slot3)(label)(req(identity12));
  };

  // output-es/Control.Monad.Error.Class/index.js
  var monadThrowEffect = { throwError: throwException, Monad0: () => monadEffect };
  var monadErrorEffect = { catchError: (b) => (a) => catchException(a)(b), MonadThrow0: () => monadThrowEffect };
  var $$try = (dictMonadError) => {
    const Monad0 = dictMonadError.MonadThrow0().Monad0();
    return (a) => dictMonadError.catchError(Monad0.Bind1().Apply0().Functor0().map(Right)(a))((x) => Monad0.Applicative0().pure($Either("Left", x)));
  };

  // output-es/Control.Monad.Reader.Trans/index.js
  var bindReaderT = (dictBind) => {
    const $0 = dictBind.Apply0();
    const $1 = $0.Functor0();
    const applyReaderT1 = /* @__PURE__ */ (() => {
      const functorReaderT1 = {
        map: (x) => {
          const $2 = $1.map(x);
          return (v) => (x$1) => $2(v(x$1));
        }
      };
      return { apply: (v) => (v1) => (r) => $0.apply(v(r))(v1(r)), Functor0: () => functorReaderT1 };
    })();
    return { bind: (v) => (k) => (r) => dictBind.bind(v(r))((a) => k(a)(r)), Apply0: () => applyReaderT1 };
  };
  var monadReaderT = (dictMonad) => {
    const $0 = dictMonad.Applicative0();
    const $1 = $0.Apply0();
    const applicativeReaderT1 = (() => {
      const $2 = $1.Functor0();
      const functorReaderT1 = {
        map: (x) => {
          const $3 = $2.map(x);
          return (v) => (x$1) => $3(v(x$1));
        }
      };
      const applyReaderT1 = { apply: (v) => (v1) => (r) => $1.apply(v(r))(v1(r)), Functor0: () => functorReaderT1 };
      return {
        pure: (x) => {
          const $3 = $0.pure(x);
          return (v) => $3;
        },
        Apply0: () => applyReaderT1
      };
    })();
    const bindReaderT1 = bindReaderT(dictMonad.Bind1());
    return { Applicative0: () => applicativeReaderT1, Bind1: () => bindReaderT1 };
  };
  var monadThrowReaderT = (dictMonadThrow) => {
    const monadReaderT1 = monadReaderT(dictMonadThrow.Monad0());
    return {
      throwError: (x) => {
        const $0 = dictMonadThrow.throwError(x);
        return (v) => $0;
      },
      Monad0: () => monadReaderT1
    };
  };
  var monadErrorReaderT = (dictMonadError) => {
    const monadThrowReaderT1 = monadThrowReaderT(dictMonadError.MonadThrow0());
    return { catchError: (v) => (h) => (r) => dictMonadError.catchError(v(r))((e) => h(e)(r)), MonadThrow0: () => monadThrowReaderT1 };
  };

  // output-es/DOM.HTML.Indexed.ButtonType/index.js
  var $ButtonType = (tag) => tag;
  var ButtonButton = /* @__PURE__ */ $ButtonType("ButtonButton");

  // output-es/Control.Parallel/index.js
  var identity13 = (x) => x;
  var parTraverse_ = (dictParallel) => (dictApplicative) => {
    const traverse_7 = traverse_(dictApplicative);
    return (dictFoldable) => {
      const traverse_14 = traverse_7(dictFoldable);
      return (f) => {
        const $0 = traverse_14((x) => dictParallel.parallel(f(x)));
        return (x) => dictParallel.sequential($0(x));
      };
    };
  };

  // output-es/Effect.Aff/foreign.js
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
    function nonCanceler2(error3) {
      return new Aff2(PURE, void 0);
    }
    function runEff(eff) {
      try {
        eff();
      } catch (error3) {
        setTimeout(function() {
          throw error3;
        }, 0);
      }
    }
    function runSync(left, right, eff) {
      try {
        return right(eff());
      } catch (error3) {
        return left(error3);
      }
    }
    function runAsync(left, eff, k) {
      try {
        return eff(k)();
      } catch (error3) {
        k(left(error3))();
        return nonCanceler2;
      }
    }
    var Scheduler = function() {
      var limit = 1024;
      var size4 = 0;
      var ix = 0;
      var queue = new Array(limit);
      var draining = false;
      function drain() {
        var thunk4;
        draining = true;
        while (size4 !== 0) {
          size4--;
          thunk4 = queue[ix];
          queue[ix] = void 0;
          ix = (ix + 1) % limit;
          thunk4();
        }
        draining = false;
      }
      return {
        isDraining: function() {
          return draining;
        },
        enqueue: function(cb) {
          var i, tmp;
          if (size4 === limit) {
            tmp = draining;
            drain();
            draining = tmp;
          }
          queue[(ix + size4) % limit] = cb;
          size4++;
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
            function kill(fid) {
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
                kill(k);
              }
            }
            fibers = {};
            fiberId = 0;
            count = 0;
            return function(error3) {
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
      function run2(localRunTick) {
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
                        run2(runTick);
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
                  step3 = sequential(util, supervisor, step3._1);
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
      function onComplete(join2) {
        return function() {
          if (status === COMPLETED) {
            rethrow = rethrow && join2.rethrow;
            join2.handler(step3)();
            return function() {
            };
          }
          var jid = joinId++;
          joins = joins || {};
          joins[jid] = join2;
          return function() {
            if (joins !== null) {
              delete joins[jid];
            }
          };
        };
      }
      function kill(error3, cb) {
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
              interrupt = util.left(error3);
              status = COMPLETED;
              step3 = interrupt;
              run2(runTick);
              break;
            case PENDING:
              if (interrupt === null) {
                interrupt = util.left(error3);
              }
              if (bracketCount === 0) {
                if (status === PENDING) {
                  attempts = new Aff2(CONS, new Aff2(FINALIZER, step3(error3)), attempts, interrupt);
                }
                status = RETURN;
                step3 = null;
                fail2 = null;
                run2(++runTick);
              }
              break;
            default:
              if (interrupt === null) {
                interrupt = util.left(error3);
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
      function join(cb) {
        return function() {
          var canceler = onComplete({
            rethrow: false,
            handler: cb
          })();
          if (status === SUSPENDED) {
            run2(runTick);
          }
          return canceler;
        };
      }
      return {
        kill,
        join,
        onComplete,
        isSuspended: function() {
          return status === SUSPENDED;
        },
        run: function() {
          if (status === SUSPENDED) {
            if (!Scheduler.isDraining()) {
              Scheduler.enqueue(function() {
                run2(runTick);
              });
            } else {
              run2(runTick);
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
      function kill(error3, par2, cb2) {
        var step3 = par2;
        var head = null;
        var tail = null;
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
                  kills2[count++] = tmp.kill(error3, function(result) {
                    return function() {
                      count--;
                      if (count === 0) {
                        cb2(result)();
                      }
                    };
                  });
                }
                if (head === null) {
                  break loop;
                }
                step3 = head._2;
                if (tail === null) {
                  head = null;
                } else {
                  head = tail._1;
                  tail = tail._2;
                }
                break;
              case MAP:
                step3 = step3._2;
                break;
              case APPLY:
              case ALT:
                if (head) {
                  tail = new Aff2(CONS, head, tail);
                }
                head = step3;
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
      function join(result, head, tail) {
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
            if (head === null) {
              cb(fail2 || step3)();
              return;
            }
            if (head._3 !== EMPTY) {
              return;
            }
            switch (head.tag) {
              case MAP:
                if (fail2 === null) {
                  head._3 = util.right(head._1(util.fromRight(step3)));
                  step3 = head._3;
                } else {
                  head._3 = fail2;
                }
                break;
              case APPLY:
                lhs = head._1._3;
                rhs = head._2._3;
                if (fail2) {
                  head._3 = fail2;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, fail2 === lhs ? head._2 : head._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail === null) {
                        join(fail2, null, null);
                      } else {
                        join(fail2, tail._1, tail._2);
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
                  head._3 = step3;
                }
                break;
              case ALT:
                lhs = head._1._3;
                rhs = head._2._3;
                if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                  return;
                }
                if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                  fail2 = step3 === lhs ? rhs : lhs;
                  step3 = null;
                  head._3 = fail2;
                } else {
                  head._3 = step3;
                  tmp = true;
                  kid = killId++;
                  kills[kid] = kill(early, step3 === lhs ? head._2 : head._1, function() {
                    return function() {
                      delete kills[kid];
                      if (tmp) {
                        tmp = false;
                      } else if (tail === null) {
                        join(step3, null, null);
                      } else {
                        join(step3, tail._1, tail._2);
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
            if (tail === null) {
              head = null;
            } else {
              head = tail._1;
              tail = tail._2;
            }
          }
      }
      function resolve(fiber) {
        return function(result) {
          return function() {
            delete fibers[fiber._1];
            fiber._3 = result;
            join(result, fiber._2._1, fiber._2._2);
          };
        };
      }
      function run2() {
        var status = CONTINUE;
        var step3 = par;
        var head = null;
        var tail = null;
        var tmp, fid;
        loop:
          while (true) {
            tmp = null;
            fid = null;
            switch (status) {
              case CONTINUE:
                switch (step3.tag) {
                  case MAP:
                    if (head) {
                      tail = new Aff2(CONS, head, tail);
                    }
                    head = new Aff2(MAP, step3._1, EMPTY, EMPTY);
                    step3 = step3._2;
                    break;
                  case APPLY:
                    if (head) {
                      tail = new Aff2(CONS, head, tail);
                    }
                    head = new Aff2(APPLY, EMPTY, step3._2, EMPTY);
                    step3 = step3._1;
                    break;
                  case ALT:
                    if (head) {
                      tail = new Aff2(CONS, head, tail);
                    }
                    head = new Aff2(ALT, EMPTY, step3._2, EMPTY);
                    step3 = step3._1;
                    break;
                  default:
                    fid = fiberId++;
                    status = RETURN;
                    tmp = step3;
                    step3 = new Aff2(FORKED, fid, new Aff2(CONS, head, tail), EMPTY);
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
                if (head === null) {
                  break loop;
                }
                if (head._1 === EMPTY) {
                  head._1 = step3;
                  status = CONTINUE;
                  step3 = head._2;
                  head._2 = EMPTY;
                } else {
                  head._2 = step3;
                  step3 = head;
                  if (tail === null) {
                    head = null;
                  } else {
                    head = tail._1;
                    tail = tail._2;
                  }
                }
            }
          }
        root = step3;
        for (fid = 0; fid < fiberId; fid++) {
          fibers[fid].run();
        }
      }
      function cancel(error3, cb2) {
        interrupt = util.left(error3);
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
        var newKills = kill(error3, root, cb2);
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
      run2();
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            return cancel(killError, killCb);
          };
        });
      };
    }
    function sequential(util, supervisor, par) {
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
        return Aff.Bind(aff, function(value4) {
          return Aff.Pure(f(value4));
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
    return function(options) {
      return function(k) {
        return Aff.Bracket(acquire, options, k);
      };
    };
  }
  function _makeFiber(util, aff) {
    return function() {
      return Aff.Fiber(util, null, aff);
    };
  }
  var _sequential = Aff.Seq;

  // output-es/Effect.Aff/index.js
  var functorParAff = { map: _parAffMap };
  var functorAff = { map: _map };
  var forkAff = /* @__PURE__ */ _fork(true);
  var ffiUtil = {
    isLeft: (v) => {
      if (v.tag === "Left") {
        return true;
      }
      if (v.tag === "Right") {
        return false;
      }
      fail();
    },
    fromLeft: (v) => {
      if (v.tag === "Left") {
        return v._1;
      }
      if (v.tag === "Right") {
        return _crashWith("unsafeFromLeft: Right");
      }
      fail();
    },
    fromRight: (v) => {
      if (v.tag === "Right") {
        return v._1;
      }
      if (v.tag === "Left") {
        return _crashWith("unsafeFromRight: Left");
      }
      fail();
    },
    left: Left,
    right: Right
  };
  var applyParAff = { apply: _parAffApply, Functor0: () => functorParAff };
  var monadAff = { Applicative0: () => applicativeAff, Bind1: () => bindAff };
  var bindAff = { bind: _bind, Apply0: () => applyAff };
  var applyAff = { apply: (f) => (a) => _bind(f)((f$p) => _bind(a)((a$p) => applicativeAff.pure(f$p(a$p)))), Functor0: () => functorAff };
  var applicativeAff = { pure: _pure, Apply0: () => applyAff };
  var $$finally = (fin) => (a) => generalBracket(_pure())({ killed: (v) => (v$1) => fin, failed: (v) => (v$1) => fin, completed: (v) => (v$1) => fin })((v) => a);
  var parallelAff = { parallel: unsafeCoerce, sequential: _sequential, Apply0: () => applyAff, Apply1: () => applyParAff };
  var applicativeParAff = { pure: (x) => _pure(x), Apply0: () => applyParAff };
  var monadEffectAff = { liftEffect: _liftEffect, Monad0: () => monadAff };
  var joinFiber = (v) => makeAff((k) => {
    const $0 = v.join(k);
    return () => {
      const a$p = $0();
      const $1 = _liftEffect(a$p);
      return (v$1) => $1;
    };
  });
  var killFiber = (e) => (v) => _bind(_liftEffect(v.isSuspended))((suspended) => {
    if (suspended) {
      return _liftEffect((() => {
        const $0 = v.kill(e, (v$1) => () => {
        });
        return () => {
          $0();
        };
      })());
    }
    return makeAff((k) => {
      const $0 = v.kill(e, k);
      return () => {
        const a$p = $0();
        const $1 = _liftEffect(a$p);
        return (v$1) => $1;
      };
    });
  });
  var monadThrowAff = { throwError: _throwError, Monad0: () => monadAff };
  var monadErrorAff = { catchError: _catchError, MonadThrow0: () => monadThrowAff };
  var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
  var runAff = (k) => (aff) => {
    const $0 = _makeFiber(ffiUtil, _bind($$try2(aff))((x) => _liftEffect(k(x))));
    return () => {
      const fiber = $0();
      fiber.run();
      return fiber;
    };
  };
  var runAff_ = (k) => (aff) => {
    const $0 = runAff(k)(aff);
    return () => {
      $0();
    };
  };
  var monadRecAff = {
    tailRecM: (k) => {
      const go = (a) => _bind(k(a))((res) => {
        if (res.tag === "Done") {
          return _pure(res._1);
        }
        if (res.tag === "Loop") {
          return go(res._1);
        }
        fail();
      });
      return go;
    },
    Monad0: () => monadAff
  };
  var nonCanceler = /* @__PURE__ */ (() => {
    const $0 = _pure();
    return (v) => $0;
  })();

  // output-es/Sandbox.Component/index.js
  var $Action3 = (tag, _1) => ({ tag, _1 });
  var $Output = () => ({ tag: "SelectedCommandChanged" });
  var $Query3 = (_1) => ({ tag: "RequestProgramExecution", _1 });
  var optionsFormIsSymbol = { reflectSymbol: () => "optionsForm" };
  var slot2 = /* @__PURE__ */ slot()(optionsFormIsSymbol)(ordUnit);
  var type_4 = /* @__PURE__ */ (() => {
    const $0 = Property("type");
    return (x) => $0((() => {
      if (x === "ButtonButton") {
        return "button";
      }
      if (x === "ButtonSubmit") {
        return "submit";
      }
      if (x === "ButtonReset") {
        return "reset";
      }
      fail();
    })());
  })();
  var request2 = /* @__PURE__ */ request()(optionsFormIsSymbol)(ordUnit);
  var RequestProgramExecution = (value0) => $Query3(value0);
  var SelectedCommandChanged = /* @__PURE__ */ $Output();
  var HandleOptionsFormOutput = (value0) => $Action3("HandleOptionsFormOutput", value0);
  var RunProgram = /* @__PURE__ */ $Action3("RunProgram");
  var monadThrowErrorAppM = /* @__PURE__ */ monadThrowReaderT(monadThrowEffect);
  var monadErrorErrorAppM = /* @__PURE__ */ monadErrorReaderT(monadErrorEffect);
  var monadAskMapStringStringAp = /* @__PURE__ */ (() => {
    const monadReaderT1 = monadReaderT(monadEffect);
    return { ask: pureE, Monad0: () => monadReaderT1 };
  })();
  var asks = (f) => monadAskMapStringStringAp.Monad0().Bind1().Apply0().Functor0().map(f)(monadAskMapStringStringAp.ask);
  var monadAppM = /* @__PURE__ */ monadReaderT(monadEffect);
  var bindAppM = /* @__PURE__ */ bindReaderT(bindEffect);
  var applicativeAppM = /* @__PURE__ */ (() => {
    const functorReaderT1 = {
      map: (x) => (v) => (x$1) => {
        const $0 = v(x$1);
        return () => {
          const a$p = $0();
          return x(a$p);
        };
      }
    };
    const applyReaderT1 = {
      apply: (v) => (v1) => (r) => {
        const $0 = v(r);
        const $1 = v1(r);
        return () => {
          const f$p = $0();
          const a$p = $1();
          return f$p(a$p);
        };
      },
      Functor0: () => functorReaderT1
    };
    return { pure: (x) => (v) => () => x, Apply0: () => applyReaderT1 };
  })();
  var fileAccessAppM = {
    readFileContent: (filePath) => bindAppM.bind(asks(lookup(ordString)(filePath)))((fileContents) => {
      const $0 = monadThrowErrorAppM.throwError(error("file " + filePath + " not found"));
      if (fileContents.tag === "Nothing") {
        return $0;
      }
      if (fileContents.tag === "Just") {
        return applicativeAppM.pure(fileContents._1);
      }
      fail();
    }),
    Monad0: () => monadAppM
  };
  var runCommand = (dictMonadAff) => (filesystem) => (command) => dictMonadAff.liftAff(_liftEffect(command(filesystem)));
  var renderProgramOutput = (v) => $VDom(
    "Elem",
    Nothing,
    "div",
    [],
    [
      $VDom(
        "Elem",
        Nothing,
        "div",
        [],
        [
          $VDom("Elem", Nothing, "label", [], [$VDom("Text", "Exit code:")]),
          $VDom("Text", showIntImpl(v.exitCode))
        ]
      ),
      $VDom(
        "Elem",
        Nothing,
        "div",
        [],
        [
          $VDom("Elem", Nothing, "label", [], [$VDom("Text", "Standard output:")]),
          $VDom(
            "Elem",
            Nothing,
            "pre",
            [],
            [$VDom("Elem", Nothing, "samp", [], [$VDom("Text", v.stdout)])]
          )
        ]
      ),
      $VDom(
        "Elem",
        Nothing,
        "div",
        [],
        [
          $VDom("Elem", Nothing, "label", [], [$VDom("Text", "Standard error:")]),
          $VDom(
            "Elem",
            Nothing,
            "pre",
            [],
            [$VDom("Elem", Nothing, "samp", [], [$VDom("Text", v.stderr)])]
          )
        ]
      )
    ]
  );
  var render9 = (dictMonadAff) => (dictMonadError) => (formComponent) => (v) => $VDom(
    "Elem",
    Nothing,
    "div",
    [style("display: flex")],
    [
      $VDom(
        "Elem",
        Nothing,
        "div",
        [style("width: 100%")],
        [
          $VDom("Elem", Nothing, "div", [], [$VDom("Text", "Options Form:")]),
          slot2($$Proxy)()(formComponent)()(HandleOptionsFormOutput),
          $VDom(
            "Elem",
            Nothing,
            "button",
            [
              $Prop("Handler", "click", (ev) => $Maybe("Just", $Input("Action", RunProgram))),
              type_4(ButtonButton)
            ],
            [$VDom("Text", "execute program")]
          )
        ]
      ),
      $VDom(
        "Elem",
        Nothing,
        "div",
        [style("width: 100%")],
        [
          $VDom("Elem", Nothing, "div", [], [$VDom("Text", "Program Output:")]),
          (() => {
            if (v.programOutput.tag === "Nothing") {
              return $VDom("Text", "");
            }
            if (v.programOutput.tag === "Just") {
              return renderProgramOutput(v.programOutput._1);
            }
            fail();
          })()
        ]
      )
    ]
  );
  var handleAction3 = (dictMonadAff) => (dictMonadError) => (v) => {
    if (v.tag === "HandleOptionsFormOutput") {
      return $Free(
        $FreeView(
          "Bind",
          $HalogenF("State", (v$1) => $Tuple(void 0, { programOutput: Nothing })),
          (x) => $Free($FreeView("Return", x), CatNil)
        ),
        CatNil
      );
    }
    if (v.tag === "RunProgram") {
      const $0 = request2($$Proxy)()(RequestProgramExecution);
      return $Free(
        $0._1,
        snoc2($0._2)((programOutput) => $Free(
          $FreeView(
            "Bind",
            $HalogenF("State", (v$1) => $Tuple(void 0, { programOutput })),
            (x) => $Free($FreeView("Return", x), CatNil)
          ),
          CatNil
        ))
      );
    }
    fail();
  };
  var component5 = (dictMonadAff) => (dictMonadError) => (formComponent) => ({
    initialState: (v) => ({ programOutput: Nothing }),
    eval: mkEval({ ...defaultEval, handleAction: handleAction3(dictMonadAff)(dictMonadError) }),
    render: render9(dictMonadAff)(dictMonadError)(formComponent)
  });

  // output-es/CLI.Halogen/index.js
  var $Action4 = (tag, _1) => ({ tag, _1 });
  var $SelectedCommand = (tag) => tag;
  var compatOptionsIsSymbol = { reflectSymbol: () => "compatOptions" };
  var slot_1 = /* @__PURE__ */ slot_()(compatOptionsIsSymbol)(ordUnit);
  var diffOptionsIsSymbol = { reflectSymbol: () => "diffOptions" };
  var slot_2 = /* @__PURE__ */ slot_()(diffOptionsIsSymbol)(ordUnit);
  var validateOptionsIsSymbol = { reflectSymbol: () => "validateOptions" };
  var slot_3 = /* @__PURE__ */ slot_()(validateOptionsIsSymbol)(ordUnit);
  var outputFormatIsSymbol = { reflectSymbol: () => "outputFormat" };
  var slot_4 = /* @__PURE__ */ slot_()(outputFormatIsSymbol)(ordUnit);
  var $$get3 = /* @__PURE__ */ $Free(
    /* @__PURE__ */ $FreeView(
      "Bind",
      /* @__PURE__ */ $HalogenF("State", (s) => $Tuple(s, s)),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var request1 = /* @__PURE__ */ request()(outputFormatIsSymbol)(ordUnit);
  var request22 = /* @__PURE__ */ request()(compatOptionsIsSymbol)(ordUnit);
  var program4 = /* @__PURE__ */ program(fileAccessAppM)(monadErrorErrorAppM);
  var request3 = /* @__PURE__ */ request()(diffOptionsIsSymbol)(ordUnit);
  var program1 = /* @__PURE__ */ program2(fileAccessAppM)(monadErrorErrorAppM);
  var request4 = /* @__PURE__ */ request()(validateOptionsIsSymbol)(ordUnit);
  var program22 = /* @__PURE__ */ program3(fileAccessAppM)(monadErrorErrorAppM);
  var modify_6 = (f) => $Free(
    $FreeView(
      "Bind",
      $HalogenF("State", (s) => $Tuple(void 0, f(s))),
      (x) => $Free($FreeView("Return", x), CatNil)
    ),
    CatNil
  );
  var CompatCommand = /* @__PURE__ */ $SelectedCommand("CompatCommand");
  var DiffCommand = /* @__PURE__ */ $SelectedCommand("DiffCommand");
  var ValidateCommand = /* @__PURE__ */ $SelectedCommand("ValidateCommand");
  var Dummy = /* @__PURE__ */ $Action4("Dummy");
  var render10 = (v) => {
    const $0 = v.selectedCommand;
    const renderCommandSelectionItemFor = (command) => $VDom(
      "Elem",
      Nothing,
      "option",
      [
        selected((() => {
          if (command === "CompatCommand") {
            return $0 === "CompatCommand";
          }
          if (command === "DiffCommand") {
            return $0 === "DiffCommand";
          }
          return command === "ValidateCommand" && $0 === "ValidateCommand";
        })())
      ],
      [
        $VDom(
          "Text",
          (() => {
            if (command === "CompatCommand") {
              return "compat";
            }
            if (command === "DiffCommand") {
              return "diff";
            }
            if (command === "ValidateCommand") {
              return "validate";
            }
            fail();
          })()
        )
      ]
    );
    return $VDom(
      "Elem",
      Nothing,
      "form",
      [],
      [
        $VDom(
          "Elem",
          Nothing,
          "div",
          [],
          [
            $VDom("Elem", Nothing, "label", [$$for("command")], [$VDom("Text", "Command")]),
            $VDom(
              "Elem",
              Nothing,
              "select",
              [
                id2("command"),
                name3("command"),
                addForeignPropHandler("change")("selectedIndex")(readInt(monadIdentity))((v1) => {
                  if (v1 === 0) {
                    return $Action4("SelectedCommandUpdated", CompatCommand);
                  }
                  if (v1 === 1) {
                    return $Action4("SelectedCommandUpdated", DiffCommand);
                  }
                  if (v1 === 2) {
                    return $Action4("SelectedCommandUpdated", ValidateCommand);
                  }
                  return Dummy;
                })
              ],
              [renderCommandSelectionItemFor(CompatCommand), renderCommandSelectionItemFor(DiffCommand), renderCommandSelectionItemFor(ValidateCommand)]
            )
          ]
        ),
        slot_4($$Proxy)()(component3)(),
        $VDom(
          "Elem",
          Nothing,
          "fieldset",
          [],
          [
            $VDom("Elem", Nothing, "legend", [], [$VDom("Text", "Command Options")]),
            (() => {
              if ($0 === "CompatCommand") {
                return slot_1($$Proxy)()(component)();
              }
              if ($0 === "DiffCommand") {
                return slot_2($$Proxy)()(component2)();
              }
              if ($0 === "ValidateCommand") {
                return slot_3($$Proxy)()(component4)();
              }
              fail();
            })()
          ]
        )
      ]
    );
  };
  var handleQuery3 = (dictMonadAff) => {
    const runCommand2 = runCommand(monadAffHalogenM(dictMonadAff));
    return (v) => $Free(
      $$get3._1,
      snoc2($$get3._2)((state) => {
        const $0 = request1($$Proxy)()(GetOutputFormat);
        return $Free(
          $0._1,
          snoc2($0._2)((outputFormat) => {
            if (outputFormat.tag === "Nothing") {
              return $Free($FreeView("Return", Nothing), CatNil);
            }
            if (outputFormat.tag === "Just") {
              if (state.selectedCommand === "CompatCommand") {
                const $1 = request22($$Proxy)()(GetInput);
                return $Free(
                  $1._1,
                  snoc2($1._2)((input) => {
                    if (input.tag === "Just") {
                      const $2 = runCommand2(input._1.filesystem)(program4(outputFormat._1)(input._1.options));
                      return $Free(
                        $2._1,
                        snoc2($2._2)((programOutput) => $Free(
                          $FreeView("Return", $Maybe("Just", v._1(programOutput))),
                          CatNil
                        ))
                      );
                    }
                    if (input.tag === "Nothing") {
                      return $Free($FreeView("Return", Nothing), CatNil);
                    }
                    fail();
                  })
                );
              }
              if (state.selectedCommand === "DiffCommand") {
                const $1 = request3($$Proxy)()(GetInput);
                return $Free(
                  $1._1,
                  snoc2($1._2)((input) => {
                    if (input.tag === "Just") {
                      const $2 = runCommand2(input._1.filesystem)(program1(outputFormat._1)(input._1.options));
                      return $Free(
                        $2._1,
                        snoc2($2._2)((programOutput) => $Free(
                          $FreeView("Return", $Maybe("Just", v._1(programOutput))),
                          CatNil
                        ))
                      );
                    }
                    if (input.tag === "Nothing") {
                      return $Free($FreeView("Return", Nothing), CatNil);
                    }
                    fail();
                  })
                );
              }
              if (state.selectedCommand === "ValidateCommand") {
                const $1 = request4($$Proxy)()(GetInput);
                return $Free(
                  $1._1,
                  snoc2($1._2)((input) => {
                    if (input.tag === "Just") {
                      const $2 = runCommand2(input._1.filesystem)(program22(outputFormat._1)(input._1.options));
                      return $Free(
                        $2._1,
                        snoc2($2._2)((programOutput) => $Free(
                          $FreeView("Return", $Maybe("Just", v._1(programOutput))),
                          CatNil
                        ))
                      );
                    }
                    if (input.tag === "Nothing") {
                      return $Free($FreeView("Return", Nothing), CatNil);
                    }
                    fail();
                  })
                );
              }
            }
            fail();
          })
        );
      })
    );
  };
  var handleAction4 = (v) => {
    if (v.tag === "Dummy") {
      return $Free($FreeView("Return", void 0), CatNil);
    }
    if (v.tag === "SelectedCommandUpdated") {
      const $0 = v._1;
      const $1 = modify_6((st) => ({ ...st, selectedCommand: $0 }));
      return $Free(
        $1._1,
        snoc2($1._2)(() => $Free(
          $FreeView(
            "Bind",
            $HalogenF("Raise", SelectedCommandChanged, void 0),
            (x) => $Free($FreeView("Return", x), CatNil)
          ),
          CatNil
        ))
      );
    }
    fail();
  };
  var commandFormComponent = (dictMonadAff) => ({
    eval: mkEval({ ...defaultEval, handleAction: handleAction4, handleQuery: handleQuery3(dictMonadAff) }),
    initialState: (v) => ({ selectedCommand: ValidateCommand }),
    render: render10
  });

  // output-es/Effect.Aff.Class/index.js
  var monadAffAff = { liftAff: (x) => x, MonadEffect0: () => monadEffectAff };

  // output-es/Web.DOM.ParentNode/foreign.js
  var getEffProp = function(name4) {
    return function(node) {
      return function() {
        return node[name4];
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

  // output-es/Web.DOM.ParentNode/index.js
  var querySelector = (qs) => {
    const $0 = _querySelector(qs);
    return (x) => {
      const $1 = $0(x);
      return () => {
        const a$p = $1();
        return nullable(a$p, Nothing, Just);
      };
    };
  };

  // output-es/Web.HTML/foreign.js
  var windowImpl = function() {
    return window;
  };

  // output-es/Web.HTML.HTMLDocument.ReadyState/index.js
  var $ReadyState = (tag) => tag;
  var Loading = /* @__PURE__ */ $ReadyState("Loading");
  var Interactive = /* @__PURE__ */ $ReadyState("Interactive");
  var Complete = /* @__PURE__ */ $ReadyState("Complete");
  var parse = (v) => {
    if (v === "loading") {
      return $Maybe("Just", Loading);
    }
    if (v === "interactive") {
      return $Maybe("Just", Interactive);
    }
    if (v === "complete") {
      return $Maybe("Just", Complete);
    }
    return Nothing;
  };

  // output-es/Web.HTML.HTMLDocument/foreign.js
  function _readyState(doc) {
    return doc.readyState;
  }

  // output-es/Web.HTML.HTMLDocument/index.js
  var readyState = (doc) => () => {
    const a$p = _readyState(doc);
    const $0 = parse(a$p);
    if ($0.tag === "Nothing") {
      return Loading;
    }
    if ($0.tag === "Just") {
      return $0._1;
    }
    fail();
  };

  // output-es/Web.HTML.Window/foreign.js
  function document3(window2) {
    return function() {
      return window2.document;
    };
  }

  // output-es/Halogen.Aff.Util/index.js
  var selectElement = (query2) => _bind(_liftEffect((() => {
    const $0 = querySelector(query2);
    return () => {
      const $1 = windowImpl();
      const $2 = document3($1)();
      return $0($2)();
    };
  })()))((mel) => _pure((() => {
    if (mel.tag === "Just") {
      return _read(Nothing, Just, mel._1);
    }
    if (mel.tag === "Nothing") {
      return Nothing;
    }
    fail();
  })()));
  var runHalogenAff = /* @__PURE__ */ runAff_((v2) => {
    if (v2.tag === "Left") {
      return throwException(v2._1);
    }
    if (v2.tag === "Right") {
      return () => {
      };
    }
    fail();
  });
  var awaitLoad = /* @__PURE__ */ makeAff((callback) => () => {
    const $0 = windowImpl();
    const $1 = document3($0)();
    const rs = readyState($1)();
    if (rs === "Loading") {
      const et = windowImpl();
      const listener = eventListener((v) => callback($Either("Right", void 0)))();
      addEventListener2("DOMContentLoaded")(listener)(false)(et)();
      const $2 = _liftEffect(removeEventListener2("DOMContentLoaded")(listener)(false)(et));
      return (v) => $2;
    }
    callback($Either("Right", void 0))();
    return nonCanceler;
  });
  var awaitBody = /* @__PURE__ */ _bind(awaitLoad)(() => _bind(selectElement("body"))((body) => {
    const $0 = _throwError(error("Could not find body"));
    if (body.tag === "Nothing") {
      return $0;
    }
    if (body.tag === "Just") {
      return _pure(body._1);
    }
    fail();
  }));

  // output-es/Effect.Console/foreign.js
  var warn = function(s) {
    return function() {
      console.warn(s);
    };
  };

  // output-es/Data.Coyoneda/index.js
  var $CoyonedaF = (_1, _2) => ({ tag: "CoyonedaF", _1, _2 });

  // output-es/Effect.Ref/foreign.js
  var modifyImpl2 = function(f) {
    return function(ref) {
      return function() {
        var t = f(ref.value);
        ref.value = t.state;
        return t.value;
      };
    };
  };

  // output-es/Halogen.Query.HalogenQ/index.js
  var $HalogenQ = (tag, _1, _2) => ({ tag, _1, _2 });

  // output-es/Unsafe.Reference/foreign.js
  function reallyUnsafeRefEq(a) {
    return function(b) {
      return a === b;
    };
  }

  // output-es/Halogen.Subscription/index.js
  var traverse_3 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_1 = /* @__PURE__ */ traverse_3(foldableArray);
  var unsubscribe = (v) => v;
  var create = () => {
    let subscribers = [];
    return {
      emitter: (k) => () => {
        const $0 = subscribers;
        subscribers = [...$0, k];
        return () => {
          const $1 = subscribers;
          subscribers = deleteBy(reallyUnsafeRefEq)(k)($1);
        };
      },
      listener: (a) => {
        const $0 = traverse_1((k) => k(a));
        return () => {
          const $1 = subscribers;
          return $0($1)();
        };
      }
    };
  };

  // output-es/Halogen.Aff.Driver.Eval/index.js
  var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
  var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
  var parSequence_ = /* @__PURE__ */ parTraverse_(parallelAff)(applicativeParAff)(foldableList)(identity13);
  var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
  var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
  var alter2 = /* @__PURE__ */ alter(ordString);
  var unsubscribe2 = (sid) => (ref) => () => {
    const v = ref.value;
    const subs = v.subscriptions.value;
    return traverse_4(unsubscribe)((() => {
      const $0 = lookup(ordInt)(sid);
      if (subs.tag === "Just") {
        return $0(subs._1);
      }
      if (subs.tag === "Nothing") {
        return Nothing;
      }
      fail();
    })())();
  };
  var queueOrRun = (ref) => (au) => _bind(_liftEffect(() => ref.value))((v) => {
    if (v.tag === "Nothing") {
      return au;
    }
    if (v.tag === "Just") {
      return _liftEffect(() => ref.value = $Maybe("Just", $List("Cons", au, v._1)));
    }
    fail();
  });
  var handleLifecycle = (lchs) => (f) => _bind(_liftEffect(() => lchs.value = { initializers: Nil, finalizers: Nil }))(() => _bind(_liftEffect(f))((result) => _bind(_liftEffect(() => lchs.value))((v) => {
    const $0 = v.initializers;
    return _bind(traverse_22(forkAff)(v.finalizers))(() => _bind(parSequence_($0))(() => _pure(result)));
  })));
  var handleAff = /* @__PURE__ */ runAff_((v2) => {
    if (v2.tag === "Left") {
      return throwException(v2._1);
    }
    if (v2.tag === "Right") {
      return () => {
      };
    }
    fail();
  });
  var fresh = (f) => (ref) => _bind(_liftEffect(() => ref.value))((v) => _liftEffect(modifyImpl2((i) => ({ state: i + 1 | 0, value: f(i) }))(v.fresh)));
  var evalQ = (render11) => (ref) => (q) => _bind(_liftEffect(() => ref.value))((v) => evalM(render11)(ref)(v.component.eval($HalogenQ(
    "Query",
    $CoyonedaF((x) => $Maybe("Just", x), q),
    (v$1) => Nothing
  ))));
  var evalM = (render11) => (initRef) => (v) => foldFree2((v1) => {
    if (v1.tag === "State") {
      return _bind(_liftEffect(() => initRef.value))((v2) => {
        const v3 = v1._1(v2.state);
        if (reallyUnsafeRefEq(v2.state)(v3._2)) {
          return _pure(v3._1);
        }
        const $0 = v3._1;
        return _bind(_liftEffect((() => {
          const $1 = { ...v2, state: v3._2 };
          return () => initRef.value = $1;
        })()))(() => _bind(handleLifecycle(v2.lifecycleHandlers)(render11(v2.lifecycleHandlers)(initRef)))(() => _pure($0)));
      });
    }
    if (v1.tag === "Subscribe") {
      return _bind(fresh(SubscriptionId)(initRef))((sid) => _bind(_liftEffect(v1._1(sid)((x) => {
        const $0 = handleAff(evalF(render11)(initRef)($Input("Action", x)));
        return () => {
          $0();
        };
      })))((finalize) => _bind(_liftEffect(() => initRef.value))((v2) => _bind(_liftEffect((() => {
        const $0 = functorMaybe.map(insert(ordInt)(sid)(finalize));
        const $1 = v2.subscriptions;
        return () => {
          const $2 = $1.value;
          $1.value = $0($2);
        };
      })()))(() => _pure(v1._2(sid))))));
    }
    if (v1.tag === "Unsubscribe") {
      const $0 = v1._2;
      return _bind(_liftEffect(unsubscribe2(v1._1)(initRef)))(() => _pure($0));
    }
    if (v1.tag === "Lift") {
      return v1._1;
    }
    if (v1.tag === "ChildQuery") {
      const $0 = v1._1;
      return _bind(_liftEffect(() => initRef.value))((v1$1) => {
        const $1 = $0._2;
        return _map($0._3)(_sequential($0._1(applicativeParAff)((v3) => _bind(_liftEffect(() => v3.value))((dsx) => evalQ(render11)(dsx.selfRef)($1)))(v1$1.children)));
      });
    }
    if (v1.tag === "Raise") {
      const $0 = v1._2;
      const $1 = v1._1;
      return _bind(_liftEffect(() => initRef.value))((v2) => {
        const $2 = v2.handlerRef;
        const $3 = v2.pendingOuts;
        return _bind(_liftEffect(() => $2.value))((handler) => _bind(queueOrRun($3)(handler($1)))(() => _pure($0)));
      });
    }
    if (v1.tag === "Par") {
      return _sequential(foldFreeAp(applicativeParAff)(identity10)((() => {
        const $0 = evalM(render11)(initRef);
        return foldFreeAp(applicativeFreeAp)((x) => $FreeAp("Lift", $0(x)))(v1._1);
      })()));
    }
    if (v1.tag === "Fork") {
      const $0 = v1._1;
      return _bind(fresh(ForkId)(initRef))((fid) => _bind(_liftEffect(() => initRef.value))((v2) => {
        const $1 = v2.forks;
        return _bind(_liftEffect(() => ({ value: false })))((doneRef) => _bind(forkAff($$finally(_liftEffect((() => {
          const $2 = $$delete(ordInt)(fid);
          return () => {
            const $3 = $1.value;
            $1.value = $2($3);
            return doneRef.value = true;
          };
        })()))(evalM(render11)(initRef)($0))))((fiber) => _bind(_liftEffect((() => {
          const $2 = insert(ordInt)(fid)(fiber);
          return () => {
            const b = doneRef.value;
            if (!b) {
              const $3 = $1.value;
              $1.value = $2($3);
              return;
            }
            if (b) {
              return;
            }
            fail();
          };
        })()))(() => _pure(v1._2(fid)))));
      }));
    }
    if (v1.tag === "Join") {
      const $0 = v1._2;
      const $1 = v1._1;
      return _bind(_liftEffect(() => initRef.value))((v2) => {
        const $2 = v2.forks;
        return _bind(_liftEffect(() => $2.value))((forkMap) => _bind(traverse_32(joinFiber)(lookup(ordInt)($1)(forkMap)))(() => _pure($0)));
      });
    }
    if (v1.tag === "Kill") {
      const $0 = v1._2;
      const $1 = v1._1;
      return _bind(_liftEffect(() => initRef.value))((v2) => {
        const $2 = v2.forks;
        return _bind(_liftEffect(() => $2.value))((forkMap) => _bind(traverse_32(killFiber(error("Cancelled")))(lookup(ordInt)($1)(forkMap)))(() => _pure($0)));
      });
    }
    if (v1.tag === "GetRef") {
      const $0 = v1._1;
      return _bind(_liftEffect(() => initRef.value))((v2) => _pure(v1._2(lookup(ordString)($0)(v2.refs))));
    }
    fail();
  })(v);
  var evalF = (render11) => (ref) => (v) => {
    if (v.tag === "RefUpdate") {
      const $0 = v._2;
      const $1 = v._1;
      return _liftEffect(() => {
        const $2 = ref.value;
        ref.value = { ...$2, refs: alter2((v$1) => $0)($1)($2.refs) };
      });
    }
    if (v.tag === "Action") {
      const $0 = v._1;
      return _bind(_liftEffect(() => ref.value))((v1) => evalM(render11)(ref)(v1.component.eval($HalogenQ("Action", $0, void 0))));
    }
    fail();
  };

  // output-es/Halogen.Aff.Driver.State/index.js
  var initDriverState = (component6) => (input) => (handler) => (lchs) => () => {
    const selfRef = { value: {} };
    const childrenIn = { value: Leaf2 };
    const childrenOut = { value: Leaf2 };
    const handlerRef = { value: handler };
    const pendingQueries = { value: $Maybe("Just", Nil) };
    const pendingOuts = { value: $Maybe("Just", Nil) };
    const pendingHandlers = { value: Nothing };
    const fresh2 = { value: 1 };
    const subscriptions = { value: $Maybe("Just", Leaf2) };
    const forks = { value: Leaf2 };
    selfRef.value = {
      component: component6,
      state: component6.initialState(input),
      refs: Leaf2,
      children: Leaf2,
      childrenIn,
      childrenOut,
      selfRef,
      handlerRef,
      pendingQueries,
      pendingOuts,
      pendingHandlers,
      rendering: Nothing,
      fresh: fresh2,
      subscriptions,
      forks,
      lifecycleHandlers: lchs
    };
    return selfRef;
  };

  // output-es/Halogen.Aff.Driver/index.js
  var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
  var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
  var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
  var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
  var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
  var parSequence_2 = /* @__PURE__ */ parTraverse_(parallelAff)(applicativeParAff)(foldableList)(identity13);
  var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
  var renderStateX_ = /* @__PURE__ */ (() => {
    const traverse_$1 = traverse_(applicativeEffect)(foldableMaybe);
    return (f) => (st) => traverse_$1(f)(st.rendering);
  })();
  var newLifecycleHandlers = () => ({ value: { initializers: Nil, finalizers: Nil } });
  var handlePending = (ref) => () => {
    const queue = ref.value;
    ref.value = Nothing;
    return for_2(queue)((() => {
      const $0 = traverse_5(forkAff);
      return (x) => handleAff($0(reverse2(x)));
    })())();
  };
  var cleanupSubscriptionsAndForks = (v) => {
    const $0 = traverse_23(traverse_33(unsubscribe));
    const $1 = v.subscriptions;
    return () => {
      const $2 = $1.value;
      $0($2)();
      v.subscriptions.value = Nothing;
      const $3 = v.forks.value;
      traverse_33((() => {
        const $4 = killFiber(error("finalized"));
        return (x) => handleAff($4(x));
      })())($3)();
      return v.forks.value = Leaf2;
    };
  };
  var runUI = (renderSpec2) => (component6) => (i) => {
    const squashChildInitializers = (lchs) => (preInits) => (st) => {
      const parentInitializer = evalM(render11)(st.selfRef)(st.component.eval($HalogenQ("Initialize", void 0)));
      return () => {
        const $0 = lchs.value;
        lchs.value = {
          initializers: $List(
            "Cons",
            _bind(parSequence_2(reverse2($0.initializers)))(() => _bind(parentInitializer)(() => _liftEffect((() => {
              const $1 = handlePending(st.pendingQueries);
              return () => {
                $1();
                return handlePending(st.pendingOuts)();
              };
            })()))),
            preInits
          ),
          finalizers: $0.finalizers
        };
      };
    };
    const runComponent = (lchs) => (handler) => (j) => (c) => () => {
      const lchs$p = newLifecycleHandlers();
      const $$var = initDriverState(c)(j)(handler)(lchs$p)();
      const pre = lchs.value;
      lchs.value = { initializers: Nil, finalizers: pre.finalizers };
      const $0 = $$var.value;
      render11(lchs)($0.selfRef)();
      const $1 = $$var.value;
      squashChildInitializers(lchs)(pre.initializers)($1)();
      return $$var;
    };
    const renderChild = (lchs) => (handler) => (childrenInRef) => (childrenOutRef) => (slot3) => () => {
      const a$p = childrenInRef.value;
      const childrenIn = slot3.pop(a$p);
      const $$var = (() => {
        if (childrenIn.tag === "Just") {
          childrenInRef.value = childrenIn._1._2;
          const dsx = childrenIn._1._1.value;
          const $02 = _pure();
          dsx.handlerRef.value = (x) => {
            const $12 = slot3.output(x);
            if ($12.tag === "Nothing") {
              return $02;
            }
            if ($12.tag === "Just") {
              return handler($12._1);
            }
            fail();
          };
          handleAff(evalM(render11)(dsx.selfRef)(dsx.component.eval($HalogenQ(
            "Receive",
            slot3.input,
            void 0
          ))))();
          return childrenIn._1._1;
        }
        if (childrenIn.tag === "Nothing") {
          return runComponent(lchs)((() => {
            const $02 = _pure();
            return (x) => {
              const $12 = slot3.output(x);
              if ($12.tag === "Nothing") {
                return $02;
              }
              if ($12.tag === "Just") {
                return handler($12._1);
              }
              fail();
            };
          })())(slot3.input)(slot3.component)();
        }
        fail();
      })();
      const a$p$1 = childrenOutRef.value;
      const $0 = warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur");
      if ((() => {
        const $12 = slot3.get(a$p$1);
        if ($12.tag === "Nothing") {
          return false;
        }
        if ($12.tag === "Just") {
          return true;
        }
        fail();
      })()) {
        $0();
      }
      const $1 = childrenOutRef.value;
      childrenOutRef.value = slot3.set($$var)($1);
      const $2 = $$var.value;
      if ($2.rendering.tag === "Nothing") {
        return throwException(error("Halogen internal error: child was not initialized in renderChild"))();
      }
      if ($2.rendering.tag === "Just") {
        return renderSpec2.renderChild($2.rendering._1);
      }
      fail();
    };
    const render11 = (lchs) => ($$var) => () => {
      const v = $$var.value;
      const a$p = v.pendingHandlers.value;
      const shouldProcessHandlers = (() => {
        if (a$p.tag === "Nothing") {
          return true;
        }
        if (a$p.tag === "Just") {
          return false;
        }
        fail();
      })();
      if (shouldProcessHandlers) {
        v.pendingHandlers.value = $Maybe("Just", Nil);
      }
      v.childrenOut.value = Leaf2;
      v.childrenIn.value = v.children;
      const $0 = v.pendingHandlers;
      const rendering = renderSpec2.render((() => {
        const $12 = _map((v$1) => {
        });
        return (x) => handleAff(queueOrRun($0)($12(evalF(render11)(v.selfRef)(x))));
      })())(renderChild(lchs)((() => {
        const $12 = _map((v$1) => {
        });
        return (x) => queueOrRun(v.pendingQueries)(queueOrRun($0)($12(evalF(render11)(v.selfRef)($Input(
          "Action",
          x
        )))));
      })())(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
      const children2 = v.childrenOut.value;
      const childrenIn = v.childrenIn.value;
      foreachSlot2(childrenIn)((v1) => () => {
        const childDS = v1.value;
        renderStateX_(renderSpec2.removeChild)(childDS)();
        return finalize(lchs)(childDS)();
      })();
      const $1 = v.selfRef.value;
      v.selfRef.value = { ...$1, rendering: $Maybe("Just", rendering), children: children2 };
      const $2 = monadRecEffect.tailRecM((v1) => () => {
        const handlers = $0.value;
        $0.value = $Maybe("Just", Nil);
        traverse_23((() => {
          const $22 = traverse_5(forkAff);
          return (x) => handleAff($22(reverse2(x)));
        })())(handlers)();
        const mmore = $0.value;
        if ((() => {
          if (mmore.tag === "Nothing") {
            return false;
          }
          if (mmore.tag === "Just") {
            return mmore._1.tag === "Nil";
          }
          fail();
        })()) {
          $0.value = Nothing;
          return $Step("Done", void 0);
        }
        return $Step("Loop", void 0);
      })();
      if (shouldProcessHandlers) {
        return $2();
      }
    };
    const finalize = (lchs) => (st) => {
      const $0 = cleanupSubscriptionsAndForks(st);
      return () => {
        $0();
        const $1 = lchs.value;
        lchs.value = {
          initializers: $1.initializers,
          finalizers: $List(
            "Cons",
            evalM(render11)(st.selfRef)(st.component.eval($HalogenQ("Finalize", void 0))),
            $1.finalizers
          )
        };
        return foreachSlot2(st.children)((v) => () => {
          const dsx = v.value;
          return finalize(lchs)(dsx)();
        })();
      };
    };
    return _bind(_liftEffect(newLifecycleHandlers))((lchs) => _bind(_liftEffect(() => ({ value: false })))((disposed) => handleLifecycle(lchs)(() => {
      const sio = create();
      const $0 = runComponent(lchs)((x) => _liftEffect(sio.listener(x)))(i)(component6)();
      const dsx = $0.value;
      return {
        query: (() => {
          const $1 = dsx.selfRef;
          return (q) => _bind(_liftEffect(() => disposed.value))((v) => {
            if (v) {
              return _pure(Nothing);
            }
            return evalQ(render11)($1)(q);
          });
        })(),
        messages: sio.emitter,
        dispose: handleLifecycle(lchs)(() => {
          const v = disposed.value;
          if (v) {
            return;
          }
          disposed.value = true;
          finalize(lchs)(dsx)();
          const v2 = dsx.selfRef.value;
          return for_2(v2.rendering)(renderSpec2.dispose)();
        })
      };
    })));
  };

  // output-es/Web.DOM.Node/foreign.js
  var getEffProp2 = function(name4) {
    return function(node) {
      return function() {
        return node[name4];
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

  // output-es/Halogen.VDom.Driver/index.js
  var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
  var identity14 = (x) => x;
  var substInParent = (v) => (v1) => (v2) => {
    if (v2.tag === "Just") {
      if (v1.tag === "Just") {
        const $0 = insertBefore(v)(v1._1)(v2._1);
        return () => {
          $0();
        };
      }
      if (v1.tag === "Nothing") {
        const $0 = appendChild(v)(v2._1);
        return () => {
          $0();
        };
      }
    }
    return () => {
    };
  };
  var removeChild3 = (v) => {
    const $0 = v.node;
    const $1 = _parentNode($0);
    return () => {
      const a$p = $1();
      return traverse_6((pn) => removeChild2($0)(pn))(nullable(a$p, Nothing, Just))();
    };
  };
  var mkSpec = (handler) => (renderChildRef) => (document4) => ({
    buildWidget: (spec) => {
      const buildThunk2 = buildThunk(unsafeCoerce)(spec);
      const renderComponentSlot = (cs) => {
        const renderChild = renderChildRef.value;
        const rsx = renderChild(cs)();
        return $Step$p(
          rsx.node,
          Nothing,
          patch,
          (st) => {
            if (st.tag === "Just") {
              const $0 = st._1;
              return halt($0);
            }
          }
        );
      };
      const render11 = (slot3) => {
        if (slot3.tag === "ComponentSlot") {
          const $0 = slot3._1;
          return renderComponentSlot($0);
        }
        if (slot3.tag === "ThunkSlot") {
          const $0 = slot3._1;
          const step3 = buildThunk2($0);
          return $Step$p(
            step3._1,
            $Maybe("Just", step3),
            patch,
            (st) => {
              if (st.tag === "Just") {
                const $1 = st._1;
                return halt($1);
              }
            }
          );
        }
        fail();
      };
      const patch = (st, slot3) => {
        if (st.tag === "Just") {
          if (slot3.tag === "ComponentSlot") {
            const $0 = slot3._1;
            halt(st._1);
            return renderComponentSlot($0);
          }
          if (slot3.tag === "ThunkSlot") {
            const $0 = slot3._1;
            const step$p = step(st._1, $0);
            return $Step$p(
              step$p._1,
              $Maybe("Just", step$p),
              patch,
              (st$1) => {
                if (st$1.tag === "Just") {
                  const $1 = st$1._1;
                  return halt($1);
                }
              }
            );
          }
          fail();
        }
        return render11(slot3);
      };
      return render11;
    },
    buildAttributes: buildProp(handler),
    document: document4
  });
  var renderSpec = (document4) => (container) => ({
    render: (handler) => (child) => (v) => (v1) => {
      if (v1.tag === "Nothing") {
        return () => {
          const renderChildRef = { value: child };
          const machine = buildVDom(mkSpec(handler)(renderChildRef)(document4))(v);
          appendChild(machine._1)(container)();
          return { machine, node: machine._1, renderChildRef };
        };
      }
      if (v1.tag === "Just") {
        const $0 = v1._1.machine;
        const $1 = v1._1.node;
        const $2 = v1._1.renderChildRef;
        return () => {
          $2.value = child;
          const a$p = _parentNode($1)();
          const a$p$1 = _nextSibling($1)();
          const machine$p = step($0, v);
          const $3 = substInParent(machine$p._1)(nullable(a$p$1, Nothing, Just))(nullable(
            a$p,
            Nothing,
            Just
          ));
          if (!reallyUnsafeRefEq($1)(machine$p._1)) {
            $3();
          }
          return { machine: machine$p, node: machine$p._1, renderChildRef: $2 };
        };
      }
      fail();
    },
    renderChild: identity14,
    removeChild: removeChild3,
    dispose: removeChild3
  });
  var runUI2 = (component6) => (i) => (element) => _bind(_liftEffect(() => {
    const $0 = windowImpl();
    return document3($0)();
  }))((document4) => runUI(renderSpec(document4)(element))(component6)(i));

  // output-es/Sandbox.Main/index.js
  var commandFormComponent2 = /* @__PURE__ */ commandFormComponent(monadAffAff);
  var main = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ _bind(awaitBody)((body) => runUI2(component5(monadAffAff)(monadErrorAff)(commandFormComponent2))()(body)));

  // <stdin>
  main();
})();
