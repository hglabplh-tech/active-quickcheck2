(ns active.quickcheck2.arbitrary-syntax
  (:require [active.quickcheck2.arbitrary :refer :all])
  )

;; Arbitrary and Coarbitrary multimethods
;; --------------------------------------

(defmulti expand-arbitrary
  "Multimethod to expand `arbitrary' forms.

Dispatches on the symbol for atomic arbitrary forms,
and on [op] for compound arbitrary forms, where op is
the operator."
  (fn [form]
    (cond
      (symbol? form) form
      (or (not (seq? form)) (not (seq form))) :default
      (some #(= '-> %) form) :function
      :else [(first form)])))

(defmulti expand-coarbitrary
  "Multimethod to expand `coarbitrary' forms.

  Dispatches on the symbol for atomic coarbitrary forms,
  and on [op] for compound coarbitrary forms, where op is
  the operator."
  (fn [form]
    (cond
      (symbol? form) form
      (or (not (seq? form)) (not (seq form))) :default
      (some #(= '-> %) form) :function
      :else [(first form)])))

(defmethod expand-arbitrary :default [form]
  (throw (Exception. (str "invalid expand-arbitrary form: " form))))

(defmethod expand-coarbitrary :default [form]
  (throw (Exception. (str "invalid expand-coarbitrary form: " form))))

(defmethod expand-arbitrary :function [form]
  (let [[before with] (split-with #(not= % '->) form)
        after (rest with)]
    (if (not= 1 (count after))
      (throw (Exception. (str "more than one codomain for expand-arbitrary function form: " form))))
    `(arbitrary-function ~(expand-arbitrary (first after)) ~@(map expand-coarbitrary before))))

(defmethod expand-coarbitrary :function [form]
  (let [[before with] (split-with #(not= % '->) form)
        after (rest with)]
    (if (not= 1 (count after))
      (throw (Exception. (str "more than one codomain for expand-coarbitrary function form: " form))))
    `(coarbitrary-function ~(expand-coarbitrary (first after)) ~@(map expand-arbitrary before))))

(defmethod expand-arbitrary 'boolean [form]
  `arbitrary-boolean)

(defmethod expand-coarbitrary 'boolean [form]
  `coarbitrary-boolean)

(defmethod expand-arbitrary 'integer [form]
  `arbitrary-integer)

(defmethod expand-coarbitrary 'integer [form]
  `coarbitrary-integer)

(defmethod expand-arbitrary 'byte [form]
  `arbitrary-byte)

(defmethod expand-coarbitrary 'byte [form]
  `coarbitrary-byte)

(defmethod expand-arbitrary 'short [form]
  `arbitrary-short)

(defmethod expand-coarbitrary 'short [form]
  `coarbitrary-short)

(defmethod expand-arbitrary 'int [form]
  `arbitrary-int)

(defmethod expand-coarbitrary 'int [form]
  `coarbitrary-int)

(defmethod expand-arbitrary 'long [form]
  `arbitrary-long)

(defmethod expand-coarbitrary 'long [form]
  `coarbitrary-long)

(defmethod expand-arbitrary 'unsigned-byte [form]
  `arbitrary-unsigned-byte)

(defmethod expand-coarbitrary 'unsigned-byte [form]
  `coarbitrary-unsigned-byte)

(defmethod expand-arbitrary 'unsigned-short [form]
  `arbitrary-unsigned-short)

(defmethod expand-coarbitrary 'unsigned-short [form]
  `coarbitrary-unsigned-short)

(defmethod expand-arbitrary 'unsigned-int [form]
  `arbitrary-unsigned-int)

(defmethod expand-coarbitrary 'unsigned-int [form]
  `coarbitrary-unsigned-int)

(defmethod expand-arbitrary 'unsigned-long [form]
  `arbitrary-unsigned-long)

(defmethod expand-coarbitrary 'unsigned-long [form]
  `coarbitrary-unsigned-long)

(defmethod expand-arbitrary 'natural [form]
  `arbitrary-natural)

(defmethod expand-coarbitrary 'natural [form]
  `coarbitrary-natural)

(defmethod expand-arbitrary 'rational [form]
  `arbitrary-rational)

(defmethod expand-coarbitrary 'rational [form]
  `coarbitrary-rational)

(defmethod expand-arbitrary 'float [form]
  `arbitrary-float)

(defmethod expand-coarbitrary 'float [form]
  `coarbitrary-float)

(defmethod expand-arbitrary 'char [form]
  `arbitrary-char)

(defmethod expand-coarbitrary 'char [form]
  `coarbitrary-char)

(defmethod expand-arbitrary 'ascii-char [form]
  `arbitrary-ascii-char)

(defmethod expand-coarbitrary 'ascii-char [form]
  `coarbitrary-ascii-char)

(defmethod expand-arbitrary 'printable-ascii-char [form]
  `arbitrary-printable-ascii-char)

(defmethod expand-coarbitrary 'printable-ascii-char [form]
  `coarbitrary-printable-ascii-char)

(defmethod expand-arbitrary 'string [form]
  `arbitrary-string)

(defmethod expand-coarbitrary 'string [form]
  `coarbitrary-string)

(defmethod expand-arbitrary 'ascii-string [form]
  `arbitrary-ascii-string)

(defmethod expand-coarbitrary 'ascii-string [form]
  `coarbitrary-ascii-string)

(defmethod expand-arbitrary 'printable-ascii-string [form]
  `arbitrary-printable-ascii-string)

(defmethod expand-coarbitrary 'printable-ascii-string [form]
  `coarbitrary-printable-ascii-string)

(defmethod expand-arbitrary 'byte-array [form]
  `arbitrary-byte-array)

(defmethod expand-coarbitrary 'byte-array [form]
  `coarbitrary-byte-array)

(defmethod expand-arbitrary 'symbol [form]
  `arbitrary-symbol)

(defmethod expand-coarbitrary 'symbol [form]
  `coarbitrary-symbol)

(defmethod expand-arbitrary 'keyword [form]
  `arbitrary-keyword)

(defmethod expand-coarbitrary 'keyword [form]
  `coarbitrary-keyword)

(defn- expand-has-arg-count
  [form n]
  (if (not= (- (count form) 1) n)
    (throw (Exception. (str "Form should have " n " arguments: " form)))))

(defn- expand-has-at-least-arg-count
  [form n]
  (if (< (- (count form) 1) n)
    (throw (Exception. (str "Form should have at least " n " arguments: " form)))))

(defmethod expand-arbitrary '[clojure.core/unquote] [form]
  (expand-has-arg-count form 1)
  (second form))

(defmethod expand-coarbitrary '[clojure.core/unquote] [form]
  (expand-has-arg-count form 1)
  (second form))

(defmethod expand-arbitrary '[integer-from-to] [form]
  (expand-has-at-least-arg-count form 2)
  `(arbitrary-integer-from-to ~(second form) ~@(nthrest form 2)))

(defmethod expand-coarbitrary '[integer-from-to] [form]
  (expand-has-at-least-arg-count form 2)
  `(coarbitrary-integer-from-to ~(second form) ~@(nthrest form 2)))

(defmethod expand-arbitrary '[one-of] [form]
  (expand-has-at-least-arg-count form 2)
  `(arbitrary-one-of ~(second form) ~@(nthrest form 2)))

(defmethod expand-coarbitrary '[one-of] [form]
  (expand-has-at-least-arg-count form 2)
  `(coarbitrary-one-of ~(second form) ~@(nthrest form 2)))

(defmethod expand-arbitrary '[tuple] [form]
  `(arbitrary-tuple ~@(map expand-arbitrary (rest form))))

(defmethod expand-coarbitrary '[tuple] [form]
  `(coarbitrary-tuple ~@(map expand-coarbitrary (rest form))))

(defmethod expand-arbitrary '[list] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-list ~(expand-arbitrary (nth form 1))))

(defmethod expand-coarbitrary '[list] [form]
  (expand-has-arg-count form 1)
  `(coarbitrary-list ~(expand-coarbitrary (nth form 1))))

(defmethod expand-arbitrary '[vector] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-vector ~(expand-arbitrary (nth form 1))))

(defmethod expand-coarbitrary '[vector] [form]
  (expand-has-arg-count form 1)
  `(coarbitrary-vector ~(expand-coarbitrary (nth form 1))))

(defmethod expand-arbitrary '[spec] [form]
  (expand-has-arg-count form 1)
  `(spec->arbitrary ~(nth form 1)))

(defmethod expand-coarbitrary '[spec] [form]
  (expand-has-arg-count form 1)
  `(spec->coarbitrary ~(nth form 1)))

(defmethod expand-arbitrary '[map] [form]
  (expand-has-arg-count form 2)
  `(arbitrary-map ~(expand-arbitrary (nth form 1))
     ~(expand-arbitrary (nth form 2))))

(defmethod expand-coarbitrary '[map] [form]
  (expand-has-arg-count form 2)
  `(coarbitrary-map ~(expand-coarbitrary (nth form 1))
     ~(expand-coarbitrary (nth form 2))))

(defmethod expand-arbitrary '[set] [form]
  (expand-has-arg-count form 1)
  `(arbitrary-set ~(expand-arbitrary (nth form 1))))

(defmethod expand-coarbitrary '[set] [form]
  (expand-has-arg-count form 1)
  `(coarbitrary-set ~(expand-coarbitrary (nth form 1))))

; (record cons (acc ...) arb ...)
(defmethod expand-arbitrary '[record] [form]
  (expand-has-arg-count form 2)
  (let [ops (nth form 2)]
    (when (odd? (count ops))
      (throw (Exception. "Even number of field operands to record.")))
    (let [pairs (partition 2 ops)]
      `(arbitrary-record ~(nth form 1) (list ~@(map first pairs))
         ~@(map expand-arbitrary (map second pairs))))))

(defmethod expand-coarbitrary '[record] [form]
  (expand-has-arg-count form 2)
  (let [ops (nth form 2)]
    (when (odd? (count ops))
      (throw (Exception. "Even number of field operands to record.")))
    (let [pairs (partition 2 ops)]
      `(coarbitrary-record ~(nth form 1) (list ~@(map first pairs))
         ~@(map expand-coarbitrary (map second pairs))))))

; (mixed pred arb ...)
(defmethod expand-arbitrary '[mixed] [form]
  (expand-has-at-least-arg-count form 2)
  (when (even? (count form))
    (throw (Exception. "Odd number of operands to mixed.")))
  `(arbitrary-mixed (list ~@(map (fn [[pred arb]]
                                   `(list ~pred (delay ~(expand-arbitrary arb))))
                              (partition 2 (rest form))))))

(defmethod expand-coarbitrary '[mixed] [form]
  (expand-has-at-least-arg-count form 2)
  (when (even? (count form))
    (throw (Exception. "Odd number of operands to mixed.")))
  `(coarbitrary-mixed (list ~@(map (fn [[pred arb]]
                                     `(list ~pred (delay ~(expand-coarbitrary arb))))
                                   (partition 2 (rest form))))))


;; ------

(defmacro coarbitrary
  [form]
  (expand-coarbitrary form))

(defmacro arbitrary
  "Convenient syntax for constructing arbitraries.

This is usually used implicitly via the property macro.

The argument form can be one of the following:

- boolean, integer, byte, short, int, long, 
  unsigned byte, unsigned-short, unsigned-int, unsigned-long
  natural, rational, float, char, ascii-char,
  printable-ascii-char, string, ascii-string, printable-ascii-string,
  byte-array, symbol, keyword
- (one-of <equality> <expr> ...)
- (tuple <arb> ...)
- (list <arb>)
- (vector <arb>)
- (set <arb>)
- (record <constructor> [<accessor> <arb> ...])
- (mixed <pred> <arb> <pred> <arb> ...)
- (map <arb1> <arb2>) ; map with keys from <arb1>, values from <arb2>
- ~<expr>, which evaluates <expr> as a regular expression

The syntax is extensible via the expand-arbitrary multimethod."
  [form]
  (expand-arbitrary form))

