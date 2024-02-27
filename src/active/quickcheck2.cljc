; Copyright (c) Active Group GmbH. All rights reserved.
; The use and distribution terms for this software are covered by the
; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
; which can be found in the file epl-v10.html at the root of this distribution.
; By using this software in any fashion, you are agreeing to be bound by
; the terms of this license.
; You must not remove this notice, or any other, from this software.

; quickcheck.clj: QuickCheck clone for Clojure

(ns ^{:author "Michael Sperber,  Markus Schlegel, Florian Engel, working from John Hughes's original Haskell version"
      :doc "A QuickCheck clone for Clojure."}
  active.quickcheck2
  (:require #?(:clj [active.data.record :refer [def-record is-a?]]
               :cljs [active.data.record :refer [is-a?] :refer-macros [def-record]]))
  (:require [active.clojure.monad :as monad])
  (:require [active.quickcheck2.tree :as tree])
  (:require [active.quickcheck2.generator-applicative :refer [with-tree?
                                                              with-tree
                                                              get-tree
                                                              integrated
                                                              combine-generators
                                                              combine-generators-curry]])
  (:require [active.quickcheck2.generator :as generator])
  (:require [active.quickcheck2.random
             :refer [make-random-generator random-generator-split]])
  (:require [active.quickcheck2.shrink :as shrink])
  (:require [clojure.math.numeric-tower :refer [expt]])
  (:use [clojure.test :only [assert-expr do-report]]))

(def-record ^{:doc "Generalization of generator, suitable for producing function generators."}
  Arbitrary-type
  [arbitrary-generator])

(defn make-arbitrary
  [generator]
  (Arbitrary-type arbitrary-generator generator))

(def-record ^{:doc "Coarbitrary typeclass in original Haskell implementation"}
  Coarbitrary-type
  [coarbitrary-coarbitrary])

(defn make-coarbitrary
  [coarb]
  (Coarbitrary-type coarbitrary-coarbitrary coarb))

(defn such-that
  "Takes a arbitary and a predicate and
  returns a new generator that satisfies
  the predicate."
  [arb pred]
  (let [gen (arbitrary-generator arb)
        newgen (generator/such-that-generator gen pred)]
    (make-arbitrary newgen))) ;; TODO: write coarbitrary implementation

(defn generate-one-of
  "Randomly choose one of a list of given arbitraries"
  [arbs]
  (monad/free-bind (with-tree (generator/choose-one-of arbs))
                   arbitrary-generator))

;; Arbitraries
;; -----------

(def arbitrary-boolean
  "Arbitrary boolean."
  (make-arbitrary
    (generator/choose-one-of '(true false))))

(def coarbitrary-boolean
  "Coarbitrary boolean"
  (make-coarbitrary
   (fn [a gen]
     (generator/variant (if a 0 1) gen))))

(def arbitrary-integer
  "Arbitrary integer."
  (make-arbitrary
   (generator/sized
    (fn [n]
      (let [hi (expt 4 n)
            lo (- hi)]
        (generator/choose-integer lo hi))))))

(def coarbitrary-integer
  "Arbitrary integer."
  (make-coarbitrary
    (fn [n gen]
      (generator/variant (if (>= n 0)
                           (* 2 n)
                           (+ (* 2 (- n)) 1))
        gen))))

(def arbitrary-natural
  "Arbitrary natural number."
  (make-arbitrary
   (generator/sized
    (fn [n]
      (generator/choose-integer 0 n)))))

(def coarbitrary-natural
  "Coarbitrary natural number"
  (fn [n gen]
    (generator/variant n gen)))

(defn arbitrary-integer-from-to
  "Arbitrary integer from range."
  [from to]
  (make-arbitrary
   (generator/sized
    (fn [n]
      (generator/choose-integer from to)))))

(defn coarbitrary-integer-from-to
  "Coarbitrary integer from range."
  [from to]
  (fn [n gen]
      (generator/variant (- n from) gen)))

; TODO can we remove this
(defn- arbitrary-int-like
  [gen to-int]
  (make-arbitrary 
   gen))

(defn- coarbitrary-int-like
  [gen to-int]
  (make-coarbitrary (fn [v rgen]
                      (generator/variant (to-int v) rgen))))

(def arbitrary-byte
  "Arbitrary byte."
  (arbitrary-int-like generator/choose-byte byte))

(def coarbitrary-byte
  "Coarbitrary byte."
  (coarbitrary-int-like generator/choose-byte byte))

(def arbitrary-short
  "Arbitrary short."
  (arbitrary-int-like generator/choose-short short))

(def coarbitrary-short
  "Coarbitrary short."
  (coarbitrary-int-like generator/choose-short short))

(def arbitrary-int
  "Arbitrary int."
  (arbitrary-int-like generator/choose-int int))

(def coarbitrary-int
  "Coarbitrary int."
  (coarbitrary-int-like generator/choose-int int))

(def arbitrary-long
  "Arbitrary long."
  (arbitrary-int-like generator/choose-long long))

(def coarbitrary-long
  "Coarbitrary long."
  (coarbitrary-int-like generator/choose-long long))

(def arbitrary-unsigned-byte
  "Arbitrary unsigned byte."
  (arbitrary-int-like generator/choose-unsigned-byte short))

(def coarbitrary-unsigned-byte
  "Coarbitrary unsigned byte."
  (coarbitrary-int-like generator/choose-unsigned-byte short))

(def arbitrary-unsigned-short
  "Arbitrary unsigned short."
  (arbitrary-int-like generator/choose-unsigned-short int))

(def coarbitrary-unsigned-short
  "Coarbitrary unsigned short."
  (coarbitrary-int-like generator/choose-unsigned-short int))

(def arbitrary-unsigned-int
  "Arbitrary unsigned int."
  (arbitrary-int-like generator/choose-unsigned-int long))

(def coarbitrary-unsigned-int
  "Coarbitrary unsigned int."
  (coarbitrary-int-like generator/choose-unsigned-int long))

(def arbitrary-unsigned-long
  "Arbitrary unsigned long."
  (arbitrary-int-like generator/choose-unsigned-long bigint))

(def coarbitrary-unsigned-long
  "Coarbitrary unsigned long."
  (coarbitrary-int-like generator/choose-unsigned-long bigint))

(def arbitrary-ascii-char
  "Arbitrary ASCII character."
  (arbitrary-int-like generator/choose-ascii-char int))

(def coarbitrary-ascii-char
  "Coarbitrary ASCII character."
  (coarbitrary-int-like generator/choose-ascii-char int))

(def arbitrary-ascii-letter
  "Arbitrary ASCII letter."
  (arbitrary-int-like generator/choose-ascii-letter int))

(def coarbitrary-ascii-letter
  "Coarbitrary ASCII letter."
  (coarbitrary-int-like generator/choose-ascii-letter int))

(def arbitrary-printable-ascii-char
  "Arbitrary printable ASCII character."
  (arbitrary-int-like generator/choose-printable-ascii-char int))

(def coarbitrary-printable-ascii-char
  "Coarbitrary printable ASCII character."
  (coarbitrary-int-like generator/choose-printable-ascii-char int))

(def arbitrary-char
  "Arbitrary char."
  (arbitrary-int-like (generator/sized
                       (fn [n]
                         (generator/choose-char \u0000 (char (min n 0xffff)))))
                      int))

(def coarbitrary-char
  "Coarbitrary char."
  (coarbitrary-int-like (generator/sized
                         (fn [n]
                           (generator/choose-char \u0000 (char (min n 0xffff)))))
                        int))

(defn- make-rational
  [a b]
  (/ a
    (+ 1 b)))

(def arbitrary-rational
  "Arbitrary rational number."
  (make-arbitrary
    (combine-generators make-rational
      (arbitrary-generator arbitrary-integer)
      (arbitrary-generator arbitrary-natural))))

(def coarbitrary-rational
  "Coarbitrary rational number."
  (make-coarbitrary
   (fn [^clojure.lang.Ratio r gen]
     ((coarbitrary-coarbitrary coarbitrary-integer)
      (.numerator r)
      ((coarbitrary-coarbitrary coarbitrary-integer)
       (.denominator r) gen)))))

(defn- fraction
  [a b c]
  (+ a
    (float (/ b
             (+ (abs c) 1)))))

(def arbitrary-float
  "Arbitrary float."
  (make-arbitrary
   (combine-generators fraction
                       (arbitrary-generator arbitrary-integer)
                       (arbitrary-generator arbitrary-integer)
                       (arbitrary-generator arbitrary-integer))))

(def coarbitrary-float
  "Coarbitrary float."
  (make-coarbitrary
   (fn [r gen]
     (let [^clojure.lang.Ratio fr (rationalize r)]
       ((coarbitrary-coarbitrary coarbitrary-integer)
        (.numerator fr)
        ((coarbitrary-coarbitrary coarbitrary-integer)
         (.denominator fr) gen))))))

(declare coerce->generator)

(defn arbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (make-arbitrary
   (generator/choose-mixed (map #(delay (coerce->generator (force (second %))))
                                pred+arbitrary-promise-list))))

(defn coarbitrary-mixed
  "Arbitrary value from one of a list of (promises of) arbitraries."
  [pred+arbitrary-promise-list]
  (make-coarbitrary
    (fn [val gen]
      (loop [lis pred+arbitrary-promise-list
             n 0]
        (cond
          (not (seq lis)) (throw (Error. "arbitrary-mixed: value matches none of the predicates"))
          ((first (first lis)) val) (generator/variant n gen)
          :else (recur (rest lis) (+ 1 n)))))))

(defn arbitrary-one-of
  "Arbitrary value from a list of values, and equality predicate."
  [eql? & vals]
  (make-arbitrary
   (generator/choose-one-of vals)))

(defn coarbitrary-one-of
  "Coarbitrary value from a list of values, and equality predicate."
  [eql? & vals]
  (make-coarbitrary
    (fn [val gen]
      (loop [lis vals
             n 0]
        (cond
          (not (seq lis)) (throw (Error. "arbitrary-one-of: value matches none of the predicates"))
          (eql? (first lis) val) (generator/variant n gen)
          :else (recur (rest lis) (+ 1 n)))))))

(defn arbitrary-tuple
  "Arbitrary fixed-size vector."
  [& arbitrary-els]
  (make-arbitrary
    (apply combine-generators
      vector
      (map arbitrary-generator arbitrary-els))))

(defn coarbitrary-tuple
  [& coarbitrary-els]
  (make-coarbitrary
   (fn [lis gen]
     (letfn [(recurse [coarbitrary-els lis]
               (if (seq coarbitrary-els)
                 ((coarbitrary-coarbitrary (first coarbitrary-els))
                  (first lis)
                  (recurse (rest coarbitrary-els)
                           (rest lis)))
                 gen))]
       (recurse coarbitrary-els lis)))))

(defn arbitrary-record
  "Arbitrary record."
  [construct accessors & arbitrary-els]
  (make-arbitrary
   (apply combine-generators
          construct
          (map arbitrary-generator arbitrary-els))))

(defn coarbitrary-record
  "Coarbitrary record."
  [construct accessors & coarbitrary-els]
  (make-coarbitrary
    (fn [rec gen]
      (letfn [(recurse [coarbitrary-els lis]
                (if (seq coarbitrary-els)
                  ((coarbitrary-coarbitrary (first coarbitrary-els))
                    (first lis)
                    (recurse (rest coarbitrary-els) (rest lis)))
                  gen))]
        (recurse coarbitrary-els
          (map #(% rec) accessors))))))

(defn arbitrary-coll-of
  "Arbitrary collection mimicking Clojure spec's coll-of"
  [arbitrary-el & kwargs]
  (let [opts (apply hash-map kwargs)
        {kind :kind, :or {kind 'clojure.core/vector?}} opts
        list->sequence (cond
                         (= kind 'clojure.core/vector?) vec
                         (= kind 'clojure.core/list?) #(into () %)
                         (= kind 'clojure.core/set?) set)
        {count :count} opts
        {min-count :min-count, :or {min-count 0}} opts
        {max-count :max-count} opts
        generator-el (arbitrary-generator arbitrary-el)]
    (make-arbitrary
     (generator/sized
      (fn [n]
        (combine-generators
         list->sequence
        (if count
          (generator/choose-list generator-el count)
          (generator/choose-sequence-like-in-range generator-el
                                                   min-count
                                                   (if max-count max-count n)))))))))

(defn coarbitrary-coll-of
  "Coarbitrary collection mimicking Clojure spec's coll-of"
  [arbitrary-el & kwargs]
  :not-supported-yet)

(defn arbitrary-sequence-like
  "Arbitrary sequence-like container."
  [list->sequence arbitrary-el]
  (make-arbitrary
    (generator/sized
      (fn [n]
        (combine-generators list->sequence
                            (generator/choose-sequence-like-in-range (coerce->generator arbitrary-el) 0 n))))))

(defn arbitrary-sequence-like-in-range
  "Arbitrary sequence-like container."
  [list->sequence arbitrary-el lower upper]
  (make-arbitrary
   (generator/sized
    (fn [n]
      (generator/choose-sequence-like-in-range (coerce->generator arbitrary-el)
                                               lower
                                               (min (+ lower n) upper))))))

(defn coarbitrary-sequence-like
  "Coarbitrary sequence-like container."
  [choose-sequence sequence->list coarbitrary-el]
  (make-coarbitrary
    (fn [sequ gen]
      (letfn [(recurse [lis]
                (if (seq lis)
                  ((coarbitrary-coarbitrary coarbitrary-el)
                    (first lis)
                    (generator/variant 1 (recurse (rest lis))))
                  (generator/variant 0 gen)))]
        (recurse (sequence->list sequ))))))

(defn arbitrary-list
  "Arbitrary list."
  [arbitrary-el]
  (arbitrary-sequence-like #(into () %) arbitrary-el))

(defn arbitrary-list-in-range
   "Arbitrary list in range (lower,uppper)."
   [arbitrary-el lower upper]
   (arbitrary-sequence-like-in-range #(into () %) arbitrary-el lower upper))

(defn coarbitrary-list
  "Coarbitrary list."
  [coarbitrary-el]
  (coarbitrary-sequence-like generator/choose-list identity coarbitrary-el))

(defn arbitrary-vector
  "Arbitrary vector."
  [arbitrary-el]
  (arbitrary-sequence-like vec arbitrary-el))

(defn arbitrary-vector-in-range
  "Arbitrary vector in range (lower,uppper)."
  [arbitrary-el lower upper]
  (arbitrary-sequence-like-in-range vec arbitrary-el lower upper))

(defn coarbitrary-vector
  "Coarbitrary vector."
  [coarbitrary-el]
  (coarbitrary-sequence-like generator/choose-vector #(into () %) coarbitrary-el))

(def arbitrary-byte-array
  "Arbitrary byte-array."
  (arbitrary-sequence-like byte-array arbitrary-byte))

(defn arbitrary-byte-array-in-range
  "Arbitrary byte-array in range (lower,uppper)."
  [arbitrary-el lower upper]
  (arbitrary-sequence-like-in-range byte-array arbitrary-el lower upper))

(def coarbitrary-byte-array
  "coarbitrary byte-array."
  (coarbitrary-sequence-like (fn [_ n] (generator/choose-byte-array n)) #(into () %) coarbitrary-byte))

(defn arbitrary-map
  "Arbitrary map over the given arbitrary key and value."
  [arbitrary-key arbitrary-value]
  (arbitrary-sequence-like generator/map-of-tuples (arbitrary-tuple arbitrary-key arbitrary-value)))

(defn coarbitrary-map
  "coarbitrary map over the given arbitrary key and value."
  [coarbitrary-key coarbitrary-value]
  (coarbitrary-sequence-like generator/choose-map #(into () %) (coarbitrary-tuple coarbitrary-key coarbitrary-value)))

(defn arbitrary-set
  "Arbitrary set."
  [arbitrary-el]
  (arbitrary-sequence-like set arbitrary-el))

(defn coarbitrary-set
  "Coarbitrary set."
  [coarbitrary-el]
  (coarbitrary-sequence-like generator/choose-set #(into () %) coarbitrary-el))

(def arbitrary-ascii-string
  "Arbitrary string of ASCII characters."
  (arbitrary-sequence-like #(apply str %) arbitrary-ascii-char))

(defn arbitrary-ascii-string-in-range
  "Arbitrary string of ASCII characters in range (lower,upper)."
  [lower upper]
  (arbitrary-sequence-like-in-range #(apply str %) arbitrary-ascii-char lower upper))

(def coarbitrary-ascii-string
  "Coarbitrary string of ASCII characters."
  (coarbitrary-sequence-like #(apply str %) #(into () %) coarbitrary-ascii-char))

(def arbitrary-printable-ascii-string
  "Arbitrary string of printable ASCII characters."
  (arbitrary-sequence-like #(apply str %) arbitrary-printable-ascii-char))

(defn arbitrary-printable-ascii-string-in-range
  "Arbitrary string of printable ASCII characters in range (lower,upper)."
  [lower upper]
  (arbitrary-sequence-like-in-range #(apply str %) arbitrary-printable-ascii-char lower upper))

(def arbitrary-string
  "Arbitrary string."
  (arbitrary-sequence-like #(apply str %) arbitrary-char))

(defn arbitrary-string-in-range
  "Arbitrary string in range (lower,upper)."
  [lower upper]
  (arbitrary-sequence-like-in-range #(apply str %) arbitrary-char lower upper))

(def coarbitrary-string
  "Coarbitrary string."
  (coarbitrary-sequence-like #(apply str %) #(into () %) coarbitrary-char))

(defn- arbitrary-symbol-like
  [choose]
  (make-arbitrary
   (generator/sized (fn [n] (choose n)))))

(defn- coarbitrary-symbol-like
  [choose]
  (make-coarbitrary
    (fn [v gen]
      ((coarbitrary-coarbitrary coarbitrary-string) (name v) gen))))

(def arbitrary-symbol
  "Arbitrary symbol."
  (arbitrary-symbol-like generator/choose-symbol))

(def coarbitrary-symbol
  "Coarbitrary symbol."
  (coarbitrary-symbol-like generator/choose-symbol))

(def arbitrary-keyword
  "Arbitrary keyword."
  (arbitrary-symbol-like generator/choose-keyword))

(def coarbitrary-keyword
  "Coarbitrary keyword."
  (coarbitrary-symbol-like generator/choose-keyword))

(defn arbitrary-function
  "Arbitrary function."
  [arbitrary-result & coarbitrary-args]
  (let [coarbitrary-arg-tuple (apply coarbitrary-tuple coarbitrary-args)]
    (make-arbitrary
     (generator/promote
      (fn [& args]
        ((coarbitrary-coarbitrary coarbitrary-arg-tuple)
         args
         (arbitrary-generator arbitrary-result)))))))

(defn coarbitrary-function
  "Coarbitrary function."
  [coarbitrary-result & arbitrary-args]
  (let [arbitrary-arg-tuple (apply arbitrary-tuple arbitrary-args)]
    (make-coarbitrary
     (fn [func gen]
       (monad/monadic
        [args (arbitrary-generator arbitrary-arg-tuple)
         t
         ((coarbitrary-coarbitrary coarbitrary-result)
          (apply func args)
          gen)]
        (monad/return t))))))

(defn symbol->arbitrary
  [sym]
  (cond
    (= sym `integer?) arbitrary-integer
    (= sym `string?) arbitrary-string
    (= sym `keyword?) arbitrary-keyword))

(defn symbol->coarbitrary
  [sym]
  (cond
    (= sym `integer?) coarbitrary-integer
    (= sym `string?) coarbitrary-string
    (= sym `keyword?) coarbitrary-keyword))

(defn fn->arbitrary
  [fun]
  (cond
    (= fun integer?) arbitrary-integer
    (= fun string?) arbitrary-string
    (= fun keyword?) arbitrary-keyword))

(defn fn->coarbitrary
  [fun]
  (cond
    (= fun integer?) coarbitrary-integer
    (= fun string?) coarbitrary-string
    (= fun keyword?) coarbitrary-keyword))

(defn set->arbitrary
  "Make an arbitrary from a set (behaviour like enum)"
  [s]
  (apply arbitrary-one-of (into [identity] s)))

(defn set->coarbitrary
  "Make a coarbitrary from a set (behaviour like enum)"
  [s]
  (apply coarbitrary-one-of (into [identity] s)))

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

(def-record ^{:doc "QuickCheck property"}
  Property-type
  [property-func
   property-arg-names
   ;; (seq (union arbitrary generator))
   property-args])

(defn make-property
  [func arg-names args]
  (Property-type property-func func
                 property-arg-names arg-names
                 property-args args))

(defmacro property
  "Create a property through binding identifiers to arbitraries.

The clauses are a vector of alternating identifiers and arbitraries,
which are implicitly in the syntax understood by the arbitrary macro.

The body can use the identifiers, and should evaluate to a boolean
saying whether the property is satisfied."
  [clauses body0 & bodies]
  (when (odd? (count clauses))
    (throw (Exception. "Odd number of elements in property bindings.")))
  (let [pairs (partition 2 clauses)
        ids (map first pairs)
        rhss (map second pairs)]
    `(make-property
       (fn [~@ids]
         ~body0 ~@bodies)
       '~ids
       (list ~@(map (fn [rhs] `(arbitrary ~rhs)) rhss)))))

(def-record ^{:doc "Result from a QuickCheck run."} 
  Check-result-type
  [
   ;; nil = unknown, true, false
   check-result-ok
   check-result-stamp
   ;; (list (list (pair (union #f symbol) value)))
   check-result-arguments-list
   check-result-error-set])

(defn make-check-result
  [ok stamp arguments-list error-set]
  (Check-result-type check-result-ok ok
                     check-result-stamp stamp
                     check-result-arguments-list arguments-list
                     check-result-error-set error-set))

(defn check-result?
  [x]
  (is-a? Check-result-type x))

(defn- result-add-stamp
  [res stamp]
  (assoc res check-result-stamp (conj (check-result-stamp res) stamp)))

; result (list (pair (union #f symbol) value)) -> result
(defn result-add-arguments
  [res args]
  (assoc res check-result-arguments-list
    (conj (check-result-arguments-list res) args)))

(defn result-add-errors
  [res args]
  (assoc res check-result-error-set
         (conj (check-result-arguments-list res) args)))

(defn result-mapped
  "Monoidal plus of result."
  [result1 result2]
  (assert (check-result? result1))
  (assert (check-result? result2))
  (cond
    (check-result-ok result1) result2
    :else result1))

(defn result-add-argument-if-empty
  [res arg]
  (assert (check-result? res))
  (cond
    (empty? (check-result-arguments-list res)) (result-add-arguments res arg)
    :else res))

(def nothing
  (make-check-result nil [] [] #{}))

; A testable value is one of the following:
; - a Property object
; - a boolean
; - a Result record
; - a generator of a Result record
(declare for-all-with-shrink-with-names)

(defn coerce->result-generator
  "Coerce an object to a result generator."
  [thing]
  (cond
    (is-a? Property-type thing) (for-all-with-shrink-with-names (property-func thing)
                                                                    (property-arg-names thing)
                                                                    (property-args thing))
    (instance? Boolean thing) (monad/return (assoc nothing check-result-ok thing))
    (is-a? Check-result-type thing) (monad/return thing)
    :else thing )); 
  
(defn coerce->generator
  "Coerce an object to a generator."
  [thing]
  (if (is-a? Arbitrary-type thing)
    (arbitrary-generator thing)
    thing))

#_(defn for-all
  "Bind names to generated values."
  [func & args]
    (monad/monadic
      [args (monad/sequ (map coerce->generator args))
       res (coerce->result-generator (apply func args))]
      (monad/return (result-add-arguments res
                      (map #(conj % nil) args)))))

#_(defn for-all-with-names
  "Bind names to generated values, supplying informative names."
  [func arg-names args]
  (monad/monadic
    [args (monad/sequ (map coerce->generator args))
     res (coerce->result-generator (apply func args))]
    (monad/return (result-add-arguments res (map list arg-names args)))))

(defn find-failing
  [smaller func]
  (monad/monadic
   [results (monad/sequ
             (mapv coerce->result-generator (mapv (fn [args]
                                                    (try (apply func args)
                                                         (catch
                                                             Exception e
                                                           (make-check-result false
                                                                              []
                                                                              []
                                                                              #{(.toString e)}))))
                                                  (mapv tree/tree-outcome smaller))))]
   (let [failingResults (filter (fn [[_ result]] (not (check-result-ok result)))
                                (mapv vector smaller results))])
   (monad/return
    (cond
      (empty? failingResults) :no-failing-result
      :else (first failingResults)))))

(defn shrinking
  "
  get shrinks of args and find failing result in the resulting list
  recursive call shrinking as long as there is a failing result.
  "
  [arg-names args func fuel]
  (assert (tree/tree? args) "args has to be a tree")
  (let [children (tree/tree-shrinks args)]
    (monad/monadic
     [maybeFailingResult (find-failing children func)]
     (cond
       (or (= maybeFailingResult :no-failing-result)
           (<= fuel 0)) (monad/return (assoc nothing check-result-ok true))
       :else (monad/monadic
              (let [[shrunk, failure] maybeFailingResult])
              [result (shrinking arg-names shrunk func (- fuel 1))]
              (monad/return
               (result-add-argument-if-empty (result-mapped result failure)
                                                       [(vector arg-names (tree/tree-outcome shrunk))])))))))

(defn for-all-with-shrink-with-names
  "Bind name to generated value, try to shrink, supplying informative name.,"
  [func arg-names arg-trees]
  (assert (= (count arg-names) (count arg-trees))
          "Number of arg-names does not match number of arguments")
  (let [arg-trees (map coerce->generator arg-trees)]
    (monad/monadic
      [args-tree (with-tree (apply combine-generators vector arg-trees))
       max-shrink-depth (generator/get-max-shrink-depth)
       res (coerce->result-generator
            (try (apply func (tree/tree-outcome args-tree))
                 (catch Exception e (make-check-result false
                                                       []
                                                       []
                                                       #{(.toString e)}))))]
      (let [result (result-add-arguments res [(vector arg-names (tree/tree-outcome args-tree))])])
      [maybe-shrunken-result (cond
                               (check-result-ok result) (monad/return result)
                               :else (shrinking arg-names args-tree func max-shrink-depth))]
      (monad/return (result-mapped maybe-shrunken-result result)))))

(defmacro ==>
  "Create a property that only has to hold when its prerequisite holds."
  [?bool ?prop]
  `(if ~?bool
     ~?prop
     (monad/return nothing)))

(defn label
  "Label a testable value."
  [str testable]
  (monad/monadic
    [res (coerce->result-generator testable)]
    (monad/return (result-add-stamp res str))))

(defmacro classify
  "Classify some test cases of a testable."
  [?really? ?str ?testable]
  `(let [testable# ~?testable]
     (if ~?really?
       (label ~?str testable#)
       testable#)))

(defmacro trivial
  "Classify some test cases of a testable as trivial."
  [?really? ?testable]
  `(classify ~?really? "trivial" ~?testable))

(defn collect
  "Label a testable value with an the string representation of an object."
  [lbl testable]
  (label (str lbl) testable))

; Running the whole shebang

(def-record ^{:doc "Configuration for a series of QuickCheck test runs."}
  Config-type
  [make-config-max-test
   make-config-max-fail
   make-config-max-shrink-depth
   make-config-size
   make-config-print-every])

(defn make-config
  [max-test max-fail max-shrink-depth size print-every]
  (Config-type
   make-config-max-test max-test
   make-config-max-fail max-fail
   make-config-max-shrink-depth max-shrink-depth
   make-config-size size
   make-config-print-every print-every))

(def quick
  "Quick test-run configuration with minimal output."
  (make-config
    100
    1000
    30
    #(+ 3 (quot % 2))
    (fn [n args] nil)))

(def verbose
  "Quick test-run configuration with verbose output."
  (make-config
    100
    1000
    30
    #(+ 3 (quot % 2))
    (fn [n args]
      (print n)
      (println ":")
      (doseq [x args] (println x)))))

(declare tests)

(defn check-results
  "Run a property against a configuration and return results."
  [config prop]
  (let [rgen (make-random-generator 0)]
    (tests config (coerce->result-generator prop) rgen 0 0 '())))

(declare report-result)
(defn check
  "Run a property against a configuration and report results."
  [config prop]
  (let [[ntest stamps maybe-result]  (check-results config prop)]
    (report-result ntest stamps maybe-result)))

(defn quickcheck-results
  "Run a property against the `quick' configuration and return results."
  [prop]
  (check-results quick prop))

(defn quickcheck
  "Run a property against the `quick' configuration and report results."
  [prop]
  (check quick prop))

#_(def ntries 20)

#_(defn counter-example-of-size [gen size rgen]
  (loop [i ntries
         rgen rgen]
    (when-not (zero? i)
      (let [result (generate size rgen gen 20)
            next-rgen (first (random-generator-split rgen))]
        (case (check-result-ok result)
          true (recur (dec i) next-rgen)
          false result
          )))))

#_(clojure.test/deftest counter-example-of-size-t
  (let [prop (property [x (list integer)]
                       (every? even? x))
        gen (coerce->result-generator prop)
        rgen (make-random-generator 0)
        cex (counter-example-of-size gen 1 rgen)
        ls (-> cex
               (check-result-arguments-list)
               (first) (first) (second))]
    (clojure.test/is (= 1 (count ls)))))

#_(defn smallest-counter-example [gen size example rgen]
  (loop [size (dec size)
         smallest-example example
         rgen (second (random-generator-split rgen))]

    (if (zero? size)
      smallest-example
      (let [new-example (counter-example-of-size gen size rgen)]
        (if new-example
          ;; go smaller
          (recur (dec size) new-example (second (random-generator-split rgen)))
          ;; done
          smallest-example
          )))))

#_(clojure.test/deftest smallest-counter-example-t
  (let [prop (property [x (list integer)]
                       (every? even? x))
        gen (coerce->result-generator prop)
        rgen (make-random-generator 0)
        cex (smallest-counter-example gen 50 :failed rgen)
        ls (-> cex
               (check-result-arguments-list)
               (first) (first) (second))]
    (clojure.test/is (= 1 (count ls)))))

(defn- tests
  "Run a series of test runs.

returns three values:
- ntest
- stamps
- true for success, false for exhausted, result for failure"
  [config gen rgen ntest nfail stamps]
  (loop [rgen rgen
         ntest ntest
         nfail nfail
         stamps stamps]
    (cond
      (= ntest (make-config-max-test config)) (list ntest stamps true)
      (= nfail (make-config-max-fail config)) (list nfail stamps false)
      :else
      (let [[rgen1 rgen2] (random-generator-split rgen)
            size ((make-config-size config) ntest)
            result (generator/generate size rgen2 gen (make-config-max-shrink-depth config))]
        ((make-config-print-every config) ntest (check-result-arguments-list result))
        (case (check-result-ok result)
          nil (recur rgen1 ntest (+ 1 nfail) stamps)
          true (recur rgen1 (+ 1 ntest) nfail (conj stamps (check-result-stamp result)))
          false
          ;; found a counter-example of size size. we now try to
          ;; recursively find a smaller counter-example of size (dec
          ;; size)
          [ntest stamps result]
          #_(let [smallest-result (smallest-counter-example gen size result rgen)]
            [ntest stamps smallest-result]))))))

(declare done write-arguments)

(defn- report-result
  "Report the result of a series of test runs."
  [ntest stamps maybe-result]
  (case maybe-result
    true (done "OK, passed" ntest stamps)
    false (done "Arguments exhausted after" ntest stamps)
    (do
      (print "Falsifiable, after ")
      (print ntest)
      (println " tests:")
      (doseq [a (check-result-arguments-list maybe-result)]
        (write-arguments a))
      (when (not (empty? (check-result-error-set maybe-result)))
        (println "errors:")
        (doseq [error (check-result-error-set maybe-result)]
          (println error))))))

; (pair (union nil symbol) value)
(defn- write-argument
  "Print out an argument binding."
  [arg]
  (when (first arg)
    (print (first arg))
    (print " = "))
  (print (map str (second arg))))

; (list (pair (union nil symbol) value))
(defn- write-arguments
  "Print out a list of argument bindings."
  [args]
  (when (seq args)
    (write-argument (first args))
    (doseq [arg (rest args)]
      (print " ")
      (write-argument arg))
    (newline)))

(declare group-sizes stamp<?)

(defn- done
  "Print out final report."
  [mesg ntest stamps]
  (print mesg)
  (print " ")
  (print ntest)
  (print " tests")
  (let [sorted (sort stamp<? (filter #(and (seq? %) (seq %)) stamps))
        grouped (group-sizes sorted)
        entries (map (fn [p]
                       (let [n (first p)
                             lis (rest p)]
                         (str (quot (* 100 n) ntest)
                           "% "
                           (clojure.string/join ", " lis))))
                  (sort (fn [p1 p2]
                          (> (first p1) (first p2)))
                    grouped))]
    (cond
      (not (seq entries)) (println ".")
      (not (seq (rest entries))) (do
                                   (print " (")
                                   (print (first entries))
                                   (println ")."))
      :else
      (do
        (println ".")
        (doseq [entry entries]
          (print entry)
          (println "."))))))


(defn- group-sizes
  "Compute class-group sizes."
  [lis]
  (if (not (seq lis))
    []
    (loop [current (first lis)
           size 1
           lis (rest lis)
           res []]
      (cond
        (not (seq lis)) (reverse (conj res (list size current)))
        (= current (first lis)) (recur current (+ 1 size) (rest lis) res)
        :else
        (recur (first lis) 1 (rest lis) (conj res (list size current)))))))

(defn- stamp<?
  "Compare two stamps."
  [s1 s2]
  (cond
    (not (seq s1)) (seq s2)
    (not (seq s2)) true
    :else
    (let [c (compare (first s1) (first s2))]
      (cond
        (< c 0) true
        (= c 0) (stamp<? (rest s1) (rest s2))
        :else false))))

(defn ^:no-doc quickcheck-report [msg prop-sexpr result]
  (let [[ntests stamps success] result]
    (case success
      true (do-report {:type :pass, :message msg,
                       :expected prop-sexpr})
      false (do-report {:type :fail, 
                        :message (str "Arguments exhausted after " ntests " tries"),
                        :expected prop-sexpr, :actual false})
      (do-report {:type :fail,
                  :message (str "falsifiable")
                  :expected prop-sexpr
                  :actual (check-result-arguments-list success)}))))

(defmethod assert-expr 'quickchecked [msg form]
  ;; (is (quickchecked prop))
  ;; Asserts that the property passes the QuickCheck tests
  (let [prop (second form)]
    `(quickcheck-report ~msg '~prop (quickcheck-results ~prop))))

(defmethod assert-expr 'quickcheck [msg form]
  ;; deprecated alias of quickchecked.
  (assert-expr msg (apply list 'quickchecked (rest form))))

;; --- Distribution tools ---------

(let [incc (fn [i]
             (if i (inc i) 1))]

  (defn ^:no-doc occurrences [stamps]
    (reduce (fn [acc labels]
              (reduce (fn [acc label]
                        (update acc label incc)) acc labels))
            {} stamps)))

(let [map-values (fn [f m]
                   (into {} (for [[k v] m] [k (f v)])))]

  (defn ^:no-doc distribution [stamps]
    (let [n (count stamps)]
      (map-values (fn [x] (/ x n))
                  (occurrences stamps)))))

(defn ^:no-doc fraction-of [label stamps]
  (let [d (distribution stamps)]
    (or (get d label) 0)))

(defn ^:no-doc distributed?
  "Check stamp distribution"
  [stamps required-distribution]
  (let [actual-distribution (distribution stamps)]
    (every? (fn [[label lower-bound]]
              (>= (get actual-distribution label) lower-bound))
            required-distribution)))

(defn ^:no-doc with-distribution-report [msg prop-sexpr pairs result]
  (let [[ntests stamps success] result
        d? (distributed? stamps pairs)]
    (if (and success (not d?))
      (do-report {:type :fail, :message "Distribution requirements not met",
                  :expected pairs
                  :actual (distribution stamps)})
      (quickcheck-report msg prop-sexpr result))))

(defmethod assert-expr 'with-distribution [msg form]
  ;; (is (with-distribution [label fraction ...] (quickcheck prop)))
  ;; Asserts that the property passes the QuickCheck tests
  ;; and meets the required label distribution
  (let [pairs (drop-last (drop 1 form))
        qcform (last form)]
    (let [prop (second qcform)]
      `(with-distribution-report ~msg '~prop (hash-map ~@pairs) (quickcheck-results ~prop)))))
