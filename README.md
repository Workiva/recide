# Recide [![Clojars Project](https://img.shields.io/clojars/v/com.workiva/recide.svg)](https://clojars.org/com.workiva/recide) [![CircleCI](https://circleci.com/gh/Workiva/recide/tree/master.svg?style=svg)](https://circleci.com/gh/Workiva/recide/tree/master)

*recide, second-person singular present imperative of `recidere`:* - to fall back, come to naught; to curtail

<!-- toc -->

- [Overview](#overview)
- [API Documentation](#api-documentation)
- [Assertions](#assertions)
- [Constructing and raising "typed" ex-infos](#constructing-and-raising-typed-ex-infos)
  * [`assoc` / `update` into recide errors' ex-data](#assoc--update-into-recide-errors-ex-data)
  * [ex-info ⇒ persistent map ⇒ ex-info](#ex-info-%E2%87%92-persistent-map-%E2%87%92-ex-info)
- [Well-defined error types](#well-defined-error-types)
  * [`deferror`](#deferror)
  * [`deferror-group`](#deferror-group)
- [Enhanced try-catch](#enhanced-try-catch)
- [Exception Sanitization Tools](#exception-sanitization-tools)
  * [recide.sanex.Utils](#recidesanexutils)
    + [`getCurrentSanitizationLevel()`](#getcurrentsanitizationlevel)
    + [`createSuppressionMap(...)`](#createsuppressionmap)
    + [`sanitize(Throwable)`, `sanitize(Throwable, IPersistentMap)`](#sanitizethrowable-sanitizethrowable-ipersistentmap)
- [Customized `ErrorForm`](#customized-errorform)
- [Maintainers and Contributors](#maintainers-and-contributors)
  * [Active Maintainers](#active-maintainers)
  * [Previous Contributors](#previous-contributors)

<!-- tocstop -->

## Overview

Clojure's [`ex-info`](https://clojuredocs.org/clojure.core/ex-info) is a very useful construct: you can attach a map of arbitrary data to a thrown exception, allowing code that has caught the exception to examine, log, or otherwise process this [`ex-data`](https://clojuredocs.org/clojure.core/ex-data).

For example, [`assert`](https://clojuredocs.org/clojure.core/assert) statements provide valuable sanity checks in business logic, but when they fail it is usually highly desirable to know *how* they failed. One could attempt to insert this information into the exception string, but sometimes the relevant data is much too large for a wieldy exception message. Instead checking for the desired property and throwing an `ex-info` with all the relevant data attached can preserve the succinctness of error messages while saving the developer enormous amounts of time, especially when debugging at the REPL.

One of the principal weaknesses of `ex-info` is that its use can encourage ad-hoc exceptions with no standards: if you catch an ex-info, its [type](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/ExceptionInfo.java) is the same as all other ex-infos, its string is arbitrary, and not only are you unable to count on any particular key appearing in the `ex-data`, it's quite possible that the map is entirely empty.

If you want to enjoy the benefits of using `ex-info` widely in a large project, but you also want to retain ~~a measure of sanity~~ the benefits of well-defined errors, it is likely that you will eventually resort, in each logical component of your application, either to [breaking with universal Clojure idioms](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/IExceptionInfo.java#L16) or to defining your own defining a standard set of "throwing functions" which use `ex-info` but guarantee a certain rigidity: perhaps a common prefix to the exception strings, perhaps certain guaranteed keys in the [`ex-data`](https://clojuredocs.org/clojure.core/ex-data) maps.

The primary purpose of this library, **recide**, is to provide tools to ease this process. It provides utilities for defining standard ex-info forms, as well as the capacity for checking *at compile-time* that they are being used as intended.

All `ex-info` maps generated by tools in `recide` contain at least two keys:

- `:recide/error`, whose value is an instance of [`ErrorForm`](src/recide/error.clj#L13).
- a "type key" defined by the `ErrorForm` (default in recide is `:recide/type`).

## API Documentation

[Clojure API documentation can be found here.](/documentation/clojure/index.html)
[Java API documentation can be found here.](/documentation/java/index.html)

## Assertions

`recide.core/insist` is analogous to `assert`. Its signature is the same, and just like `assert` it only executes when `clojure.core/*assert*` is true.

But instead of throwing an [`AssertionError`](https://docs.oracle.com/javase/8/docs/api/java/lang/AssertionError.html), it throws an `ex-info` with an explanatory string of the form: "Assertion failed: \<asserted expression or supplied message\>". The type in the ex-data is `:recide/assertion`. There are two other keys used by `insist`:

 - `:expression`, whose value is the actual [expression](https://clojure.org/reference/evaluation) contained in the `insist`
 - `:values`, whose value is a map from each variable used in the expression to its value at the time of failure.

 `:values` is only present whenever `recide.impl/*capture-insists*` is true, but it is false by default. At library loading time, it is set to true if at least one of the following is true:
 
1. The RECIDO\_CAPTURE\_INSISTS env var is true
2. The system property recide.capture-insists is set to true

The signature of `insist` is `[[expr] [expr message]]`, and the ex-data in the resulting ex-info has the following form:

```clojure
 {:recide/type :recide/assertion,
  :expression <expr>
  :values { ... }}
```

Example in use:

```clojure
(let [y not
      x true]
   (insist (y x)))

;; Unhandled clojure.lang.ExceptionInfo
;;   Assertion failed: (y x)
;;      {:expression (y x),
;;       :values {y #function[clojure.core/not],
;;                x true},
;;       :recide/type :recide/assertion}
```

## Constructing and raising "typed" ex-infos

`recide.core/error` has two arities: `([type msg data] [type msg data cause])`. `error` constructs an `ex-info` with the map `data` attached, whose type (again, signified by `:recide/type` by default) is `type`. Supplying a `cause` merely gives the exception a cause according to Java idiom.

```clojure
(let [x "not b! haha"]
   (raise :my-type
          "my explanation!"
          {:a "a"
           :b x}))
	
;; #error {
;;  :cause "my explanation!"
;;  :data {:a "a",
;;         :b "not b! haha",
;;         :recide/type :my-type, :recide/error #object[...]}
;;  :via
;;  [{:type clojure.lang.ExceptionInfo
;;    :message "my explanation!"
;;    :data {:a "a",
;;           :b "not b! haha",
;;           :recide/type :my-type, :recide/error #object[...]}
;;    :at [clojure.core$ex_info invokeStatic "core.clj" 4725]}]
;;  :trace ... }   
```

`recide.core/raise` has the same two arities. `raise` throws the exception constructed by `error`.

### `assoc` / `update` into recide errors' ex-data

Convenience functions: `recide.core/assoc-error` and `recide.core/update-error` each return new exceptions of the original type, with the ex-data modified as with `assoc` and `update`.

### ex-info ⇒ persistent map ⇒ ex-info

Sometimes it can be convenient to pass around and manipulate a map representation of an error before turning it back into an exception and throwing it. To this end, we provide `recide.core/error->map` and `recide.core/error-map->throwable` which do as you would expect.

Additionally, we provide an alternative `error-map` "constructor" for errors that returns the map form, and the corresponding predicate `error-map?`.

For any error of type `type` with message `msg`, ex-data `data`, and cause `cause`, the persistent map looks like this:

```clojure
{:recide/error  <ErrorForm>,
 :recide/type   type,
 :recide/msg    msg,
 :recide/cause  cause,
 :recide/data   data}
```

All these keys except `:recide/error` can be modified by supplying a customized `ErrorForm` (see below for details).

## Well-defined error types

`raise` and `error` do very little to provide standard exception types. To address this further, recide provides `deferror` and `deferror-group`.

### `deferror`

`deferror` is a macro which takes an error-name, a type, and a "generic" string that will prefix the messages of all errors of this type. It also, optionally, takes a collection of required keys. If required keys are specified, then *compile-time* errors will be thrown any time the tools defined by `deferror` are used without specifying those keys explicitly in the source code.

Example usage:

```clojure
(deferror storage-timeout
   :storage/timeout
   "A storage operation timed out"
   [:method-at-fault :timeout-ms])
```

In this example, this call to `deferror` will define two new macros, `storage-timeout` and `raise-storage-timeout`. For your convenience, any competent IDE will be able to access detailed docstrings on these new vars:

```clojure
> (clojure.repl/doc storage-timeout)
;; -------------------------
;; my-ns/storage-timeout
;; [[detail-str data] [detail-str data cause]]
;; Macro
;;   Records this raise-site under :storage/timeout in recide, and expands into the equivalent of:
;; 
;;          (ex-info (str "A storage operation timed out: " detail-str)
;;                   (assoc data :recide/type :storage/timeout)
;;                   cause)
;; 
;;   The following keys are required in the data-map:
;;  #{:method-at-fault,
;;    :timeout-ms}

> (clojure.repl/doc raise-storage-timeout)
;; -------------------------
;; [[detail-str data] [detail-str data cause]]
;; Macro
;;   Records this raise-site under :storage/timeout in recide, and expands into:
;; 
;;          (raise :storage/timeout
;;                 (str "A storage operation timed out: " detail-str)
;;                 data
;;                 cause)
;; 
;;   The following keys are required in the data-map:
;;  #{:method-at-fault,
;;    :timeout-ms}
```

If you attempt to use either of these without, in your data map, specifying each of the required keys, the Clojure *compiler* will throw an exception:

```clojure
> (raise-storage-timeout "blah" {:method-at-fault 'not-really-a-method})

;; Unhandled clojure.lang.ExceptionInfo
;;    Assertion failed: storage-timeout requires the following missing
;;       keys in its data: :timeout-ms
```

### `deferror-group`

`deferror-group` is a macro which defines a whole *family* of errors. It takes an error-name, a declaration of base type, and some number of subtype declarations.

Each **base type declaration** must be *either* a keyword representing the common namespace for this group of errors, *or* a tuple whose first element is such a keyword and whose second element is a sequence of required keys. Keys specified here will be required in every subtype.

Each **subtype declaration** consists of a sequence whose first term is a symbol, second term is a generic string for the error, and third (optional) term is a sequence of required keys for that subtype.

Example:

```clojure
(deferror-group parse-err
   (:query.invalid [:expression])
   (find-spec "Invalid find spec")
   (inputs "Invalid inputs" [:invalid]))
```

In this example, there are two error types defined: `:query.invalid/find-spec` and `:query.invalid/inputs`. The first requires `:expression` in its data map, but the second requires both `:expression` and `:invalid`.

As with `deferror`, the utilities produced by `deferror-group` have detailed docstrings:

```clojure
> (clojure.repl/doc parse-err)
;; -------------------------
;; recide/parse-err
;; [[subtype detail-str data] [subtype detail-str data cause]]
;; Macro
;;   Records this raise-site under :query.invalid/<subtype> in recide, and expands into the
;;             equivalent of:
;; 
;;          (ex-info (str "<subtype-generic-str>: " detail-str)
;;                   (assoc data
;;                          :recide/type
;;                          :query.invalid/<subtype>)
;;                   cause)
;; 
;; The following map shows, for each subtype, what keywords are required in
;; the data map, and what the generic portion of the string will be:
;; 
;; {:find-spec {:required #{:expression},
;;              :generic-str "Invalid find spec"},
;;  :inputs {:required #{:expression :invalid},
;;           :generic-str "Invalid inputs"}}

> (clojure.repl/doc raise-parse-err)
;; -------------------------
;; recide/raise-parse-err
;; [[subtype detail-str data] [subtype detail-str data cause]]
;; Macro
;;   Records this raise-site under :query.invalid/<subtype> in recide, and expands into:
;; 
;;          (raise :query.invalid/<subtype>
;;                 (str "<subtype-generic-str>: " detail-str)
;;                 data
;;                 cause)
;; 
;; The following map shows, for each subtype, what keywords are required in
;; the data map, and what the generic portion of the string will be:
;; 
;; {:find-spec {:required #{:expression},
;;              :generic-str "Invalid find spec"},
;;  :inputs {:required #{:expression :invalid},
;;           :generic-str "Invalid inputs"}}
```

As seen before, required keys generate *compile-time* errors when omitted.

```clojure
> (raise-parse-err :inputs "detailed this, detailed that" {:expression nil})

;; Unhandled clojure.lang.ExceptionInfo
;;    Assertion failed: parse-err called with subtype :inputs requires
;;       the following missing keys in its data: :invalid
```

## Enhanced try-catch

If we are using keywords to designate error types, it would be useful to be able to `catch` errors by means of these keywords. Recide provides `try*` for this purpose. `try*` is a macro that expands to Clojure's `try`, which is one of Clojure's few [special forms](https://clojure.org/reference/special_forms). In most cases, `try*` then should behave exactly like `try`. It differs in that it exposes enhanced `catch` functionality. You can catch:

- Classes/Interfaces (whatever will satisfy an [`instance?`](https://clojuredocs.org/clojure.core/instance_q) check.
- keywords (recide error types -- regardless of `ErrorForm` used to construct them)
- arbitrary predicates (of one argument -- the Throwable)

```clojure
recide.core/try*
[(try* expr* catch-clause* finally-clause?)]
Macro
  Expands to Clojure's try Special Form, allowing for enhanced `catch` clauses:
  You can catch:
       * Classes/Interfaces (represents an instance? check)
         `(catch RuntimeException e ...)`
       * keywords (recide error types; fully-qualified: :namspace/name, wildcard: :namespace/*)
         `(catch :library/error e ...)`
       * arbitrary predicates
         `(catch bad-error? e ...)`
  You can also catch conjunctions/disjunctions of these:
       * conjunction
         `(catch :and [RuntimeException :library/error bad-error?] e ...)`
       * disjunction
         `(catch :or [IllegalArgumentException :library/error bad-error?] e ...)`
  You can also negate each of these:
       `(catch (:not RuntimeException) e ...)`
       `(catch :and [(:not RuntimeException) :library/*] e ...)`
  Otherwise, behavior should match 'normal' catch clauses in `clojure.core/try`.
```

Note that you can use keywords of the form `:namespace/*` as wildcards to catch families of recide errors, such as those defined by `deferror-group`.

```clojure
> (try* (raise :genus/species-1
               "went extinct"
	       {:year -1839421})
	(catch :genus/* e
	   (println (:year (ex-data e)))))
;; -1839421
```

## Exception Sanitization Tools

Recide provides a collection of tools to acquire a [_sanitized_](/java-src/recide/sanex/ISanitized.java) version of a exception that should be considered safe to log (but may not be useful as a result).

### recide.sanex.Utils

This class contains a handful of static utility methods:

#### `getCurrentSanitizationLevel()`

Equivalent to deref'ing `recide.sanex/*sanitization-level*`.

#### `createSuppressionMap(...)`

Creates an IPersistentMap with the appropriate keywords corresponding to the boolean args.

#### `sanitize(Throwable)`, `sanitize(Throwable, IPersistentMap)`

Shortcut to Clojure IFn `recide.sanex/sanitize`.

## Customized `ErrorForm`

By default, errors raised by this library use `ex-info` as a constructor, `recide.utils/serialize-throwable` and `recide.utils/deserialize-throwable` for (de)serialization, and in map form they use `:recide/type`, `:recide/msg`, `:recide/data`, and `:recide/cause` as their standard keywords.

By defining a new [`ErrorForm`](src/recide/error.clj#L13), you can change all of this behavior for your own library. By modifying the keywords, you can "brand" errors coming out of your library. You can swap out `ex-info` for another constructor of the same arity, which will be expected to return an [`IExceptionInfo`](https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/IExceptionInfo.java). For instance, concerns related to Java interop might motivate creating a new exception class, while Clojure idioms might motivate retaining ex-info compatibility.

You can easily define your customization with `recide.core/def-error-form`, overriding only the methods you wish to override; methods not specified default to recide library defaults.

The error-*handling* methods in `recide.core` are agnostic to the specific `ErrorForm` used. To *create* errors using a custom `ErrorForm`, you can easily generate a full suite of recide methods tailored *specifically* to your customizations with `recide.core/generate-library!`. 

```clojure
(ns my-library.error
    (:require [recide.core :as rc]))

(rc/def-error-form custom-error-form
   (type-kw [_] :my-library/type)
   (constructor [_] my-library/error-constructor)
   ;; all other methods are filled out with recide defaults.

(def ^:dynamic *capture-insists?* true)

(rc/generate-library! custom-error-form *capture-insists?*)
                      
;; recide.core/generate-library!
;;  [custom-error capture-flag]
;; Macro
;;   Generates and defs custom versions of the following recide.core methods, tailored specifically
;;   to custom-error, with variable capture in the generated insist subject to capture-flag.
;;     * error
;;     * error?
;;     * error-map
;;     * error-map?
;;     * throwable->error-map
;;     * raise
;;     * insist
;;     * deferror
;;     * deferror-group
;; 
;;  custom-error should be an instance of recide.error/ErrorForm (see def-error-form).
;;  capture-flag must be resolvable to a dynamic var.
```

## Maintainers and Contributors

### Active Maintainers

-

### Previous Contributors

- Timothy Dean <galdre@gmail.com>
- Houston King <houston.king@workiva.com>
- Ryan Heimbuch <ryan.heimbuch@workiva.com>
