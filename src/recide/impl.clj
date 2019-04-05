;; Copyright 2016-2019 Workiva Inc.
;; 
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;; 
;;     http://www.apache.org/licenses/LICENSE-2.0
;; 
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

(ns recide.impl
  (:require [backtick]
            [clojure.edn]
            [clojure.set]
            [clojure.spec.alpha :as s]
            [recide.utils :as utils]
            [recide.error :as rerr]
            [utiliva.macros :refer [class-sym? sym->class]])
  (:import (clojure.lang IExceptionInfo)
           (recide.error ErrorForm)
           (java.util ArrayList)
           (java.io Serializable)))

;;;;;;;;;;;;;;;
;;; DEFAULTS:

(def default-error-definition
  (rerr/error-form {}))

(def ^:dynamic *capture-insists*
  "If true, default `insist` will capture the values of all lexical bindings
  used within the clause."
  (boolean (or (System/getenv "RECIDERE_CAPTURE_INSISTS")
               (System/getProperty "recide.capture-insists"))))

(def read-string-with-default-tagging
  (partial clojure.edn/read-string {:default tagged-literal}))

;;;;;;;;;;;;;;;;;;;;;
;; SHAPE OF ERRORS:

;; {:recide/error default-error-definition,
;;  :recide/type :some/error,
;;  :recide/msg "You done goofed.",
;;  :recide/data {:foo :bar},
;;  :recide/cause (GremlinException. "I've got more wrenches."),
;;  :recide.serialized-throwable/vX some-serialization}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CORE / DEFAULT ERROR SITES:

(defn record-raised-error-site!
  [custom-error err-type msg ns form]
  (let [site-info (-> {:recide/raised err-type}
                      (cond-> (string? msg) (assoc (rerr/message-keyword custom-error) msg))
                      (assoc :recide/location (merge {:namespace (str ns)} (meta form))))
        array-list (rerr/raised-sites custom-error)]
    (locking array-list
      (.add ^ArrayList array-list site-info))))

(defn error-map*
  "Creates the map form of a recide error with the type (keyword), msg (string), data map, and cause."
  ([custom-error type msg data] (error-map* custom-error type msg data nil))
  ([custom-error type msg data cause]
   (assert (instance? ErrorForm custom-error))
   (assert (keyword? type) (str type))
   (assert (string? msg) (str msg))
   (assert (map? data))
   (assert (or (nil? cause) (instance? Throwable cause)))
   (cond-> {:recide/error                      custom-error
            (rerr/type-keyword custom-error)     type
            (rerr/message-keyword custom-error)  msg
            (rerr/data-keyword custom-error)     data}
     cause (assoc (rerr/cause-keyword custom-error) cause))))

(defn generate-error-map*
  [custom-error]
  (let [custom-error (backtick/resolve-symbol custom-error)]
    `(defn ~'error-map
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            ["Returns a map representing a recide error with the type (keyword), msg (string), data map,"
             "and cause."
             ""
             "{%s type,"
             " %s msg,"
             " %s data,"
             " %s cause,"
             " %s <serialized-form>}"
             ""
             "ErrorForm definition: %s"
             "Constructor: %s"])
           custom-error
           (rerr/type-keyword resolved-error)
           (rerr/message-keyword resolved-error)
           (rerr/data-keyword resolved-error)
           (rerr/cause-keyword resolved-error)
           (rerr/serialized-keyword resolved-error)
           (.getName ^Class (class (rerr/constructor resolved-error)))))
       {:arglists '~'[[type msg data] [type msg data cause]]}
       ([type# msg# data#] (error-map* ~custom-error type# msg# data#))
       ([type# msg# data# cause#] (error-map* ~custom-error type# msg# data# cause#)))))

(defmacro generate-error-map [custom-error] (generate-error-map* custom-error))
(generate-error-map default-error-definition)

(defn error-map?
  "Checks whether the given map constitutes a valid recide error. If a
  keyword type is provided, this also verifies that the error is of that type.
  Matches :namespace/* as a partial wildcard for error types."
  ([x]
   (and (map? x)
        (contains? x :recide/error)))
  ([x type]
   (and (error-map? x)
        (let [custom-error (:recide/error x)
              recorded-type (->> custom-error rerr/type-keyword (get x))]
          (or (= type recorded-type)
              (and (= (name type) "*")
                   (= (namespace type) (namespace recorded-type))))))))

(defn custom-error-map?
  ([error-form x]
   (and (map? x)
        (let [tagged (:recide/error x)
              proj (juxt rerr/serialization-tag
                         rerr/type-keyword
                         rerr/message-keyword
                         rerr/data-keyword
                         rerr/cause-keyword
                         rerr/serialized-keyword
                         rerr/constructor)]
          (or (= error-form tagged)
              (and (= (juxt error-form)
                      (juxt tagged)))))))
  ([custom-error x type]
   (and (custom-error-map? custom-error x)
        (let [custom-error (:recide/error x)
              recorded-type (->> custom-error rerr/type-keyword (get x))]
          (or (= type recorded-type)
              (and (= (name type) "*")
                   (= (namespace type) (namespace recorded-type))))))))

(defn generate-error-map?*
  [custom-error]
  (let [custom-error (backtick/resolve-symbol custom-error)]
    `(defn ~'error-map?
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            (conj
             (into
              ["Checks if the given hash-map is polymorphic to a recide error AND is constructed according"
               "to the following ErrorForm:"
               " - %s"
               ""
               "So its shape should be:"
               "{%s <type>,"
               " %s <msg>,"
               " %s <data>,"
               " %s <cause>,"]
              (for [[k _] (rerr/metadata-fns resolved-error)]
                (format " %s (<%s-fn> <ex-info>),"
                        k (name k))))
             " %s <serialized-form>}"))
           custom-error
           (rerr/type-keyword resolved-error)
           (rerr/message-keyword resolved-error)
           (rerr/data-keyword resolved-error)
           (rerr/cause-keyword resolved-error)
           (rerr/serialized-keyword resolved-error)))
       {:arglists '~'[[x] [x type]]}
       ([x#] (custom-error-map? ~custom-error x#))
       ([x# type#] (custom-error-map? ~custom-error x# type#)))))

(defmacro generate-error-map? [custom-error] (generate-error-map?* custom-error))

(defn error?
  "Checks whether the given IExceptionInfo is a recide error.
  If a type keyword is provided, it also checks that the error has that type."
  ([x]
   (and (instance? IExceptionInfo x)
        (error-map? (ex-data x))))
  ([x type]
   (and (instance? IExceptionInfo x)
        (error-map? (ex-data x) type))))

(defn custom-error?
  ([custom-error x]
   (and (instance? IExceptionInfo x)
        (custom-error-map? custom-error (ex-data x))))
  ([custom-error x type]
   (and (instance? IExceptionInfo x)
        (custom-error-map? custom-error (ex-data x) type))))

(defn error-type
  "Accepts a recide error or error-map. Returns nil when it's not. Not safe for use in
  a custom constructor unless serialization/deserialization is out of the question."
  [x]
  (cond (instance? IExceptionInfo x)
        (recur (ex-data x))
        (map? x)
        (when (contains? x :recide/error)
          (->> x :recide/error rerr/type-keyword (get x)))
        :else
        nil))

(defn generate-error?*
  [custom-error]
  (let [custom-error (backtick/resolve-symbol custom-error)]
    `(defn ~'error?
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            (conj
             (into
              ["Checks if the given ex-info wraps a recide error AND is constructed according"
               "to the following ErrorForm:"
               " - %s"
               ""
               "Thus converted from error to map form it would look like this:"
               "{%s <type>,"
               " %s <msg>,"
               " %s <data>,"
               " %s <cause>,"]
              (for [[k _] (rerr/metadata-fns resolved-error)]
                (format " %s (<%s-fn> <ex-info>),"
                        k (name k))))
             " %s <serialized-form>}"))
           custom-error
           (rerr/type-keyword resolved-error)
           (rerr/message-keyword resolved-error)
           (rerr/data-keyword resolved-error)
           (rerr/cause-keyword resolved-error)
           (rerr/serialized-keyword resolved-error)))
       {:arglists '~'[[x] [x type]]}
       ([x#] (custom-error? ~custom-error x#))
       ([x# type#] (custom-error? ~custom-error x# type#)))))

(defmacro generate-error? [custom-error] (generate-error?* custom-error))

(defn error->map
  "Verifies that the IExceptionInfo is a recide error, then converts it to map form."
  [x]
  {:pre [(error? x)]}
  (let [data (ex-data x)
        custom-error (get data :recide/error)
        m (into data
                (map (juxt first (comp #(% x) second)))
                (rerr/metadata-fns custom-error))]
    (error-map* custom-error
                (get m (rerr/type-keyword custom-error))
                (.getMessage ^Exception x)
                (dissoc m (rerr/type-keyword custom-error) :recide/error)
                (.getCause ^Exception x))))

(defn throwable->error-map
  [custom-error e]
  (cond (custom-error-map? custom-error e) e
        (custom-error? custom-error e)
        (-> (error->map e)
            (update (rerr/cause-keyword custom-error) #(when (instance? Throwable %)
                                                         (pr-str (Throwable->map %))))
            (assoc (rerr/serialized-keyword custom-error) (utils/serialize-throwable e)))
        (instance? Throwable e)
        (-> (error-map* custom-error
                        :exception
                        (.getMessage ^Throwable e)
                        {:exception (pr-str (Throwable->map e))})
            (assoc (rerr/serialized-keyword custom-error) (utils/serialize-throwable e)))))

(defn generate-throwable->error-map*
  [custom-error]
  (let [custom-error (backtick/resolve-symbol custom-error)]
    `(defn ~'throwable->error-map
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            (conj
             (into
              ["Takes an error-map, error, or any Throwable, and converts it to a recide error-map"
               "conforming to the ErrorForm:"
               "  - %s"
               ""
               "The return map will be shaped like so:"
               ""
               "{%s <type>,"
               " %s <msg>,"
               " %s <ex-data>,"
               " %s <cause>,"]
              (for [[k _] (rerr/metadata-fns resolved-error)]
                (format " %s (<%s-fn> <ex-info>),"
                        k (name k))))
             " %s <serialized-form>}"))
           custom-error
           (rerr/type-keyword resolved-error)
           (rerr/message-keyword resolved-error)
           (rerr/data-keyword resolved-error)
           (rerr/cause-keyword resolved-error)
           (rerr/serialized-keyword resolved-error)))
       {:arglists '~'[[e]]}
       [e#] (throwable->error-map ~custom-error e#))))

(defmacro generate-throwable->error-map [custom-error] (generate-throwable->error-map* custom-error))

(defn- error-map->error*
  [err f]
  {:pre [(error-map? err)]}
  (let [custom-error (:recide/error err)]
    ((rerr/constructor custom-error)
     ((rerr/message-keyword custom-error) err)
     (assoc (get err (rerr/data-keyword custom-error))
            (rerr/type-keyword custom-error)
            (get err (rerr/type-keyword custom-error))
            :recide/error
            (f custom-error))
     (when-some [cause (get err (rerr/cause-keyword custom-error))]
       (cond
         (instance? Throwable cause) cause
         (instance? String cause)
         (ex-info (format "Unknown cause: %s"
                          (read-string-with-default-tagging cause))
                  {:cause (read-string-with-default-tagging cause)}))))))

(defn error-map->serializable-error [err] (error-map->error* err rerr/serialization-tag))
(defn error->serializable [err]
  {:pre [(error? err)]}
  (-> err error->map error-map->serializable-error))

(defn error-map->error [err] (error-map->error* err identity))

(defn error*
  ([custom-error type msg data] (error* custom-error type msg data nil))
  ([custom-error type msg data cause]
   (error-map->error
    (error-map* custom-error type msg data cause))))

(defn generate-error*
  [custom-error]
  (let [custom-error (backtick/resolve-symbol custom-error)]
    `(defn ~'error
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            ["Returns a recide error (IExceptionInfo) with the type (keyword), msg (string), data (map),"
             "and cause (Throwable). Merges the following map onto the ex-data:"
             ""
             "For Example: `{:recide/error recide.error.ErrorForm@447521e, :recide/type type}`"
             ""
             "ErrorForm definition: `%s`"
             ""
             "Constructor: `%s`"])
           custom-error
           (.getName ^Class (class (rerr/constructor resolved-error)))))
       {:arglists '~'[[type msg data] [type msg data cause]]}
       ([type# msg# data#] (error* ~custom-error type# msg# data#))
       ([type# msg# data# cause#] (error* ~custom-error type# msg# data# cause#)))))

(defmacro generate-error [custom-error] (generate-error* custom-error))
(generate-error default-error-definition)

(defn error-map->throwable
  "Verifies that the map is polymorphic to a recide error, then converts it back into an IExceptionInfo.
  It will use the ErrorForm (and constructor) designated in the map by :recide/error."
  [err]
  {:pre [(error-map? err)]
   :post [(instance? Throwable %)]}
  (let [custom-error (:recide/error err)]
    (if-some [st (get err (rerr/serialized-keyword custom-error))]
      (utils/deserialize-throwable st)
      (error-map->error err))))

(defn var->qualified-sym [v] (symbol (-> v meta :ns str) (-> v meta :name str)))

(defmacro raise*
  "raised-sites and custom-error are fully-qualified symbols
  Records this raise-site for the error type, and expands into:

  (throw (error {:recide/type type,
                 :recide/msg msg,
                 :recide/data data,
                 :recide/cause cause}))"
  ([custom-error type msg data]
   (record-raised-error-site! @(resolve custom-error) type msg *ns* &form)
   `(throw (error* ~custom-error ~type ~msg ~data)))
  ([custom-error type msg data cause]
   (record-raised-error-site! @(resolve custom-error) type msg *ns* &form)
   `(throw (error* ~custom-error ~type ~msg ~data ~cause))))

(defn generate-raise*
  "custom-error must be fully-qualified symbols."
  [custom-error]
  (let [custom-error (backtick/resolve-symbol custom-error)]
    (let [type (gensym 'type)
          msg (gensym 'msg)
          data (gensym 'data)
          cause (gensym 'cause)
          cust-err (gensym 'custom-error)
          err-sites (gensym 'error-sites)]
      `(defmacro ~'raise
         ~(let [resolved-error @(resolve custom-error)]
            (format
             (clojure.string/join
              "\n  "
              ["Records this raise-site on the ErrorForm definition, and constructs"
               "an IExceptionInfo as a recide-style error."
               ""
               "ErrorForm: %s"
               "Constructor: %s"
               ""
               "Merges the following map onto the data:"
               "{:recide/error %s,"
               " %s type}"])
             custom-error
             (.getName ^Class (class (rerr/constructor resolved-error)))
             custom-error
             (rerr/type-keyword resolved-error)))
         {:arglists '~'[[type msg data] [type msg data cause]]}
         (~[type msg data]
          (with-meta
            `(raise* ~'~custom-error ~~type ~~msg ~~data)
            (select-keys (meta ~'&form) [:line :column])))
         (~[type msg data cause]
          (with-meta
            `(raise* ~'~custom-error ~~type ~~msg ~~data ~~cause)
            (select-keys (meta ~'&form) [:line :column])))))))

(defmacro generate-raise [custom-error] (generate-raise* custom-error))
(generate-raise recide.impl/default-error-definition)

(defmacro insist*
  "capture-flag, raised-sites, and custom-error must be fully-qualified symbols.
  Same syntax as assert. If assertion is disabled, insistence will also be disabled.
  Throws an exception if expr does not evaluate to logical true.

  Otherwise, it acts like recide/raise, with type :recide/assertion. The bindings of
  symbols used in the assertion are inserted into the info map."
  ([capture-flag custom-error expr]
   (when *assert*
     (with-meta `(insist* ~capture-flag ~custom-error ~expr nil)
       (select-keys (meta &form) [:line :column]))))
  ([capture-flag custom-error expr message]
   (when *assert*
     (let [affected (->> expr (tree-seq coll? seq) (filter (-> &env keys set)))]
       `(when-not ~expr
          ~(with-meta `(raise* ~custom-error
                               :recide/assertion
                               (str "Assertion failed: " ~(or message (pr-str expr)))
                               ~(if (and (.hasRoot ^clojure.lang.Var (resolve capture-flag))
                                         @(resolve capture-flag)
                                         (seq affected))
                                  `{:expression '~expr,
                                    :values (zipmap '~affected
                                                    ~(vec affected))}
                                  `{:expression '~expr}))
             (select-keys (meta &form) [:line :column])))))))

(defn ^:private generate-insist*
  [capture-flag custom-error]
  (let [capture-flag (backtick/resolve-symbol capture-flag)
        custom-error (backtick/resolve-symbol custom-error)
        expr (gensym 'expr)
        message (gensym 'message)]
    `(defmacro ~'insist
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            ["Same syntax as assert. If assertion is disabled, insistence will also be disabled."
             "Throws an IExceptionInfo if expr does not evaluate to logical true."
             "%s is the constructor used."
             ""
             "Otherwise, it acts like recide/raise, constructing a recide error of"
             "the following form:"
             "{%s :recide/assertion,"
             " %s msg,"
             " %s <data>,"
             " %s <serialized-form>}"
             ""
             "When %s is true, this will also capture the bindings of symbols used"
             "in the assertion, inserting these values into the data map. %s"
             "is false by default, unless one of the following was true at class load:"
             " (1) env var is true: RECIDERE_CAPTURE_INSISTS"
             " (2) system property is set: recide.capture-insists"])
           custom-error
           (rerr/type-keyword resolved-error)
           (rerr/message-keyword resolved-error)
           (rerr/data-keyword resolved-error)
           (rerr/serialized-keyword resolved-error)
           capture-flag
           capture-flag))
       {:arglists '~'[[expr] [expr message]]}
       (~[expr]
        (with-meta
          `(insist* ~'~capture-flag ~'~custom-error ~~expr)
          (select-keys (meta ~'&form) [:line :column])))
       (~[expr message]
        (with-meta
          `(insist* ~'~capture-flag ~'~custom-error ~~expr ~~message)
          (select-keys (meta ~'&form) [:line :column]))))))

(defmacro generate-insist
  [capture-flag custom-error]
  (generate-insist* capture-flag custom-error))

(generate-insist recide.impl/*capture-insists*
                 recide.impl/default-error-definition)

(defmacro deferror*
  "Defines a new error constructor and a specialized version of the recide/raise macro,
  where the type and the 'generic' portion of the error message are static and required
  arguments are specified via the final sequence of key-words. Defines two macros (for
  compile-time errors). The first is a constructor named error-name, with the signature:
  ([detail-msg data] [detail-msg data cause])

  Also defined is a corresponding `raise-` macro with the signature:
  ([detail-msg {:as extra-data}]
   [detail-msg {:as extra-data} cause]).

  Example:
  (deferror storage-timeout
      :storage/timeout
      \"A storage operation timed out\"
      [:method])

  You can use the constructor like so:
  (storage-timeout \"A call to get-value took too long.\"
                   {:method 'get-value,
                    :nodes-count (count kvs)})

  This macro would expand into the following:
  (error :storage/timeout
         (str \"A storage operation timed out: \"
              \"A call to get-value took too long.\")
         {:method 'get-value,
          :nodes-count (count kvs)})

  Using the generated raise- macro is similar:
  (raise-storage-timeout \"A call to get-value took too long.\"
                         {:method 'get-value
                          :nodes-count (count kvs)})

  This macro would expand similarly into a raise form. In both examples, if :method were left
  out of the data map, an exception would be thrown at compile time."
  ([custom-error error-name type generic-str]
   `(deferror* ~custom-error ~error-name ~type ~generic-str []))
  ([custom-error error-name type generic-str required-keys]
   (insist (symbol? error-name) "The first argument to deferror must be a symbol.")
   (insist (keyword? type) "deferror requires that you specify error type as a keyword.")
   (insist (string? generic-str) "deferror requires a string for argument generic-str.")
   (doseq [required required-keys]
     (insist (keyword? required) "Required keys specified in deferror must be keywords."))
   (let [docstring-req-keys (if (seq required-keys)
                              (format "The following keys are required in the data-map:\n #{%s}"
                                      (clojure.string/join ",\n   " (map str required-keys)))
                              "")]
     `(do
        (defmacro ~error-name
          ~(format
            "Records this raise-site under %s in %s, and expands into the equivalent of:

         (error %s
                (str \"%s\" detail-str)
                data
                cause)

  %s"
            type, (backtick/resolve-symbol custom-error), type, (str generic-str ": "), docstring-req-keys)
          {:arglists '~'[[detail-str data] [detail-str data cause]]}
          ([detail-str# data#]
           `(~(backtick/syntax-quote ~error-name) ~detail-str# ~data# nil))
          ([detail-str# data# cause#]
           (insist (map? data#)
                   ~(format "The data argument passed to %s must be a map literal" error-name))
           (insist (empty? (clojure.set/difference ~(set required-keys) (set (keys data#))))
                   (format ~(format "%s requires the following missing keys in its data: %s"
                                    error-name "%s")
                           (clojure.string/join ", " (clojure.set/difference ~(set required-keys) (set (keys data#))))))
           (with-meta
             `(error* ~'~custom-error
                      ~~type
                      (str ~~generic-str ": " ~detail-str#)
                      ~data#
                      ~@(when cause# [cause#]))
             (select-keys (meta ~'&form) [:line :column]))))
        (defmacro ~(symbol (str "raise-" error-name))
          ~(format
            "Records this raise-site under %s in recide, and expands into:

         (raise %s
                (str \"%s\" detail-str)
                data
                cause)

  %s"
            type, type, (str generic-str ": ") docstring-req-keys)
          {:arglists '~'[[detail-str data] [detail-str data cause]]}
          ([detail-str# data#]
           `(~(backtick/syntax-quote ~(symbol (str "raise-" error-name))) ~detail-str# ~data# nil))
          ([detail-str# data# cause#]
           (insist (map? data#)
                   ~(format "The data argument passed to %s must be a map literal" error-name))
           (insist (empty? (clojure.set/difference ~(set required-keys) (set (keys data#))))
                   (format ~(format "%s requires the following missing keys in its data: %s"
                                    error-name "%s")
                           (clojure.string/join ", " (clojure.set/difference ~(set required-keys) (set (keys data#))))))
           (with-meta
             `(raise* ~'~custom-error
                      ~~type
                      (str ~~generic-str ": " ~detail-str#)
                      ~data#
                      ~@(when cause# [cause#]))
             (select-keys (meta ~'&form) [:line :column]))))))))

(defn generate-deferror*
  "custom-error is fully-qualified symbol."
  [custom-error]
  (let [error-name (gensym 'error-name)
        type (gensym 'type)
        generic-str (gensym 'generic-str)
        required-keys (gensym 'required-keys)
        custom-error (backtick/resolve-symbol custom-error)]
    `(defmacro ~'deferror
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            ["Defines specialized versions of the recide.core/error and recide.core/raise macros,"
             "where the type and the 'generic' portion of the error message are static and required"
             "data are specified via the final sequence of keywords. Defines two macros (for"
             "compile-time errors). The first is a specialized constructor interned as `<error-name>`,"
             "analogous to recide.core/error, with the following signature:"
             "([detail-msg data] [detail-msg data cause])"
             ""
             "Also defined is a corresponding `raise-<error-name>` macro with the signature:"
             "([detail-msg {:as extra-data}]"
             " [detail-msg {:as extra-data} cause])"
             ""
             "Example:"
             "(deferror io-timeout"
             "          :io/timeout"
             "          \"A network operation timed out\""
             "          [:operation])"
             ""
             "Use the defined constructor like so:"
             "(io-timeout \"the gremlins endpoint failed to respond.\""
             "            {:operation 'gremlin-endpoint,"
             "             :other-info :kazzam})"
             ""
             "This will construct a recide error of the following form:"
             "{%s :io/timeout"
             " %s \"A network operation timed out: the gremlins endpoint failed to respond.\""
             " %s {:operation 'gremlin-endpoint, :other-info :kazzam}"
             " %s <serialized-form>}"
             ""
             "Using the generated raise-* macro is similar:"
             "(raise-io-timeout \"the gremlins endpoint failed to respond.\""
             "                  {:operation 'gremlin-endpoint,"
             "                   :other-info :kazzam}"
             ""
             "In both macros, if :operation is left out of the data map, an exception will be thrown"
             "at compile-time because it was marked as required in `deferror`."
             ""
             "Uses ErrorForm: %s"])
           (rerr/type-keyword resolved-error)
           (rerr/message-keyword resolved-error)
           (rerr/data-keyword resolved-error)
           (rerr/serialized-keyword resolved-error)
           custom-error))
       {:arglists '~'[[error-name type generic-str] [error-name type generic-str required-keys]]}
       (~[error-name type generic-str]
        `(deferror* ~'~custom-error ~~error-name ~~type ~~generic-str))
       (~[error-name type generic-str required-keys]
        `(deferror* ~'~custom-error ~~error-name ~~type ~~generic-str ~~required-keys)))))

(defmacro generate-deferror [custom-error] (generate-deferror* custom-error))

(defn- format-map
  ([m] (format-map " " m))
  ([prefix m]
   (let [recurse (fn [[k v]]
                   (if (map? v)
                     (let [add (repeat (+ 2 (count (pr-str k))) " ")]
                       [k (format-map (apply str prefix add) v)])
                     [k (pr-str v)]))]
     (->> m
          (sequence (comp (map recurse)
                          (map (partial clojure.string/join " "))))
          (clojure.string/join (str ",\n" prefix))
          (format "{%s}")))))

(defmacro deferror-group*
  "Defines specialized (macro) variants of recide/error and recide/raise, for working
  with a namespaced family of error types. For each subtype, the 'generic' portion of the
  error message is static. Required data keywords can be specified for both the entire
  family of errors and for each subtype.

  Both the constructor and raise macros have the same signature:
  ([subtype detail-msg {:as extra-data}]
   [subtype detail-msg {:as extra-data} cause]).

  Example:
  (deferror-group parse-err
      (:query.invalid [:expression])
      (find-spec \"Invalid find spec\")
      (inputs \"Invalid inputs\" [:invalid]))

  An example of using the constructor is:
  (parse-err :inputs
             \"Input arguments must be integers\"
             {:expression spec,
              :invalid (remove input? args)})

  This would expand into the following:
  (error :query.invalid/inputs
         (str \"Invalid inputs\"
              \":\"
              \"Input arguments must be integers\"
         {:expresion spec, :invalid (remove input? args)})

  To use the raise form, you would replace `parse-err` with `raise-parse-err`. In either case
  in this example, if either :expression or :invalid were left out of the data map, an
  exception would be thrown at compile time."
  [custom-error error-name group-key subtypes]
  (when-not (symbol? error-name)
    (throw (IllegalArgumentException. "First argument to deferror-group must be a symbol.")))
  (when-not (or (keyword? group-key)
                (and (sequential? group-key)
                     (keyword? (first group-key))
                     (every? keyword? (second group-key))))
    (throw (IllegalArgumentException.
            "The second argument to deferror-group must be the group-key (a non-namespaced keyword) or of the form (group-key [required-keys])")))
  (when (empty? subtypes)
    (throw (IllegalArgumentException. "You must define at least one subtype in deferror-group.")))
  (when-not (every? #(let [[sym str req] %]
                       (and (symbol? sym)
                            (string? str)
                            (or (nil? req) (every? keyword? req))))
                    subtypes)
    (throw (IllegalArgumentException.
            "Each subtype must be of the form (subtype-symbol generic-string required-keys). required-keys is optional.")))
  (let [[group-key group-required] (if (keyword? group-key)
                                     [group-key #{}]
                                     [(first group-key) (set (second group-key))])
        group-ns (-> group-key name)
        subtype-data (into {}
                           (for [[subtype generic-str required] subtypes]
                             [(keyword subtype)
                              {:required (into group-required required)
                               :generic-str generic-str}]))]
    `(do
       (defmacro ~error-name
         ~(format
           "Records this raise-site under %s/<subtype> in %s, and expands into the
            equivalent of:

         (error (str \"<subtype-generic-str>: \" detail-str)
                %s/<subtype>
                cause)

  The following map shows, for each subtype, what keywords are required in
  the data map, and what the generic portion of the string will be:

%s%s"
           group-key, (backtick/resolve-symbol custom-error), group-key, "     " (format-map "     " subtype-data))
         {:arglists '~'[[subtype detail-str data] [subtype detail-str data cause]]}
         ([subtype# detail-str# data#]
          `(~(backtick/syntax-quote ~error-name) ~subtype# ~detail-str# ~data# nil))
         ([subtype# detail-str# data# cause#]
          (insist (keyword? subtype#)
                  ~(format "%s requires that you specify an error subtype as the first argument."
                           error-name))
          (insist (find ~subtype-data subtype#)
                  (format ~(format "%s does not have a %s subtype." error-name "%s")
                          subtype#))
          (insist (map? data#)
                  ~(format "%s requires a map literal as its data argument." error-name))
          (let [required-diff# (clojure.set/difference (get-in ~subtype-data [subtype# :required])
                                                       (set (keys data#)))]
            (insist (empty? required-diff#)
                    (-> ~(format "%s called with subtype %s requires the following missing keys in its data: %s"
                                 error-name "%s" "%s")
                        (format subtype# (clojure.string/join ", " required-diff#)))))
          (with-meta
            `(error* ~'~custom-error
                     ~(keyword ~group-ns (name subtype#))
                     (str ~(get-in ~subtype-data [subtype# :generic-str]) ": " ~detail-str#)
                     ~data#
                     ~@(when cause# [cause#]))
            ~(select-keys (meta '&form) [:line :column]))))
       (defmacro ~(symbol (str "raise-" error-name))
         ~(format
           "Records this raise-site under %s/<subtype> in %s and expands into:

         (raise %s/<subtype>
                (str \"<subtype-generic-str>: \" detail-str)
                data
                cause)

  The following map shows, for each subtype, what keywords are required in
  the data map, and what the generic portion of the string will be:

%s%s"
           group-key, (backtick/resolve-symbol custom-error), group-key, "     " (format-map "     " subtype-data))
         {:arglists '~'[[subtype detail-str data] [subtype detail-str data cause]]}
         ([subtype# detail-str# data#]
          `(~(backtick/syntax-quote ~(symbol (str "raise-" error-name))) ~subtype# ~detail-str# ~data# nil))
         ([subtype# detail-str# data# cause#]
          (insist (keyword? subtype#)
                  ~(format "%s requires that you specify an error subtype as the first argument."
                           error-name))
          (insist (find ~subtype-data subtype#)
                  (format ~(format "%s does not have a %s subtype." error-name "%s")
                          subtype#))
          (insist (map? data#)
                  ~(format "%s requires a map literal as its data argument." error-name))
          (let [required-diff# (clojure.set/difference (get-in ~subtype-data [subtype# :required])
                                                       (set (keys data#)))]
            (insist (empty? required-diff#)
                    (-> ~(format "%s called with subtype %s requires the following missing keys in its data: %s"
                                 error-name "%s" "%s")
                        (format subtype# (clojure.string/join ", " required-diff#)))))
          (with-meta
            `(raise* ~'~custom-error
                     ~(keyword ~group-ns (name subtype#))
                     (str ~(get-in ~subtype-data [subtype# :generic-str]) ": " ~detail-str#)
                     ~data#
                     ~@(when cause# [cause#]))
            ~(select-keys (meta '&form) [:line :column])))))))

(defn generate-deferror-group*
  [&form &env custom-error]
  (let [error-name (gensym 'error-name)
        group-key (gensym 'group-key)
        subtypes (gensym 'subtypes)
        custom-error (backtick/resolve-symbol custom-error)]
    `(defmacro ~'deferror-group
       ~(let [resolved-error @(resolve custom-error)]
          (format
           (clojure.string/join
            "\n  "
            ["Defines new specialized versions of the recide.core/error and recide.core/raise macros,"
             "for working with a namespaced family of error types. For each subtype, the 'generic' portion"
             "of the error message is static. Required data keywords can be specified both for the entire"
             "family of errors and for each subtype."
             ""
             "Both the constructor and raise-* macros have the same signature:"
             "([detail-msg {:as extra-data}]"
             " [detail-msg {:as extra-data} cause])"
             ""
             "Example:"
             "(deferror-group io-failure"
             "               (:io.failure [:operation])"
             "               (timeout \"A network operation timed out\" [:timeout-ms])"
             "               (connection-reset \"The other node reset the connection.\")"
             ""
             "An example of using the defined constructor is:"
             "(io-failure :timeout"
             "            \"the gremlins endpoint failed to respond.\""
             "            {:operation 'gremlin-endpoint,"
             "             :timeout-ms 10000})"
             ""
             "This will construct a recide error of the following form:"
             "{%s :io.failure/timeout"
             " %s \"A network operation timed out: the gremlins endpoint failed to respond.\""
             " %s {:operation 'gremlin-endpoint, :timeout-ms 10000}"
             " %s <serialized-form>}"
             ""
             "The generated raise-* macro uses the exact same syntax, but throws the constructed exception."
             ""
             "In both macros, a compile-time exception will be thrown if either :operation or :timeout-ms is"
             "left out of the data-map for :io.failure/timeout, because the entire family of errors requires"
             ":operation and the :timeout error in particular requires :timeout-ms."
             ""
             "Uses ErrorForm: %s"])
           (rerr/type-keyword resolved-error)
           (rerr/message-keyword resolved-error)
           (rerr/data-keyword resolved-error)
           (rerr/serialized-keyword resolved-error)
           custom-error))
       {:arglists '~'[[error-name namespace-key & subtypes]]}
       ~[error-name group-key '& subtypes]
       `(deferror-group* ~'~custom-error ~~error-name ~~group-key ~~subtypes))))

(defmacro generate-deferror-group [custom-error] (generate-deferror-group* &form &env custom-error))

(defmacro generate-custom-error-library
  [custom-error capture-flag]
  (let [custom-error @(resolve custom-error)
        capture-flag (resolve capture-flag)]
    (insist (instance? ErrorForm custom-error))
    (insist (.isDynamic ^clojure.lang.Var capture-flag)))
  `(do
     (generate-error-map ~custom-error)
     (generate-error ~custom-error)
     (generate-error-map? ~custom-error)
     (generate-error? ~custom-error)
     (generate-throwable->error-map ~custom-error)
     (generate-raise ~custom-error)
     (generate-insist ~capture-flag ~custom-error)
     (generate-deferror ~custom-error)
     (generate-deferror-group ~custom-error)))
