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

(ns recide.core
  (:require [clojure.set]
            [clojure.spec.alpha :as s]
            [backtick]
            [recide.impl :as impl]
            [recide.error :as rerr]
            [recide.utils :as utils]
            [utiliva.macros :refer [class-sym? sym->class]])
  (:import [java.io Serializable]
           [java.util ArrayList]))

(defmacro generate-library!
  "Generates and defs custom versions of the following recide.core methods, tailored specifically
  to custom-error, with variable capture in the generated `insist` subject to capture-flag.
    * error
    * error?
    * error-map
    * error-map?
    * throwable->error-map
    * raise
    * insist
    * deferror
    * deferror-group

  custom-error should be an instance of recide.error/ErrorForm (see `def-error-form`).
  capture-flag must be resolvable to a dynamic var."
  [custom-error capture-flag]
  (let [custom-error (backtick/resolve-symbol custom-error)
        capture-flag (backtick/resolve-symbol capture-flag)]
    `(impl/generate-custom-error-library ~custom-error ~capture-flag)))

;; def raise
(impl/generate-raise impl/default-error-definition)

;; def insist
(impl/generate-insist impl/*capture-insists* impl/default-error-definition)

;; def error-map
(impl/generate-error-map impl/default-error-definition)

;; def error
(impl/generate-error impl/default-error-definition)

(def error? impl/error?)
(alter-meta! #'error? merge (-> #'impl/error? meta (select-keys [:doc :arglists])))

(def error-map? impl/error-map?)
(alter-meta! #'error-map? merge (-> #'impl/error-map? meta (select-keys [:doc :arglists])))

;; def throwable->error-map
(impl/generate-throwable->error-map impl/default-error-definition)

(def error-map->throwable impl/error-map->throwable)
(alter-meta! #'error-map->throwable merge (-> #'impl/error-map->throwable meta (select-keys [:doc :arglists])))

(def error->map impl/error->map)
(alter-meta! #'error->map merge (-> #'impl/error->map meta (select-keys [:doc :arglists])))

(def error-type impl/error-type)
(alter-meta! #'error-type merge (-> #'impl/error-type meta (select-keys [:doc :arglists])))

;; def deferror
(impl/generate-deferror impl/default-error-definition)

;; def deferror-group
(impl/generate-deferror-group impl/default-error-definition)

(defn error->form
  "If x is a recide error, returns the ErrorForm defining its construction."
  [x]
  {:pre [(impl/error? x)]}
  (:recide/error (ex-data x)))

(defn error-map->form
  "If x is a recide error-map, returns the ErrorForm defining its construction."
  [x]
  {:pre [(impl/error-map? x)]}
  (:recide/error x))

(defn assoc-error
  "Given a recide error, this returns a copy with [k v] assoc'ed into its ex-data."
  [x k v]
  {:pre [(impl/error? x)]}
  (let [custom-error (:recide/error (ex-data x))]
    (as-> (impl/error->map x) err
      (assoc-in err [(rerr/data-keyword custom-error) k] v)
      (impl/error-map->error err))))

(defn update-error
  "Given a recide error, this returns a copy with its ex-data modified as though
  with (apply update data f args)."
  [x k f & args]
  {:pre [(impl/error? x)]}
  (let [custom-error (:recide/error (ex-data x))]
    (as-> (impl/error->map x) err
      (apply update-in err [(rerr/data-keyword custom-error) k] f args)
      (impl/error-map->error err))))

(defn make-tag->error-form
  "Utility function. Pass in the ErrorForms you care about, and this returns
  tag->error-form appropriate for use with deserialize-throwable."
  [& error-form]
  (into {}
        (for [form error-form]
          [(rerr/serialization-tag form) error-form])))

(def error-form rerr/error-form)
(alter-meta! #'error-form merge (-> #'rerr/error-form meta (select-keys [:doc :arglists])))

;;;;;;;;;;;;
;;; TRY* ;;;
;;;;;;;;;;;;

(defn- keyword-target?
  [k]
  (and (keyword? k)
       (some? (namespace k))))

(defn- pred-target?
  [x]
  (or (symbol? x) (seq? x)))

(defn- catch-symbol? [x] (= x 'catch))

(defn- binding? [s] (simple-symbol? s))

;; compound modifiers
(def ^:private modifier? #{:and :or})

(defn- resolve-modifier
  [modifier]
  (case modifier
    :and `and
    :or `or))

(s/def ::catch-target
  (s/or :keyword keyword-target?
        :type class-sym?
        :negated (s/cat :neg #{:not}
                        :target ::catch-target)
        :pred pred-target?))

(s/def ::targets
  (s/alt :many (s/cat :modifier modifier?
                      :targets (s/coll-of ::catch-target
                                          :kind vector?))
         :one ::catch-target))

(s/def ::catch-clause
  (s/cat :catch catch-symbol?
         :targets ::targets
         :binding binding?
         :bodies (s/* any?)))

(s/def ::finally-clause
  (s/cat :finally #(= % 'finally)
         :&rest (s/* any?)))

(s/def ::try*-body
  (s/cat :expressions (s/* #(not (and (sequential? %)
                                      (= 'catch (first %)))))
         :catches (s/* (s/spec ::catch-clause))
         :finally (s/? (s/spec ::finally-clause))))

(defn- target->pred
  [throwable [type x]]
  (case type
    :keyword `(error? ~throwable ~x)
    :type (let [class (sym->class x)]
            (insist (or (= java.lang.Throwable class)
                        (contains? (ancestors class) java.lang.Throwable))
                    (format "%s does not inherit from java.lang.Throwable" x))
            `(instance? ~x ~throwable))
    :pred `(~x ~throwable)
    :negated `(not ~(target->pred throwable (:target x)))))

(defn- targets->pred
  [throwable [arity targets]]
  (if (identical? arity :one)
    (target->pred throwable targets)
    (let [modifier (:modifier targets)
          targets (:targets targets)]
      `(~(resolve-modifier modifier)
        ~@(map (partial target->pred throwable) targets)))))

(defn- catch->cond-expr
  [throwable {:keys [targets binding bodies]}]
  (list (targets->pred throwable targets)
        `(let [~binding ~throwable]
           ~@bodies)))

(defn- compose-catches
  [catches]
  (let [throwable (gensym 't)]
    `(catch Throwable ~throwable
       (cond ~@(mapcat (partial catch->cond-expr throwable) catches)
             :else (throw ~throwable)))))

(defn- finally-clauses
  [{:as finally? &rest :&rest}]
  (when finally?
    `((finally ~@&rest))))

(defmacro try*
  "Expands to Clojure's try Special Form, allowing for enhanced `catch` clauses:
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
  Otherwise, behavior should match 'normal' catch clauses in `clojure.core/try`."
  {:arglists '[(try* expr* catch-clause* finally-clause?)]}
  [& body]
  (let [conformed (s/conform ::try*-body body)]
    (when (identical? conformed ::s/invalid)
      (raise :recide/malformed-try*
             "Failed to parse try*."
             (s/explain-data ::try*-body body)))
    `(try ~@(:expressions conformed)
          ~(compose-catches (:catches conformed))
          ~@(finally-clauses (:finally conformed)))))
