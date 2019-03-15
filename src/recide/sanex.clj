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

(ns recide.sanex
  (:require [recide.core :as rc]
            [recide.error :as rerr]
            [utiliva.control :refer [?->]])
  (:import [clojure.lang IExceptionInfo IPersistentMap]
           [recide.sanex ISanitized ISanitizable]))

(def ^:private empty-trace
  "Less garbage, eh? I gud at optimizing chokepoints."
  (make-array StackTraceElement 0))

(defn apply-suppression
  "Returns a new map of error-components with the suppression-levels applied. Suppression level values
  are handled on a truthy basis. This is a lower-level utility and ignores :suppress-recursively?."
  [{:keys [cause message stack-trace type data] :as error-components}
   {:keys [suppress-cause? suppress-message? suppress-stack? suppress-data?]
    :as suppression-levels}]
  {:cause (when-not suppress-cause? cause)
   :message (if suppress-message? "" message)
   :stack-trace (if suppress-stack? empty-trace stack-trace)
   :data (if-not suppress-data?
           data
           (if-not type
             {}
             (select-keys data [type])))})

(defn ^:private suppressed-throwable
  ([t opts] (suppressed-throwable t nil opts))
  ([^Throwable t cause {:keys [suppress-cause?
                               suppress-message?
                               suppress-stack?
                               suppress-data?]
                        :as suppression-map}]
   (if (instance? ISanitizable t)
     (.getSanitized ^ISanitizable t suppression-map)
     (let [{:keys [cause message stack-trace data]}
           (apply-suppression {:cause (or cause (.getCause t))
                               :message (.getMessage t)
                               :stack-trace (.getStackTrace t)
                               :data (ex-data t)
                               :type (when (rc/error? t)
                                       (->> :recide/error
                                            (get (ex-data t))
                                            (rerr/type-keyword)))}
                              suppression-map)
           original-class-name (.getName (.getClass t))]
       ;; TODO: proxy names are garbage. gen-class wants aot. with-redefs on proxy-name doesn't work. Come on, Clojure!
       (if (instance? IExceptionInfo t)
         (proxy [Exception IExceptionInfo ISanitized] [message cause]
           (getCause [] cause)
           (getMessage [] message)
           (getStackTrace [] stack-trace)
           (toString [] (str original-class-name ": " message " " (str data)))
           (getData [] data)
           (getUnsanitized [] t))
         (proxy [Exception ISanitized] [message cause]
           (getCause [] cause)
           (getMessage [] message)
           (getStackTrace [] stack-trace)
           (toString [] (str original-class-name ": " message))
           (getUnsanitized [] t)))))))

(def noop-sanitization
  {:suppress-data? false,
   :suppress-cause? false,
   :suppress-message? false,
   :suppress-stack? false,
   :suppress-recursively? false})

(def default-sanitization
  {:suppress-data? true,
   :suppress-cause? false,
   :suppress-message? true,
   :suppress-stack? false,
   :suppress-recursively? true})

(def ^:dynamic *sanitization-level*
  (if (or (= "false" (clojure.string/lower-case (or (System/getenv "RECIDE_SANITIZATION") "")))
          (= "false" (clojure.string/lower-case (or (System/getProperty "recide.sanitization") ""))))
    noop-sanitization
    default-sanitization))

(defmacro without-sanitization
  "Executes body with the sanitization level set to recide.sanex/noop-sanitization."
  [& body]
  `(binding [*sanitization-level* noop-sanitization]
     ~@body))

(defn specify-sanitization ;; defn hygienate
  "x : error / error-ex / ex-data
  sanitization : true / false / map

  When sanitization is a boolean, it applies either no sanitization (false) or
  the sanitization level defined in recide.sanex/*sanitization-level* (true).

  When sanitization is a map, it may specify booleans for the following keys:
   - :suppress-data?
   - :suppress-cause?
   - :suppress-message?
   - :suppress-stack?
   - :suppress-recursively?

  This map will be merged onto recide.sanex/*sanitization-level*"
  [x sanitization]
  (assert (or (boolean? sanitization)
              (map? sanitization)))
  (cond (rc/error-map? x)
        (let [custom-error (:recide/error x)]
          (assoc-in x [(rerr/data-keyword custom-error) ::sanitary?] sanitization))

        (rc/error? x)
        (rc/assoc-error x ::sanitary? sanitization)

        (map? x)
        (assoc x ::sanitary? sanitization)))

(defn sanitary?
  "Returns false for all exceptions unless they are ISanitized or recide errors.
  For recide, this examines the map for the key ::sanitary. Returns false, true,
  or a map of suppression options."
  [t]
  (or (instance? ISanitized t)
      (if-let [sanitary? (?-> t rc/error? (-> ex-data ::sanitary?))]
        (cond (map? sanitary?) sanitary?
              (boolean? sanitary?) sanitary?
              :else false)
        false)))

(declare sanitize)
(defn- sanitize*
  [^Throwable t {:as opts :keys [suppress-recursively?]}]
  (if (every? (complement identity) (vals opts))
    t ;; prettier no-op case -- leave it alone.
    (let [sanitary-option (sanitary? t)]
      (case [sanitary-option suppress-recursively?]
        [true false] t
        [true true] (suppressed-throwable t (some-> (.getCause t) (sanitize opts)) noop-sanitization)
        [false false] (suppressed-throwable t opts)
        [false true] (suppressed-throwable t (some-> (.getCause t) (sanitize opts)) opts)
        ;; Then sanitary-option is a map.
        (if suppress-recursively?
          (suppressed-throwable t
                                (some-> (.getCause t) (sanitize opts))
                                (merge opts sanitary-option))
          (suppressed-throwable t (merge opts sanitary-option)))))))

(defn sanitize
  "Sanitizes the Throwable t according to `recide.sanex/*sanitization-level*`, subject to optional overrides in opts."
  (^Exception [t]
   (sanitize* t *sanitization-level*))
  (^Exception [^Throwable t opts]
   (sanitize* t (merge *sanitization-level* opts))))
