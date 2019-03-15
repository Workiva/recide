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

(ns recide.sanex.logging
  (:require [recide.sanex :as sanex]
            [clojure.tools.logging :as log]))

(defmacro log
  "Evaluates and logs a message only if the specified level is enabled.
  Wrapper of clojure.tools.logging/log that sanitizes exceptions."
  ([level message]
   `(log/log ~level ~message))
  ([level throwable message]
   `(log/log ~level (sanex/sanitize ~throwable) ~message))
  ([logger-ns level throwable message]
   `(log/log ~logger-ns ~level (sanex/sanitize ~throwable) ~message))
  ([logger-factory logger-ns level throwable message]
   `(log/log ~logger-factory ~logger-ns ~level (sanex/sanitize ~throwable) ~message)))

(defmacro logp
  "Wrapper of clojure.tools.logging/logp that sanitizes exceptions."
  {:arglists '([level message & more] [level throwable message & more])}
  [level x & more]
  `(let [x# ~x]
     (if (instance? Throwable x#)
       (log/logp ~level (sanex/sanitize x#) ~@more)
       (log/logp ~level x# ~@more))))

(defmacro logf
  "Wrapper of clojure.tools.logging/logf that sanitizes exceptions."
  {:arglists '([level fmt & fmt-args] [level throwable fmt & fmt-args])}
  [level x & more]
  `(let [x# ~x]
     (if (instance? Throwable x#)
       (log/logf ~level (sanex/sanitize x#) ~@more)
       (log/logf ~level x# ~@more))))

(defmacro enabled?
  "No-op wrapper of clojure.tools.logging/enabled?"
  [& whatever] `(log/enabled? ~@whatever))

;; NOTE / POSSIBLE UNRESOLVED: There may be an issue here related to an incompatibility with logging libraries
(defmacro spy
  "recide.sanex.logging/spy has problems. Don't use this. If you do, expect failure."
  [& whatever]
  (throw (Exception. "Don't use recide.sanex.logging/spy.")))

(defmacro spyf
  "Spyf has problems. Don't use this. If you do,"
  [& whatever]
  (throw (Exception. "Don't use recide.sanex.logging/spyf.")))

(defn log-stream "wraps recide.sanex.logging/log-stream" [& whatever] (apply log/log-stream whatever))

(defmacro trace
  "Wrapper of clojure.tools.logging/trace. Trace level logging using print-style args.
  Sanitizes exceptions."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :trace ~@args))

(defmacro debug
  "Wrapper of clojure.tools.logging/debug. Debug level logging using print-style args.
  Sanitizes exceptions."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :debug ~@args))

(defmacro info
  "Wrapper of clojure.tools.logging/info. Info level logging using print-style args.
  Sanitizes exceptions."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :info ~@args))

(defmacro warn
  "Wrapper of clojure.tools.logging/warn. Warn level logging using print-style args.
  Sanitizes exceptions."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :warn ~@args))

(defmacro error
  "Wrapper of clojure.tools.logging/error. Error level logging using print-style args.
  Sanitizes exceptions."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :error ~@args))

(defmacro fatal
  "Wrapper of clojure.tools.logging/fatal. Fatal level logging using print-style args.
  Sanitizes exceptions."
  {:arglists '([message & more] [throwable message & more])}
  [& args]
  `(logp :fatal ~@args))

(defmacro tracef
  "Wrapper of clojure.tools.logging/tracef. Trace level logging using format.
  Sanitizes exceptions."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :trace ~@args))

(defmacro debugf
  "Wrapper of clojure.tools.logging/debugf. Debug level logging using format.
  Sanitizes exceptions."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :debug ~@args))

(defmacro infof
  "Wrapper of clojure.tools.logging/infof. Info level logging using format.
  Sanitizes exceptions."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :info ~@args))

(defmacro warnf
  "Wrapper of clojure.tools.logging/warnf. Warn level logging using format.
  Sanitizes exceptions."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :warn ~@args))

(defmacro errorf
  "Wrapper of clojure.tools.logging/errorf. Error level logging using format.
  Sanitizes exceptions."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :error ~@args))

(defmacro fatalf
  "Wrapper of clojure.tools.logging/fatalf. Fatal level logging using format.
  Sanitizes exceptions."
  {:arglists '([fmt & fmt-args] [throwable fmt & fmt-args])}
  [& args]
  `(logf :fatal ~@args))
