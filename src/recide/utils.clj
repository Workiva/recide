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

(ns recide.utils
  (:import [java.io ByteArrayOutputStream
            ObjectOutputStream
            ByteArrayInputStream
            ObjectInputStream]))

(defn serialize-throwable [e]
  {:pre [(instance? Throwable e)]}
  (let [bao (java.io.ByteArrayOutputStream.)]
    (with-open [oos (java.io.ObjectOutputStream. bao)]
      (.writeObject oos e))
    (.toByteArray bao)))

(defn deserialize-throwable [arr]
  {:post [(instance? Throwable %)]}
  (let [bai (java.io.ByteArrayInputStream. arr)]
    (with-open [ois (java.io.ObjectInputStream. bai)]
      (.readObject ois))))

(defn ^Throwable root-cause
  "Returns the initial cause of an exception or error by peeling off all of its wrappers."
  [^Throwable t]
  (loop [cause t]
    (if (and (instance? clojure.lang.Compiler$CompilerException cause)
             (not= (.source ^clojure.lang.Compiler$CompilerException cause) "NO_SOURCE_FILE"))
      cause
      (if-let [cause (.getCause cause)]
        (recur cause)
        cause))))
