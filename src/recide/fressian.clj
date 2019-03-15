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

(ns recide.fressian
  (:require [recide.core :as rc]
            [recide.error :as re]
            [recide.impl :as ri])
  (:import [org.fressian Writer Reader]
           [org.fressian.handlers WriteHandler ReadHandler]
           [recide.error ErrorForm]))

(def error-form-writer
  (reify WriteHandler
    (^void write [_ ^Writer writer v]
      (assert (instance? ErrorForm v))
      (.writeTag writer "recide/error" 6)
      (.writeObject writer (re/serialization-tag v))
      (.writeObject writer (re/type-keyword v))
      (.writeObject writer (re/message-keyword v))
      (.writeObject writer (re/data-keyword v))
      (.writeObject writer (re/cause-keyword v))
      (.writeObject writer (re/serialized-keyword v)))))

(defn construct-error-form-reader
  [error-forms]
  (let [reifiers
        (into {}
              (for [error-form error-forms]
                [(re/serialization-tag error-form)
                 (let [constructor (re/constructor error-form)
                       raised-sites (re/raised-sites error-form)
                       fressian-tag (re/serialization-tag error-form)]
                   (fn [type-kw msg-kw data-kw cause-kw ser-kw]
                     (re/error-form
                      {:serialization-tag fressian-tag
                       :type-keyword type-kw
                       :message-keyword msg-kw
                       :data-keyword data-kw
                       :cause-keyword cause-kw
                       :serialized-keyword ser-kw
                       :constructor constructor})))]))]
    (reify ReadHandler
      (read [_ ^Reader reader tag ^int _]
        (let [tag (.readObject reader)
              type-kw (.readObject reader)
              msg-kw (.readObject reader)
              data-kw (.readObject reader)
              cause-kw (.readObject reader)
              ser-kw (.readObject reader)]
          ((get reifiers tag) type-kw msg-kw data-kw cause-kw ser-kw))))))

(def default-error-form-reader (construct-error-form-reader [ri/default-error-definition]))

(defn construct-write-handlers
  [& error-forms]
  (assert (every? (partial instance? ErrorForm) error-forms))
  {ErrorForm {"recide/error" error-form-writer}})

(defn construct-read-handlers
  [& error-forms]
  (assert (every? (partial instance? ErrorForm) error-forms))
  {"recide/error" (construct-error-form-reader (cons ri/default-error-definition error-forms))})
