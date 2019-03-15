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

(ns recide.error
  (:require [recide.utils :refer [serialize-throwable deserialize-throwable]])
  (:import [recide.error ErrorForm]
           [clojure.lang Keyword IFn APersistentMap]))

(def default-serialization-tag "recide/error")
(def default-type-keyword :recide/type)
(def default-message-keyword :recide/msg)
(def default-data-keyword :recide/data)
(def default-cause-keyword :recide/cause)
(def default-serialized-keyword :recide.serialized-throwable/v2)
(def default-constructor ex-info)
(def default-metadata-fns {})

(defmacro ^:private define-wrapper
  [name wrapped]
  `(def ~name (memfn ~(with-meta wrapped {:tag 'ErrorForm}))))

(define-wrapper serialization-tag getSerializationTag)
(define-wrapper type-keyword getTypeKeyword)
(define-wrapper message-keyword getMessageKeyword)
(define-wrapper data-keyword getDataKeyword)
(define-wrapper cause-keyword getCauseKeyword)
(define-wrapper serialized-keyword getSerializedKeyword)
(define-wrapper constructor getConstructor)
(define-wrapper metadata-fns getMetadataFunctions)
(define-wrapper raised-sites getRaisedSites)

(defn error-form
  "Constructor for recide.error/ErrorForm. Values not supplied in the map here will default
  to the same behavior as recide.core errors. All available keys illustrated here with return
  values corresponding to recide defaults:
  ```
  (error-form
     {:serialization-tag \"recide/error\"
      :type-keyword :recide/type
      :message-keyword :recide/msg
      :data-keyword :recide/data
      :cause-keyword :recide/cause
      :serialized-keyword :recide.serialized-throwable/v2
      :serializer recide.utils/serialize-throwable
      :constructor clojure.core/ex-info
      :metadata-fns {}})
  ```"
  [{:keys [serialization-tag,
           type-keyword,
           message-keyword,
           data-keyword,
           cause-keyword,
           serialized-keyword,
           constructor,
           metadata-fns]
    :or {serialization-tag "recide/error",
         type-keyword :recide/type,
         message-keyword :recide/msg,
         data-keyword :recide/data,
         cause-keyword :recide/cause,
         serialized-keyword :recide.serialized-throwable/v1,
         constructor ex-info,
         metadata-fns {}}}]
  (ErrorForm. ^String serialization-tag
              ^Keyword type-keyword
              ^Keyword message-keyword
              ^Keyword data-keyword
              ^Keyword cause-keyword
              ^Keyword serialized-keyword
              ^IFn constructor
              ^APersistentMap metadata-fns))
