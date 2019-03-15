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

(ns recide.error-test-declarations-2
  (:require [recide.core :as rc]))

(def test-equivalent-error
  (rc/error-form
   {:serialization-tag "etd2"
    :type-keyword :foo/class,
    :message-keyword :foo/message,
    :data-keyword :foo/info,
    :cause-keyword :foo/occasion,
    :serialized-keyword :foo/i-done-serialized}))

(def ^:dynamic *bar*)

(rc/generate-library! test-equivalent-error *bar*)
