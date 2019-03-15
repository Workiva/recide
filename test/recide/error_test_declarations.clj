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

(ns recide.error-test-declarations
  (:require [recide.core :as rc]
            [recide.error :as rerr]
            [recide.utils :refer [serialize-throwable deserialize-throwable]])
  (:import [java.util ArrayList]))

(rc/deferror simplest
  :recide/simplest-raise
  "The simplest raise of all raises.")

(rc/deferror namespaced-custom
  :recide/namespaced-simple
  "An error was raised from namespaced-custom-raise"
  [:required])

(rc/deferror aliased-custom
  :recide/aliased-simple
  "An error was raised from aliased-custom-raise"
  [:required])

(rc/deferror referred-custom
  :recide/referred-simple
  "An error was raised from referred-custom-raise"
  [:required])

(rc/deferror-group namespaced-group
  (:recide.namespaced-group [:required])
  (subtype-1 "I'm a namespaced group error of subtype 1." [:necessary])
  (subtype-2 "I'm a namespaced group error of subtype 2."))

(rc/deferror-group aliased-group
  (:recide.aliased-group [:required])
  (subtype-1 "I'm an aliased group error of subtype 1." [:necessary])
  (subtype-2 "I'm an aliased group error of subtype 2."))

(rc/deferror-group referred-group
  (:recide.referred-group [:required])
  (subtype-1 "I'm a referred group error of subtype 1." [:necessary])
  (subtype-2 "I'm a referred group error of subtype 2."))

(defn custom-constructor
  ([m d] (ex-info m (dissoc d ::original)))
  ([m d c] (ex-info m (dissoc d ::original) c)))

(def test-custom-error
  (rerr/error-form
   {:serialization-tag "etd"
    :type-keyword :foo/class,
    :message-keyword :foo/message,
    :data-keyword :foo/info,
    :cause-keyword :foo/occasion,
    :serialized-keyword :foo/i-done-serialized
    :constructor custom-constructor
    :metadata-fns {::original identity}}))

(def ^:dynamic *foo*)

(rc/generate-library! test-custom-error *foo*)
