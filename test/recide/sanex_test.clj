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

(ns recide.sanex-test
  (:require [clojure.test :refer :all]
            [recide.sanex :as sx]
            [recide.impl :as impl]
            [recide.core :as rc])
  (:import [recide.sanex Utils]))

(deftest recursive-suppression
  (testing "Clojure tools directly"
    (let [cause ^Exception (rc/error :trojan/horse "Greeks bearing gifts." {:whos-the-hero :odysseus})
          error ^Exception (rc/error :windows/blue "Death." {:micro$oft :$tinks} cause)]
      (testing "recursively, sanitize suppresses"
        (testing "causes"
          (testing "not by default"
            (is (instance? Exception (.getCause (sx/sanitize error)))))
          (testing "on command"
            (is (nil? (.getCause (sx/sanitize error {:suppress-cause? true}))))))
        (testing "messages"
          (testing "by default"
            (is (= "" (.getMessage (sx/sanitize error))))
            (is (= "" (.getMessage (.getCause (sx/sanitize error))))))
          (testing "not when disabled"
            (is (= "Death." (.getMessage (sx/sanitize error {:suppress-message? false}))))
            (is (= "Greeks bearing gifts." (.getMessage (.getCause (sx/sanitize error {:suppress-message? false})))))))
        (testing "stack traces"
          (testing "not by default"
            (is (< 0 (alength (.getStackTrace (sx/sanitize error)))))
            (is (< 0 (alength (.getStackTrace (.getCause (sx/sanitize error)))))))
          (testing "on command"
            (is (= 0 (alength (.getStackTrace (sx/sanitize error {:suppress-stack? true})))))
            (is (= 0 (alength (.getStackTrace (.getCause (sx/sanitize error {:suppress-stack? true}))))))))
        (testing "ex-info data"
          (testing "by default without removing error type."
            (is (= {:recide/type :windows/blue}
                   (ex-data (sx/sanitize error))))
            (is (= {:recide/type :trojan/horse}
                   (ex-data (.getCause (sx/sanitize error))))))
          (testing "not when disabled."
            (is (= {:recide/type :windows/blue
                    :recide/error impl/default-error-definition
                    :micro$oft :$tinks}
                   (ex-data (sx/sanitize error {:suppress-data? false}))))
            (is (= {:recide/type :trojan/horse
                    :recide/error impl/default-error-definition
                    :whos-the-hero :odysseus}
                   (ex-data (.getCause (sx/sanitize error {:suppress-data? false}))))))))))
  (testing "recide.sanex.Utils"
    (let [cause ^Exception (rc/error :trojan/horse "Greeks bearing gifts." {:whos-the-hero :odysseus})
          error ^Exception (rc/error :windows/blue "Death." {:micro$oft :$tinks} cause)]
      (testing "recursively, sanitize suppresses"
        (testing "causes"
          (testing "not by default"
            (is (instance? Exception (.getCause (Utils/sanitize error)))))
          (testing "on command"
            (is (nil? (.getCause (Utils/sanitize error {:suppress-cause? true}))))))
        (testing "messages"
          (testing "by default"
            (is (= "" (.getMessage (Utils/sanitize error))))
            (is (= "" (.getMessage (.getCause (Utils/sanitize error))))))
          (testing "not when disabled"
            (is (= "Death." (.getMessage (Utils/sanitize error {:suppress-message? false}))))
            (is (= "Greeks bearing gifts." (.getMessage (.getCause (Utils/sanitize error {:suppress-message? false})))))))
        (testing "stack traces"
          (testing "not by default"
            (is (< 0 (alength (.getStackTrace (Utils/sanitize error)))))
            (is (< 0 (alength (.getStackTrace (.getCause (Utils/sanitize error)))))))
          (testing "on command"
            (is (= 0 (alength (.getStackTrace (Utils/sanitize error {:suppress-stack? true})))))
            (is (= 0 (alength (.getStackTrace (.getCause (Utils/sanitize error {:suppress-stack? true}))))))))
        (testing "ex-info data"
          (testing "by default without removing error type."
            (is (= {:recide/type :windows/blue}
                   (ex-data (Utils/sanitize error))))
            (is (= {:recide/type :trojan/horse}
                   (ex-data (.getCause (Utils/sanitize error))))))
          (testing "not when disabled."
            (is (= {:recide/type :windows/blue
                    :recide/error impl/default-error-definition
                    :micro$oft :$tinks}
                   (ex-data (Utils/sanitize error {:suppress-data? false}))))
            (is (= {:recide/type :trojan/horse
                    :recide/error impl/default-error-definition
                    :whos-the-hero :odysseus}
                   (ex-data (.getCause (Utils/sanitize error {:suppress-data? false})))))))))))

(deftest opt-out-suppression
  (let [root-cause ^Exception (rc/error :wiring/fault "Bzzzt." {:ungrounded? true})
        cause ^Exception (rc/error :ankle/Styx "Oops." {:wash :carefully} root-cause)
        error ^Exception (sx/specify-sanitization
                          (rc/error :mac/frownie-face "Boop." {:apple+shift :power} cause)
                          (assoc sx/noop-sanitization :suppress-recursively? true))
        oppression {:suppress-data? true,
                    :suppress-cause? true,
                    :suppress-message? true,
                    :suppress-stack? true,
                    :suppress-recursively? true}]
    (testing "opt out of suppression"
      (testing "via whitelisting"
        (let [sanitized (sx/sanitize error oppression)]
          (is (instance? Exception (.getCause sanitized)))
          (is (nil? (.getCause (.getCause sanitized))))
          (is (= "Boop." (.getMessage sanitized)))
          (is (= "" (.getMessage (.getCause sanitized))))
          (is (< 0 (alength (.getStackTrace sanitized))))
          (is (= 0 (alength (.getStackTrace (.getCause sanitized)))))
          (is (= {:recide/type :mac/frownie-face
                  :recide/error impl/default-error-definition
                  :apple+shift :power
                  :recide.sanex/sanitary? (assoc sx/noop-sanitization
                                                 :suppress-recursively?
                                                 true)}
                 (ex-data sanitized)))
          (is (= {:recide/type :ankle/Styx}
                 (ex-data (.getCause sanitized))))))
      (testing "via convenient and artfully-crafted macro"
        (sx/without-sanitization
         (let [sanitized (sx/sanitize error)]
           (is (instance? Exception (.getCause sanitized)))
           (is (instance? Exception (.getCause (.getCause sanitized))))
           (is (= "Boop." (.getMessage sanitized)))
           (is (= "Oops." (.getMessage (.getCause sanitized))))
           (is (= "Bzzzt." (.getMessage (.getCause (.getCause sanitized)))))
           (is (< 0 (alength (.getStackTrace sanitized))))
           (is (< 0 (alength (.getStackTrace (.getCause sanitized)))))
           (is (< 0 (alength (.getStackTrace (.getCause (.getCause sanitized))))))
           (is (= {:recide/type :mac/frownie-face
                   :recide/error impl/default-error-definition
                   :apple+shift :power
                   :recide.sanex/sanitary? (assoc sx/noop-sanitization
                                                  :suppress-recursively?
                                                  true)}
                  (ex-data sanitized)))
           (is (= {:recide/type :ankle/Styx
                   :recide/error impl/default-error-definition
                   :wash :carefully}
                  (ex-data (.getCause sanitized))))
           (is (= {:recide/type :wiring/fault
                   :recide/error impl/default-error-definition
                   :ungrounded? true}
                  (ex-data (.getCause (.getCause sanitized)))))))))))
