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

(ns recide.error-test
  (:require [clojure.test :refer :all]
            [recide.impl :refer [default-error-definition *capture-insists*]]
            [recide.core :refer :all]
            [recide.utils :as utils]
            [recide.error :as err]
            [recide.error-test-declarations :as etd
             :refer [raise-referred-custom, raise-referred-group]]
            [recide.error-test-declarations-2 :as etd2])
  (:import (clojure.lang ExceptionInfo)
           [java.io IOException]))

(deftest test:error-functions
  (testing "error construction"
    (is (= {:recide/error default-error-definition
            :recide/type  ::test-err
            :recide/msg  "test message"
            :recide/data {:foo :bar}}
           (error-map ::test-err
                      "test message"
                      {:foo :bar})))
    (is (error-map? (error-map ::test-err
                               "test message"
                               {:foo :bar})))
    (let [e (Exception.)]
      (is (= {:recide/error default-error-definition
              :recide/type   ::test-err
              :recide/msg   "test message"
              :recide/data  {:foo :bar}
              :recide/cause e}
             (error-map ::test-err
                        "test message"
                        {:foo :bar}
                        e))))
    (is (error-map? (error-map ::test-err
                               "test message"
                               {:foo :bar}
                               (Exception.)))))
  (testing "ex-info and error:"
    (let [err (error-map ::test-err
                         "test message"
                         {:foo :bar})]
      (is (instance? clojure.lang.IExceptionInfo (error-map->throwable err)) "it's an IExceptionInfo")
      (is (error? (error-map->throwable err)) "we can convert error to ex-info form.")
      (is (not (error? (ex-info "test ex-info" {:foo :bar}))) "plain ex-infos can be distinguished.")
      (is (= err (-> err error-map->throwable error->map)) "we can roundtrip conversion."))))

(deftest test:custom-error-functions
  (testing "error construction"
    (is (= {:recide/error etd/test-custom-error
            :foo/class  ::test-err
            :foo/message  "test message"
            :foo/info {:foo :bar}}
           (etd/error-map ::test-err
                          "test message"
                          {:foo :bar})))
    (is (error-map? (etd/error-map ::test-err
                                   "test message"
                                   {:foo :bar})))
    (is (etd/error-map? (etd/error-map ::test-err
                                       "test message"
                                       {:foo :bar})))
    (is (not (etd2/error-map? (etd/error-map ::test-err
                                             "test message"
                                             {:foo :bar}))))
    (is (not (etd/error-map? (etd2/error-map ::test-err
                                             "test message"
                                             {:foo :bar}))))
    (is (= (dissoc (ex-data (etd/error-map ::test-err
                                           "test message"
                                           {:foo :bar}))
                   :recide/error)
           (dissoc (ex-data (etd2/error-map ::test-err
                                            "test message"
                                            {:foo :bar}))
                   :recide/error)))
    (is (not (etd/error-map? (error-map ::test-err
                                        "test message"
                                        {:foo :bar})))
        "Specialized error-map? method only recognizes specialized errors!")
    (let [e (Exception.)]
      (is (= {:recide/error etd/test-custom-error
              :foo/class   ::test-err
              :foo/message   "test message"
              :foo/info  {:foo :bar}
              :foo/occasion e}
             (etd/error-map ::test-err
                            "test message"
                            {:foo :bar}
                            e))))
    (is (error-map? (etd/error-map ::test-err
                                   "test message"
                                   {:foo :bar}
                                   (Exception.)))))
  (testing "ex-info and error:"
    (let [err (etd/error-map ::test-err
                             "test message"
                             {:foo :bar})]
      (is (instance? clojure.lang.IExceptionInfo (error-map->throwable err)) "it's an IExceptionInfo")
      (is (error? (error-map->throwable err)) "we can convert error to ex-info form.")
      (is (etd/error? (error-map->throwable err)) "we can convert error to ex-info form.")
      (is (not (error? (ex-info "test ex-info" {:foo :bar}))) "plain ex-infos can be distinguished.")
      (is (= (ex-data (error-map->throwable err))
             (-> err error-map->throwable error->map error-map->throwable ex-data))
          "ex-data on errors round-trips")
      (let [err2 (-> err error-map->throwable error->map)]
        (doseq [k (keys err)]
          (when-not (= k :foo/info)
            (is (= (get err k) (get err2 k)) (str "testing " k))))))))

(deftest unit:required-data
  (let [simple-regex #"Assertion failed: .* requires the following missing keys in its data: .*"
        group-regex #"Assertion failed: .* called with subtype .* requires the following missing keys in its data: .*"]
    (testing "expressions using macros output by defraise must check for required keys in data maps."
      (is (thrown-with-msg? Exception
                            simple-regex
                            (eval `(recide.error-test-declarations/raise-namespaced-custom "I'm gonna make him an offer he can't refuse." {}))))
      (is (thrown-with-msg? Exception
                            simple-regex
                            (eval `(recide.error-test-declarations/raise-namespaced-custom "Inconceivable!" {} (Exception.)))))
      (is (thrown-with-msg? Exception
                            simple-regex
                            (eval `(etd/raise-aliased-custom "Here's looking at you, kid." {}))))
      (is (thrown-with-msg? Exception
                            simple-regex
                            (eval `(etd/raise-aliased-custom "Not all treasure is silver and gold, mate." {} (Exception.)))))
      (is (thrown-with-msg? Exception
                            simple-regex
                            (eval `(raise-referred-custom "I think this is the beginning of a beautiful friendship." {}))))
      (is (thrown-with-msg? Exception
                            simple-regex
                            (eval `(raise-referred-custom "First law of political indiscretion: always have a drink before you leak." {} (Exception.))))))
    (testing "expressions using macros output by defraise-group must check for required keys in data maps."
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(recide.error-test-declarations/raise-namespaced-group :subtype-2 "Bond. James Bond." {}))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(recide.error-test-declarations/raise-namespaced-group :subtype-2 "In that case, Minister, I think it's best I file it." {} (Exception.)))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(etd/raise-aliased-group :subtype-2 "After all, tomorrow is another day!" {}))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(etd/raise-aliased-group :subtype-2 "Well, four can play at this game, buddy." {} (Exception.)))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(raise-referred-group :subtype-2 "Seize the day, boys. Make your lives extraordinary." {}))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(raise-referred-group :subtype-2 "I know it ain't mine, so go ahead." {} (Exception.))))))
    (testing "error types defined in raise-groups may have heterogeneous info-map requirements."
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(recide.error-test-declarations/raise-namespaced-group :subtype-1 "I'll get you, my pretty, and your little dog, too!"
                                                                                          {:required :I}))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(recide.error-test-declarations/raise-namespaced-group :subtype-1 "One dance, one look, one kiss, that's all we get, Albert."
                                                                                          {:required :I} (Exception.)))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(etd/raise-aliased-group :subtype-1 "What we've got here is failure to communicate." {:required :am}))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(etd/raise-aliased-group :subtype-1 "Where we're going, we don't need roads." {:required :am} (Exception.)))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(raise-referred-group :subtype-1 "Hey, it's good to be a young man and to live the way you please." {:required :here}))))
      (is (thrown-with-msg? Exception
                            group-regex
                            (eval `(raise-referred-group :subtype-1 "And he's fair and he's true and he's boring as hell and he'll go to the grave as an angry old man" {:required :here} (Exception.))))))))

(deftest unit:raises-raise-actually
  (testing "raises defined by defraise raise."
    (is (thrown-with-msg? Exception
                          #"An error was raised from namespaced-custom-raise: When I was your age, television was called books."
                          (recide.error-test-declarations/raise-namespaced-custom "When I was your age, television was called books." {:required :books})))
    (is (thrown-with-msg? Exception
                          #"An error was raised from namespaced-custom-raise: When I was your age, television was called books."
                          (recide.error-test-declarations/raise-namespaced-custom "When I was your age, television was called books." {:required :books} (Exception.))))
    (is (thrown-with-msg? Exception
                          #"An error was raised from aliased-custom-raise: Pigeons fly, women fall from the sky!"
                          (etd/raise-aliased-custom "Pigeons fly, women fall from the sky!" {:required :books})))
    (is (thrown-with-msg? Exception
                          #"An error was raised from aliased-custom-raise: Pigeons fly, women fall from the sky!"
                          (etd/raise-aliased-custom "Pigeons fly, women fall from the sky!" {:required :books} (Exception.))))
    (is (thrown-with-msg? Exception
                          #"An error was raised from referred-custom-raise: We must work in the world, your eminence. The world is thus."
                          (raise-referred-custom "We must work in the world, your eminence. The world is thus." {:required :books})))
    (is (thrown-with-msg? Exception
                          #"An error was raised from referred-custom-raise: We must work in the world, your eminence. The world is thus."
                          (raise-referred-custom "We must work in the world, your eminence. The world is thus." {:required :books} (Exception.)))))
  (testing "raises defined by generated defraise raise."
    (is (thrown-with-msg? Exception
                          #"When I was your age, television was called books."
                          (recide.error-test-declarations/raise :foo/mac "When I was your age, television was called books." {:required :books})))
    (is (thrown-with-msg? Exception
                          #"When I was your age, television was called books."
                          (recide.error-test-declarations/raise :blah/foo "When I was your age, television was called books." {:required :books} (Exception.))))
    (is (thrown-with-msg? Exception
                          #"Pigeons fly, women fall from the sky!"
                          (etd/raise :happy/pigs "Pigeons fly, women fall from the sky!" {:required :books})))
    (is (thrown-with-msg? Exception
                          #"Pigeons fly, women fall from the sky!"
                          (etd/raise :charming/penguins "Pigeons fly, women fall from the sky!" {:required :books} (Exception.)))))
  (testing "raises defined by defraise-group raise."
    (is (thrown-with-msg? Exception
                          #"I'm a namespaced group error of subtype 2.: Typical. Just when you're getting ahead, someone changes the odds."
                          (recide.error-test-declarations/raise-namespaced-group :subtype-2
                                                                                 "Typical. Just when you're getting ahead, someone changes the odds."
                                                                                 {:required :data})))
    (is (thrown-with-msg? Exception
                          #"I'm a namespaced group error of subtype 2.: Typical. Just when you're getting ahead, someone changes the odds."
                          (recide.error-test-declarations/raise-namespaced-group :subtype-2
                                                                                 "Typical. Just when you're getting ahead, someone changes the odds."
                                                                                 {:required :data}
                                                                                 (Exception.))))
    (is (thrown-with-msg? Exception
                          #"I'm an aliased group error of subtype 2.: Yes, if only we had a magical tool that could slow time down."
                          (etd/raise-aliased-group :subtype-2
                                                   "Yes, if only we had a magical tool that could slow time down."
                                                   {:required :courses})))
    (is (thrown-with-msg? Exception
                          #"I'm an aliased group error of subtype 2.: Yes, if only we had a magical tool that could slow time down."
                          (etd/raise-aliased-group :subtype-2
                                                   "Yes, if only we had a magical tool that could slow time down."
                                                   {:required :courses}
                                                   (Exception.))))
    (is (thrown-with-msg? Exception
                          #"I'm a referred group error of subtype 1.: Hegel is arguing that the reality is merely an a priori adjunct of non-naturalistic ethics."
                          (raise-referred-group :subtype-1
                                                "Hegel is arguing that the reality is merely an a priori adjunct of non-naturalistic ethics."
                                                {:required :immunizations,
                                                 :necessary :sufficient})))
    (is (thrown-with-msg? Exception
                          #"I'm a referred group error of subtype 1.: Hegel is arguing that the reality is merely an a priori adjunct of non-naturalistic ethics."
                          (raise-referred-group :subtype-1
                                                "Hegel is arguing that the reality is merely an a priori adjunct of non-naturalistic ethics."
                                                {:required :immunizations,
                                                 :necessary :sufficient}
                                                (Exception.))))))

(deftest unit:raise-ex-data
  (testing "Checking ex-data on raise's ex-infos."
    (let [data {:required [0 1 2 3]
                :necessary "alphabet soup"
                :superfluous {:really :this :is :a :bit :dumb}
                :recide/error default-error-definition}]
      (is (= (assoc data :recide/type :recide/simplest-raise)
             (try (etd/raise-simplest "a raise would be nice"
                                      {:required [0 1 2 3]
                                       :necessary "alphabet soup"
                                       :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide/namespaced-simple)
             (try (recide.error-test-declarations/raise-namespaced-custom "yup"
                                                                          {:required [0 1 2 3]
                                                                           :necessary "alphabet soup"
                                                                           :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide/aliased-simple)
             (try (etd/raise-aliased-custom "totally"
                                            {:required [0 1 2 3]
                                             :necessary "alphabet soup"
                                             :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide/referred-simple)
             (try (raise-referred-custom "mhm"
                                         {:required [0 1 2 3]
                                          :necessary "alphabet soup"
                                          :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide.namespaced-group/subtype-1)
             (try (recide.error-test-declarations/raise-namespaced-group :subtype-1
                                                                         "dunno what's up"
                                                                         {:required [0 1 2 3]
                                                                          :necessary "alphabet soup"
                                                                          :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide.namespaced-group/subtype-2)
             (try (recide.error-test-declarations/raise-namespaced-group :subtype-2
                                                                         "dunno what's up"
                                                                         {:required [0 1 2 3]
                                                                          :necessary "alphabet soup"
                                                                          :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide.aliased-group/subtype-1)
             (try (etd/raise-aliased-group :subtype-1
                                           "whatevs"
                                           {:required [0 1 2 3]
                                            :necessary "alphabet soup"
                                            :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide.aliased-group/subtype-2)
             (try (etd/raise-aliased-group :subtype-2
                                           "avoid the flu"
                                           {:required [0 1 2 3]
                                            :necessary "alphabet soup"
                                            :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide.referred-group/subtype-1)
             (try (raise-referred-group :subtype-1
                                        "blah blah blah blah blah blah blah"
                                        {:required [0 1 2 3]
                                         :necessary "alphabet soup"
                                         :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))
      (is (= (assoc data :recide/type :recide.referred-group/subtype-2)
             (try (raise-referred-group :subtype-2
                                        "Hello, I love you won't you tell me your name?"
                                        {:required [0 1 2 3]
                                         :necessary "alphabet soup"
                                         :superfluous {:really :this :is :a :bit :dumb}})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e)))))))
  (testing "Checking ex-data on the generated raise's ex-infos."
    (let [data {:apples :red
                :bananas :yellow
                :recide/error etd/test-custom-error}]
      (is (= (assoc data :foo/class :partition/map)
             (try (etd/raise :partition/map
                             "a raise would be nice"
                             {:apples :red
                              :bananas :yellow})
                  (catch clojure.lang.ExceptionInfo e
                    (ex-data e))))))))

(deftest unit:try*-basics
  (testing "try* should be transparent and behave in the base case like a do"
    (is (= 3 (try* (+ 1 2))))
    (is (= 4 (try* (+ 1 2)
                   (+ 2 2))))
    (let [a (atom nil)]
      (is (= 5 (try* (reset! a 5)
                     (+ 2 3))
             @a))))
  (testing "you can catch from within a try*"
    (is (thrown-with-msg? Exception
                          #"Hi hi!"
                          (try* (throw (Exception. "Hi hi!")))))
    (is (= 1 (try* (throw (Exception.))
                   (catch Exception e
                     (* 2 1/2))))))
  (testing "catches are evaluated in order."
    (is (= 1 (try* (throw (ex-info "hi" {}))
                   (catch ExceptionInfo e
                     1)
                   (catch Exception e
                     2))))
    (is (= 2 (try* (throw (ex-info "hi" {}))
                   (catch Exception e
                     2)
                   (catch ExceptionInfo e
                     1)))))
  (testing "you can catch recide errors by specifying a keyword type, or just a namespace with wildcard."
    (is (thrown-with-msg? ExceptionInfo
                          #"You have offended the library author's sensibilities for its usage."
                          (try* (raise :designationexpanse/kind
                                       "You have offended the library author's sensibilities for its usage."
                                       {:data :some-context}))))
    (is (= 5 (try* (raise :designationexpanse/kind
                          "You have offended the library author's sensibilities for its usage."
                          {:data :some-context})
                   (catch :designationexpanse/kind e
                     (/ 20 4)))))
    (is (= 8 (try* (raise :designationexpanse/kind
                          "You have offended the library author's sensibilities for its usage."
                          {:data :some-context})
                   (catch :designationexpanse/* e
                     (* 2 2 2))))))
  (testing "catches within a try* don't care what ErrorForm was used."
    (is (thrown-with-msg? ExceptionInfo
                          #"You have offended the library author's sensibilities for its usage."
                          (try* (etd/raise :designationexpanse/kind
                                           "You have offended the library author's sensibilities for its usage."
                                           {:data :some-context}))))
    (is (= 5 (try* (etd/raise :designationexpanse/kind
                              "You have offended the library author's sensibilities for its usage."
                              {:data :some-context})
                   (catch :designationexpanse/kind e
                     (/ 20 4)))))
    (is (= 8 (try* (etd2/raise :designationexpanse/kind
                               "You have offended the library author's sensibilities for its usage."
                               {:data :some-context})
                   (catch :designationexpanse/* e
                     (* 2 2 2))))))
  (testing "you can catch exceptions by specifying a predicate function."
    (letfn [(ex-info-with-key? [k v]
              (fn [e]
                (and (instance? ExceptionInfo e)
                     (= v (get (ex-data e) k)))))]
      (is (= 5 (try* (raise :designationexpanse/kind
                            "You have offended the library author's sensibilities for its usage."
                            {:data :some-context})
                     (catch (ex-info-with-key? :data :some-context) e
                       (/ 20 4)))))
      (is (= 8 (try* (raise :designationexpanse/kind
                            "You have offended the library author's sensibilities for its usage."
                            {:data :some-other-context})
                     (catch (ex-info-with-key? :data :some-context) e
                       (* 2 2 2 2 2))
                     (catch (ex-info-with-key? :data :some-other-context) e
                       (* 2 2 2))))))))

(deftest unit:try*-with-modifiers
  (testing "catches in try* should handle negated selections"
    (is (= :escrow
           (try* (raise :throwable/exception
                        "Almost vanilla."
                        {:escalating :escrow})
                 (catch (:not :throwable/frisbee) boo
                   (:escalating (ex-data boo))))))
    (is (= :small-brewery
           (try* (raise :shiner/bock
                        "Clean flavor and a slightly sweet finish."
                        {:small-town :small-brewery})
                 (catch (:not IllegalArgumentException) prosit
                   (:small-town (ex-data prosit))))))
    (is (= :with-salt
           (try* (raise :delenda/est
                        "Carthago!"
                        {:sewn :with-salt})
                 (catch (:not (partial instance? IllegalArgumentException)) mediterranean
                   (:sewn (ex-data mediterranean)))))))
  (testing "catches in try* should handle conjunctive combinations"
    (is (= 11
           (try* (raise :angelic/choir
                        "Perfection itself."
                        {:extraordinary! :blast-it})
                 (catch :and [ExceptionInfo :angoric/wool] e
                        10)
                 (catch :and [ExceptionInfo :angelic/*] e
                        11)))))
  (testing "catches in try* should handle disjunctive combinations"
    (is (= 13
           (try* (raise :market-reach/juggernaut
                        "That's not exactly right."
                        {:extraordinary! :blast-it})
                 (catch :or [IllegalArgumentException :market-breach/*] e
                        12)
                 (catch :or [IllegalArgumentException :market-reach/*] e
                        13)))))
  (testing "catches in try* should handle conjunction and negation"
    (is (= :c
           (try* (raise :pilot/corporation
                        "Refill for roller ball pen"
                        {:hi-tec :c})
                 (catch :and [:pilot/* (:not RuntimeException)] e
                        :d)
                 (catch :and [ExceptionInfo (:not :passenger/*)] e
                        (:hi-tec (ex-data e)))))))
  (testing "catches in try* should handle disjunction and negation"
    (is (= :confabulate
           (try* (raise :fabula/fabulae
                        "story, tale, play"
                        {:fabulous :confabulate})
                 (catch :or [IllegalArgumentException (:not :imperium/imperii)] e
                        (:fabulous (ex-data e))))))
    (is (= :text
           (try* (raise :canon/biblical
                        "The mythical council of Jamnia"
                        {:masoretic :text})
                 (catch :or [ExceptionInfo (:not :canon/biblical)] e
                        (:masoretic (ex-data e))))))))

(deftest unit:modify-error-ex-data
  (testing "setting ex-data on error"
    (is (= :amen (:can-i-get-an-amen
                  (-> (raise :angelic/choir "That's us." {:can-i-get-an-amen nil})
                      (try (catch ExceptionInfo e
                             (throw (assoc-error e :can-i-get-an-amen :amen))))
                      (try (catch ExceptionInfo e
                             (ex-data e))))))))
  (testing "updating ex-data on error"
    (is (= 3
           (-> (raise :foo/type "I'm a foo." {:blah-dee-blah 0})
               (try (catch ExceptionInfo e
                      (throw (update-error e :blah-dee-blah inc))))
               (try (catch ExceptionInfo e
                      (throw (update-error e :blah-dee-blah inc))))
               (try (catch ExceptionInfo e
                      (throw (update-error e :blah-dee-blah inc))))
               (try (catch ExceptionInfo e
                      (:blah-dee-blah (ex-data e)))))))))

(deftest test:insist-expression-capture
  (testing "captures when it should"
    (let [error (try
                  (binding [*capture-insists* true]
                    (eval `(let [x# 3] (insist (= x# 4)))))
                  (catch Exception e e))]
      (is (map? (ex-data error)))
      (is (contains? (ex-data error) :values))))
  (testing "captures not when it should not"
    (let [error (try
                  (binding [*capture-insists* false]
                    (eval `(let [x# 3] (insist (= x# 4)))))
                  (catch Exception e e))]
      (is (map? (ex-data error)))
      (is (not (contains? (ex-data error) :values))))))

(deftest test:serializability
  (testing "can round-trip"
    (let [err (error :foo/bad "my message" {})
          round-tripped (utils/deserialize-throwable (utils/serialize-throwable err))]
      (is (= (.getMessage ^Exception err) (.getMessage ^Exception round-tripped)))
      (is (= (dissoc (ex-data err) :recide/error)
             (dissoc (ex-data round-tripped) :recide/error)))
      (doseq [:let [prev (error->form err)
                    after (error->form round-tripped)]
              test-fn [err/serialization-tag
                       err/type-keyword
                       err/message-keyword
                       err/data-keyword
                       err/cause-keyword
                       err/serialized-keyword
                       (comp (memfn ^java.lang.Class getName) type err/constructor)
                       err/metadata-fns]]
        (is (= (test-fn prev) (test-fn after))
            (format "%s is not %s" (test-fn prev) (test-fn after))))))
  (testing "can round-trip even when nested"
    (let [err (error :foo/bad "my message" {} (error :peacock/feathers "yo dawg" {}))
          round-tripped (utils/deserialize-throwable (utils/serialize-throwable err))]
      (is (= (.getMessage ^Exception err) (.getMessage ^Exception round-tripped)))
      (is (= (dissoc (ex-data err) :recide/error)
             (dissoc (ex-data round-tripped) :recide/error)))
      (doseq [:let [prev (error->form err)
                    after (error->form round-tripped)]
              test-fn [err/serialization-tag
                       err/type-keyword
                       err/message-keyword
                       err/data-keyword
                       err/cause-keyword
                       err/serialized-keyword
                       (comp (memfn ^java.lang.Class getName) type err/constructor)
                       err/metadata-fns]]
        (is (= (test-fn prev) (test-fn after))
            (format "%s is not %s" (test-fn prev) (test-fn after)))))))
