(ns org.noisesmith.adj-test
  (:require [clojure.test :refer [deftest is testing]]
            [org.noisesmith.adj :as adj])
  (:import (org.noisesmith.adj PersistentAdj)))

(def adj-list
  (-> (PersistentAdj. {} {} #{})
      (adj/insert :a nil)
      (adj/insert :b nil)
      (adj/insert :c nil)
      (adj/connect :a :b)
      (adj/connect :b :c)))

(deftest adj-linked-list
  (let [list-a (adj/->list adj-list :a)
        list-b (adj/->list adj-list :b)
        list-c (adj/->list adj-list :c)]
    (is (= [{:node :a :val nil} {:node :b :val nil} {:node :c :val nil}]
           list-a))
    (is (= [{:node :b :val nil} {:node :c :val nil}]
           list-b))
    (is (= [{:node :c :val nil}]
           list-c))))

(deftest adj-linked-list-tree
  (let [[tree visited] (adj/->tree adj-list :a)]
    (is (= visited #{:a :b :c}))
    (is (= {:node :a
            :val nil
            :children {:b {:node :b
                           :val nil
                           :children {:c {:node :c
                                          :val nil}}}}}
           tree))))

(def adj-cycle
  (adj/connect adj-list :c :a))

(deftest adj-cycle-list
  (let [cycle-list (take 10000 (adj/->list adj-cycle))]
    (is (= 10000 (count cycle-list)))
    (is (= #{{:node :a :val nil} {:node :b :val nil} {:node :c :val nil}}
           (into #{} cycle-list)))))

(deftest adj-cycle-tree
  (let [[tree-a visited-a] (adj/->tree adj-cycle :a)
        [tree-b visited-b] (adj/->tree adj-cycle :b)
        [tree-c visited-c] (adj/->tree adj-cycle :c)
        mk-tree (fn [a b c]
                  {:node a
                   :val nil
                   :children {b {:node b
                                 :val nil
                                 :children {c {:node c
                                               :val nil}}}}})]
    (is (= visited-a
           visited-b
           visited-c
           #{:a :b :c}))
    (is (= tree-a (mk-tree :a :b :c)))
    (is (= tree-b (mk-tree :b :c :a)))
    (is (= tree-c (mk-tree :c :a :b)))))
