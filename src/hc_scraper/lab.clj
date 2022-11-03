(ns hc-scraper.lab
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.io File)))


(defn read-bytes
  ^bytes [^File file]
  (with-open [in (io/input-stream file)]
    (.readAllBytes in)))


(def ^bytes target-a (byte-array [16r39 16r8E 16rE3 16r3F]))
(def ^bytes target-b (byte-array [16rE2 16rE1 16rE1 16r3F]))

(def ^bytes replacement-ratio (byte-array [16rCD 16rCC 16rCC 16r3F])) ;; 16/10 ratio. more here: https://www.wsgf.org/article/common-hex-values

(defn same-bytes?
  [^bytes src ^long index ^bytes target]
  (loop [i 0]
    (if (= i 4)
      true
      (if (= (aget src (+ i index))
            (aget target i))
        (recur (inc i))
        false))))


(defn replace-ratio-bytes
  [^File file]
  (let [all-bytes (read-bytes file)
        len       (alength all-bytes)]
    (loop [i        0
           changed? false]
      (if (= i len)
        (when changed? all-bytes)
        (do
          (let [found? (or (same-bytes? all-bytes i target-a)
                           (same-bytes? all-bytes i target-b))]
            (if found? (System/arraycopy replacement-ratio 0 all-bytes i 4))
            (recur (inc i) (or changed? found?))))))))


(comment

  (defmacro for-loop
    [for-args body]
    (let [arg-name      (nth for-args 0)
          init-value    (nth for-args 1)
          condition     (nth for-args 2)
          post-eval-exp (nth for-args 3)]
      `(loop [~arg-name ~init-value]
         (when ~condition
           ~body
           (recur ~post-eval-exp)))))


  (defmacro for-loop
    [[arg init condition post-eval] & body]
    `(loop [~arg ~init]
       (when ~condition
         ~@body
         (recur ~post-eval))))


  (for-loop [i 0, (< i 10), (inc i)]
    (print i " "))


  (loop [i 0]
    (when (< i 10)
      (print i " ")
      (recur (inc i))))


  (def pentagonal-number*
    (memoize (fn [self-fn ^long n]
               (case n
                 0 0
                 1 1
                 (let [n-1 (dec n)]
                   (+ (self-fn self-fn n-1) (* 3 n-1) 1))))))


  (defn pentagonal-number
    [n]
    (pentagonal-number* pentagonal-number* n))

  ;
  )

;; TODO: Riddle I solved back in school to get an A. I should try to solve it in Clojure

; Peter once again dreamed of big money.
; He was just imagining winning the lottery when suddenly there was a bright flash.
; A fairy stood in front of him and said, "You have one wish."
;
; Without hesitation, Peter handed her a piece of paper and a pen.
; "How about you jot down next week's lottery numbers here for me?" he said.
; "All six lottery numbers," said the fairy in astonishment,
; "That's six wishes at once, so that's really not possible."
; Still, the fairy wrote down a number and said:
; "If you a add all six winning numbers together, you'll get this result!"
;
; Peter looked at the number and considered.
; "Oh God, there are certainly thousands of ways to get this sum with six different numbers between 1 and 49,"
; he said resignedly. "OK, I'll give you another tip," said the fairy,
; "calculate exactly how many possibilities there are that add up to this sum.
; If you then multiply the result with the number that I just wrote down for you,
; you get a very large number of a few million,
; and this number also comes out when you multiply all six lottery numbers together.”
;
; Peter was just about to thank him for the tip when the fairy disappeared again.
; Now he began to calculate, and in the next lottery draw he actually got six correct numbers.
; What six numbers were drawn?

