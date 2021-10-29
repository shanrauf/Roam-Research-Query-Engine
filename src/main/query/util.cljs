(ns query.util
  (:require goog.crypt goog.crypt.Md5))

(defonce branch-clauses ["and" "or" "not" "between"])
(defn branch? [branch]
  (some #(= branch %) branch-clauses))


(defn string->md5-hex
  "String goes in, md5 hex string comes out."
  [s]
  {:pre [(string? s)]
   :post [(string? %)]}
  (goog.crypt/byteArrayToHex
   (let [md5 (goog.crypt.Md5.)]
     (.update md5 (goog.crypt/stringToUtf8ByteArray s))
     (.digest md5))))