#!/usr/bin/env bb

(require '[clojure.pprint :as pprint])
(require '[clojure.tools.cli :refer [parse-opts]])

(def base32 "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")
(def base64 "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789-_")
#?(:clj (def ^:private base64-vec (vec base64)))

(def ^:private base64->index
  (->> base64 (map-indexed (fn [index ch]
                             [ch index]))
       (into {})))

(def ^:private base32->index
  (->> base32 (map-indexed (fn [index ch]
                             [ch index]))
       (into {})))

(def hex-regex
  "Regex matching to a UUID in hexadecimal form, e.g., 0713d2bf-8e70-41e2-8b81-c8ca3ef4a8cd."
  #"[0-9A-Fa-f]{8}-[0-9A-Fa-f]{4}-[0-9A-Fa-f]{4}-[89ABab][0-9A-Fa-f]{3}-[0-9A-Fa-f]{12}")

(def base64-regex
  "Regex matching to a UUID in base64 form, e.g., AHE9K_jnBB4ouByMo-9KjN."
  #"[-0-9A-Za-z_]{22}")

(def base32-regex
  "Regex matching to a UUID in base32 form, e.g., AHCPJL7DTQIHRIXAOIZI7PJKGN. Accept lower and upper case, it doesn't
  matter here and is handled in the conversion process."
  #"[A-Za-z2-7]{26}")

(defn uuid-str?
  "Check if input is a string in the canonical UUID form or BASE64 or BASE32 coded UUID"
  [s]
  (and (string? s) (or (re-matches hex-regex s) (re-matches base64-regex s) (re-matches base32-regex s))))

(defn- dec-list->hex-string
  "Converts a list of decimal (regular base10) numbers to a hex string with appropriate padding length represented by
  num-chars. If coll length is 11, it was base64 encoded (11 groups of 3 hex chars). Otherwise, it was base32 encoded
  (7 groups of 5 hex chars)."
  [coll]
  (let [num-chars (if (= 11 (count coll)) 3 5)]
    (->> coll (map #?(:clj  #(format "%x" %)
                      :cljs #(.toString % 16)))
         (map (fn [coll]
                (let [num-char (count coll)
                      padding (apply str (repeat (- num-chars num-char) "0"))]
                  (str padding coll))))
         (apply str))))

(defn- str->uuid
  "Transforms a UUID string into a UUID reader literal canonical form by removing left-padding of num-chars.
  Previosuly base64 encoded string has a single left-pad character, base32 has 3."
  [uuid-str]
  (let [hex (subs uuid-str (if (= 33 (count uuid-str)) 1 3))
        with-sep (str (subs hex 0 8) "-" (subs hex 8 12) "-" (subs hex 12 16)
                      "-" (subs hex 16 20) "-" (subs hex 20))]
    #?(:clj  (parse-uuid with-sep)
       :cljs (uuid with-sep))))

(defn- parse-base64
  "Converts a UUID as a BASE64 string to a UUID."
  [base64-str]
  (->> base64-str (partition 2 2)
       (map (partial reduce (fn [acc ch]
                              (+ (* acc 64) (base64->index ch))) 0))
       dec-list->hex-string
       str->uuid))

(defn- parse-base32
  "Converts a UUID as a BASE32 string to a UUID. For computation, we have to add 10 bits in front to make the total
  number of bits divisible by 20 (or 4 5-bit characters). See ->base32 for more details."
  [base32-str]
  (->> base32-str (str/upper-case)
       (str "AA")
       (partition 4 4)
       (map (partial reduce (fn [acc ch]
                              (+ (* acc 32) (base32->index ch))) 0))
       dec-list->hex-string
       str->uuid))

(defn parse
  "Parses UUID from string."
  [s]
  (cond (uuid? s) s
        (not (string? s)) nil
        (re-matches hex-regex s) #?(:clj  (parse-uuid s)
                                    :cljs (uuid s))
        (re-matches base64-regex s) (parse-base64 s)
        (re-matches base32-regex s) (parse-base32 s)))

;; https://github.com/clojure/tools.cli
;; https://clojure.github.io/tools.cli/index.html#clojure.tools.cli/parse-opts
;; https://book.babashka.org/#_input_and_output_flags
(def cli-options
  ;; An option with a required argument
  [["-h" "--help"]])

(let [{:keys [options arguments] :as opts-map} (parse-opts *command-line-args* cli-options)
      {:keys [help]} options
      [uuid-candidate] arguments]
  (if help
    (pprint/pprint opts-map)
    (println (parse uuid-candidate))))
