#!/usr/bin/env bb

(require '[clojure.pprint :as pprint])
(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.java.io :as io])
(import java.security.SecureRandom
        (java.util Base64 Base64$Encoder))

;; From buddy.core.nonce
(defn random-bytes
  "Generate a byte array of specified length with random
  bytes taken from secure random number generator.
  This method should be used to generate a random
  iv/salt or arbitrary length."
  ([^long numbytes]
   (random-bytes numbytes (SecureRandom.)))
  ([^long numbytes ^SecureRandom sr]
   (let [buffer (byte-array numbytes)]
     (.nextBytes sr buffer)
     buffer)))

;; From buddy.core.codecs
(defn bytes->b64u
  "Encode data to base64 byte array (using url-safe variant)."
  {:added "1.8.0"}
  [^bytes data]
  (let [^Base64$Encoder encoder (-> (Base64/getUrlEncoder)
                                    (.withoutPadding))]
    (.encode encoder data)))

;; From buddy.core.codecs
(defn bytes->str
  "Convert byte array to String."
  ([^bytes data]
   (bytes->str data "UTF-8"))
  ([^bytes data, ^String encoding]
   (String. data encoding)))

(defn base64-str
  "Retrieves a secure BASE64 string of given length.
  Gets more securely generated 8 bit bytes than the provided number of 6 bit BASE64 characters would consume."
  [length]
  (when (and (number? length) (pos? length))
    (let [num-random-bytes (-> length (* 6) (/ 8) Math/ceil long)]
      (-> (random-bytes num-random-bytes) bytes->b64u
          bytes->str (subs 0 length)))))

(defn base64-just-alpha-num-str
  "Retrieves a secure alphanumeric [A-Za-z0-9] string of given length.
  Gets 10% longer BASE64 string because it is likely special characters will occur, removes these [-_] characters,
  checks if the length is sufficient. If not, retrieves another string ~ one tenth as long and repreats the process."
  [length]
  (when (and (number? length) (pos? length))
    (letfn [(get-alpha-num [length]
              (str/replace (base64-str (-> length (* 1.1) Math/ceil long)) #"[-_]" ""))]
      (loop [s (get-alpha-num length)]
        (if (>= (count s) length)
          (subs s 0 length)
          (recur (str (get-alpha-num (-> length (/ 10) Math/ceil long)) s)))))))

;; https://github.com/clojure/tools.cli
;; https://clojure.github.io/tools.cli/index.html#clojure.tools.cli/parse-opts
;; https://book.babashka.org/#_input_and_output_flags
(def cli-options
  ;; An option with a required argument
  [["-n" "--number NUMBER" "Number of characters to generate"
    :validate [#(number? %) "Please give a number."]
    :parse-fn #(Long/parseLong %)]
   ["-h" "--help"]])

(let [{:keys [options] :as opts-map} (parse-opts *command-line-args* cli-options)
      {:keys [number help]} options]
  (if (or help (not number))
    (pprint/pprint opts-map)
    (println (base64-just-alpha-num-str number))))
