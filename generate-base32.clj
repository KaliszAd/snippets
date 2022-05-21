#!/usr/bin/env bb

(require '[clojure.pprint :as pprint])
(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.java.io :as io])
(import java.security.SecureRandom)

(def base32 "ABCDEFGHIJKLMNOPQRSTUVWXYZ234567")

(defn random-int
  "Generate a random int between 0 inclusive and n exclusive
  using secure random number generator."
  [^int n]
  (.nextInt (SecureRandom.) n))

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
    (println (str/join (repeatedly number #(get base32 (random-int 32)))))))
