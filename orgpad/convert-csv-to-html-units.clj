#!/usr/bin/env bb

(require '[clojure.pprint :as pprint])
(require '[clojure.tools.cli :refer [parse-opts]])
(require '[clojure.data.csv :as csv])
(require '[hiccup2.core :as hiccup])
(require '[clojure.java.io :as io])

;; https://github.com/clojure/tools.cli
;; https://clojure.github.io/tools.cli/index.html#clojure.tools.cli/parse-opts
;; https://book.babashka.org/#_input_and_output_flags
(def cli-options
  ;; An option with a required argument
  [["-i" "--input INPUT" "Input CSV file"
    :validate [#(string? %) "An input CSV file must be given."]]
   ["-o" "--output OUTPUT" "OUTPUT html file."
    :validate [#(string? %) "An output HTML file must be given."]]
   ["-h" "--help"]])

(defn blank->nil
  "Converts a blank string into nil."
  [s]
  (when-not (and (string? s) (str/blank? s))
    s))

(defn csv->hiccup-unit-titles
  ""
  [input]
  (let [[first-unit first-unit-content & hiccup] (->> input (map str/join)
                                                      (map blank->nil)
                                                      (remove nil?)
                                                      (map #(vector [:h2 [:span {:class "mw-headline"} %]]
                                                                    [:div]))
                                                      (apply concat))]
    [:html
     [:meta {:http-equiv "content-type"
             :content    "text/html; charset=utf-8"}]
     (apply concat [[first-unit (conj first-unit-content "a")] hiccup])]))

(let [{:keys [options] :as opts-map} (parse-opts *command-line-args* cli-options)
      {:keys [input output help]} options
      csv (when (not help)
            (if input (csv/read-csv (slurp input)) (line-seq (io/reader *in*))))
      html (when csv (str (hiccup/html (csv->hiccup-unit-titles csv))))]
  (if help
    (pprint/pprint opts-map)
    (if output
      (spit output html)
      (println html))))
