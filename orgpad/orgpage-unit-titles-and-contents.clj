#!/usr/bin/env bb

(require '[clojure.pprint :as pprint])
(require '[clojure.tools.cli :refer [parse-opts]])

;; https://github.com/clojure/tools.cli
;; https://clojure.github.io/tools.cli/index.html#clojure.tools.cli/parse-opts
;; https://book.babashka.org/#_input_and_output_flags
(def cli-options
  ;; An option with a required argument
  [["-i" "--input INPUT" "Input EDN file or URL"
    :validate [#(string? %) "An input EDN file or URL must be given."]]
   ["-o" "--output OUTPUT" "OUTPUT simplified EDN file."
    :validate [#(string? %) "An output EDN file must be given."]]
   ["-h" "--help"]])

(defn blank->nil
  "Converts a blank string into nil."
  [s]
  (when-not (and (string? s) (str/blank? s))
    s))

(defn seq->map
  "Transforms a sequence into a map whose values are the elements of the sequences with keys
  produced by calling a suplied function on each element. For instance, a keyword can be used
  as a function."
  [elements key-fn]
  (persistent!
    (reduce (fn [m element]
              (assoc! m (key-fn element) element))
            (transient {}) elements)))

(defn seq->>map
  "Works the same as seq->map, but with the opposite order of parameters."
  [key-fn elements]
  (seq->map elements key-fn))

(defn page-id->content-map
  "Builds a map of page-ids to their respective contents."
  [page-ids pages]
  (->> page-ids
       (map (fn [page-id]
              (let [{:unit/keys [content]} (get pages page-id)]
                [page-id content])))
       (into {})))

(defn get-unit-id-title-content
  "From a list of units, builds a list of (book) units with their ids, titles and the child page contents as a map."
  [units]
  (let [book-units (->> units (filter (fn [{:unit/keys [type]}]
                                        (= type :unit/book))))
        pages (->> units (filter (fn [{:unit/keys [type]}]
                                   (= type :unit/page)))
                   (seq->>map :unit/id))]
    (->> book-units
         (map (fn [{:unit/keys [id title child-unit-ids]}]
                (when (not-empty child-unit-ids)
                  #:unit{:id    id
                         :title title
                         :pages (page-id->content-map child-unit-ids pages)}))))))

(let [{:keys [options] :as opts-map} (parse-opts *command-line-args* cli-options)
      {:keys [input output help]} options
      {:orgpage/keys [units] :as orgpage-edn} (when (not help)
                                                (edn/read-string (slurp (or input *in*))))

      processed-units (when orgpage-edn
                        (vec (get-unit-id-title-content units)))]
  (if help
    (pprint/pprint opts-map)
    (if output
      (spit output processed-units)
      (println processed-units))))
