(ns gcse-results.core
  (:use [datomic.api :only [q db] :as d])
  (:use quil.core))

;; File downloaded from https://docs.google.com/spreadsheet/pub?key=0AoEZjwuqFS2PdEZfSVpFd0UwdExROXlQbHR4d2laUHc&output=csv
;; Line three had corrupted data. Cleaned before parsing. Also, All Subject removed
(defonce csv-data
  (->> "GCSEresults2012.csv"
     slurp
     clojure.string/split-lines
     (map #(clojure.string/split % #","))
     rest))

(def schema-tx
  [{:db/id #db/id[:db.part/db]
    :db/ident :results/subject
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id #db/id[:db.part/db]
    :db/ident :results/gender
    :db/valueType :db.type/string
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id #db/id[:db.part/db]
    :db/ident :results/year
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id #db/id[:db.part/db]
    :db/ident :results/sat
    :db/valueType :db.type/long
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}
   {:db/id #db/id[:db.part/db]
    :db/ident :results/c-or-above
    :db/valueType :db.type/bigdec
    :db/cardinality :db.cardinality/one
    :db.install/_attribute :db.part/db}])

(def data-tx
  (for [[subject gender year sat no-sat a-star a b c d e f g u] csv-data]
    {:db/id (d/tempid :db.part/user)
     :results/subject (clojure.string/trim subject)
     :results/gender (clojure.string/trim gender)
     :results/year (Long/parseLong year)
     :results/sat (Long/parseLong sat)
     :results/c-or-above (BigDecimal. c)}))

(def uri "datomic:mem://gcse-results")

;; Deleting database to avoid garbage when doing interactive development
(d/delete-database uri)

(d/create-database uri)

(def connection (d/connect uri))

@(d/transact connection schema-tx)
@(d/transact connection data-tx)

;; We can now easily query the data
(q '[:find ?subject
     :where [_ :results/subject ?subject]]
   (db connection))

;; Drawing

(def gender (atom "Male"))

(defn setup []
  (smooth)
  (frame-rate 10)
  (background 255))

(defn draw-axis [map-x map-y]
  (fill (color 0))
  (text-align :center :bottom)
  (text "C or above on GCSE 2011" (/ (width) 2) (map-y 0))
  (text-align :left :center)
  (text "C or above\non GCSE\n2012" (+ 5 (map-x 0)) (/ (height) 2))
  
  (stroke (color 0))
  (line (map-x 0) (map-y 0) (map-x 0) (map-y 100))
  (line (map-x 0) (map-y 0) (map-x 100) (map-y 0))
  
  (text-align :right :top)
  (text "0%" (map-x 0) (map-y 0))
  (text-align :center :top)
  (doseq [percent [25 50 75 100]]
    (let [x (map-x percent)
          y (map-y 0)]
      (line x y x (+ y 5))
      (text (str percent "%") x (+ y 7))))
  (text-align :right :center)
  (doseq [percent [25 50 75 100]]
    (let [x (map-x 0)
          y (map-y percent)]
      (line x y (- x 5) y)
      (text (str percent "%") (- x 7) y))))

(defn draw []
  
  (background 255)

  (let [map-x (fn [x] (map-range x 0 100 40 (- (width) 40)))
        map-y (fn [y] (map-range y 0 100 (- (height) 40) 40))
        results (q '[:find ?s ?c-2011 ?c-2012 ?sat-2012
                     :in $ ?gender
                     :where
                     [?r-2011 :results/gender ?gender]
                     [?r-2012 :results/gender ?gender]
                     [?r-2011 :results/subject ?s]
                     [?r-2012 :results/subject ?s]
                     [?r-2011 :results/year 2011]
                     [?r-2012 :results/year 2012]
                     [?r-2011 :results/c-or-above ?c-2011]
                     [?r-2012 :results/c-or-above ?c-2012]
                     [?r-2012 :results/sat ?sat-2012]]
                   (db connection) @gender)
        sats (for [[ _ _ _ sat] results] sat)
        circles (for [[s c-2011 c-2012 sat-2012] results]
                  (let [r (sqrt
                           (/
                            (map-range sat-2012 0 (apply max sats) 0.0 1.0)
                            PI))]
                    {:d (map-range (* 2 r) 0 1 0 50)
                     :x (map-x c-2011) 
                     :y (map-y c-2012)
                     :c (if (< c-2011 c-2012) (color 0 0 255 75) (color 255 0 0 75))
                     :s s, :c-2011 c-2011, :c-2012 c-2012, :sat-2012 sat-2012}))
        mouse-dist (fn [{:keys [x y]}] (dist (mouse-x) (mouse-y) x y))
        mouse-inside? (fn [{d :d :as circle}] (< (mouse-dist circle) (/ d 2)))
        closest-to-mouse (if-let [touched (seq (filter mouse-inside? circles))]
                           (apply min-key mouse-dist touched))]
    ;; axis
    (draw-axis map-x map-y)

    (stroke (color 0))
    (text-align :center :top)
    (text @gender (/ (width) 2) 0)
    
    ;; circles
    (doseq [{:keys [d x y c] :as circle} circles]
      (fill c)
      (stroke-weight 1)
      (stroke (if (= circle closest-to-mouse) (color 0) c))
      (ellipse x y d d))

    ;; closest circle
    (when-let [{:keys [d x y s c-2011 c-2012]} closest-to-mouse]
      (fill (color 0))
      (text-align :right :center)
      (text s (- x d) y)
      (text-align :left :center)
      (text (str c-2012 "% (" c-2011 "%)") (+ x d) y))))

(defsketch graph-sketch
  :title "GCSE results 2012 vs 2011"
  :setup setup
  :draw draw
  :key-pressed (fn []
                 (swap! gender {"Male" "Female"
                                "Female" "Male & Female"
                                "Male & Female" "Male"}))
  :size [600 600])
