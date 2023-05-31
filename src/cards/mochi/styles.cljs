(ns cards.mochi.styles
  (:require
    [clojure.string           :as      str]
    ["@emotion/is-prop-valid" :default isPropValid]
    [reagent.core             :as      r]
    [medley.core              :refer   [remove-keys filter-keys map-vals]])

  (:require-macros [cards.mochi.styles]))


;; Need to define this here to prevent the macro from raising a warning.
(def is-prop-valid? isPropValid)

(defn -camel-case [k]
  (if (keyword? k)
    (let [[first-word & words] (str/split (name k) #"-")]
      (if (or (empty? words)
              (= "aria" first-word)
              (= "data" first-word))
        k
        (-> (map str/capitalize words)
            (conj first-word)
            str/join
            keyword)))
    k))


;; TODO: Same as kw->css-custom-prop in the clj namespace.
;; Merge these namespaces into one .cljc
(defn color [kw]
  (str "var("
       (str "--mochi--"
         (apply str (interpose "--"
                      [(-> kw (namespace) (or "")
                           (str/replace "." "-")
                           (str/replace ":" "-"))
                       (str/replace (name kw) ":" "-")])))
       ")"))


(defn styled [{:keys [el class vars]}]
  (fn []
    (let [self     (r/current-component)
          props    (r/props self)
          children (r/children self)

          ;; Get valid HTML props.
          html-props (filter-keys #(-> % -camel-case name is-prop-valid?) props)
          css-vars   (map-vals #(if (fn? %) (% props) %) vars)]

      (into [(or (:el props) el) 
             (merge html-props
                    {:class (flatten (conj [class] (:class props)))
                     :style (merge css-vars (js->clj (:style html-props)))})]
                     ;; Converting to js->clj because dnd converts props to js?
            children))))

