(ns cards.mochi.styles
  (:require
    [cljs.env]
    [clojure.java.io :as io]
    [clojure.string  :as str]
    [cljs.core       :as cljs.core]
    [cljs.analyzer   :as cljs]
    [cljs.analyzer.api :as cljs.api]))

(declare add-vector-rules)
(declare add-map-rules)
(declare add-value)

(def styles-dir "example/styles/compiled")
(def styles-file "example/styles/app.css")

(def rules (clojure.core/atom {}))

(defn color [kw]
  (str "var("
       (str "--mochi--"
         (apply str (interpose "--"
                      [(-> kw (namespace) (or "")
                           (str/replace "." "-")
                           (str/replace ":" "-"))
                       (str/replace (name kw) ":" "-")])))
       ")"))


(defn sym->css-class
  [ns sym]
  (str
    (clojure.string/replace (str ns) "." "_")
    "_"
    (clojure.string/replace (name sym) ":" "--")))

; (s/def ::styles (s/or :vector vector? :map ::styles-map))
; (s/def ::style-maps (s/map-of ::prop ::val))
; (s/def ::prop (s/or :keyword keyword? :string string?))
; (s/def ::prop (s/or number? string? keyword? ::styles)) 

(defn clj->css [class styles env]
  (if (vector? styles)
    (add-vector-rules {} class styles env)
    (add-map-rules    {} class styles env)))


(defn add-key [k env]
  ;; We use cljs.analyzer to get the fully qualified symbol, then
  ;; use resolve to try to resolve it in clojure while the macros is running.
  (cond (symbol? k) @(resolve (:name (cljs/resolve-var @env k)))
        :else       (name k)))
    


(defn add-vector-rules [acc k v env]
  (reduce (fn [acc sub-m]
            (add-map-rules acc k sub-m env))
          acc v))


(defn random-id [length]
  (apply str (repeatedly length #(rand-nth "abcdefghijklmnopqrstuvwxyz0123456789"))))


(defn next-fn-name [acc]
  ; (let [var-name (str "--" (random-id 4))]
  ;   (if (get-in acc [:vars var-name])
  ;     (next-fn-name acc)
  ;     var-name))
  (str "--fn" (-> acc :vars keys count)))


(defn resolve-cljs-sym [sym env]
  (let [var-data (cljs/resolve-var env sym)]
    (require `(~(:ns var-data)))
    ; (println (cljs.core/resolve (:name var-data)))
    ; (println (ns-publics (:ns var-data)))
    ; (println (:ns var-data))
    ; (println @(ns-resolve (find-ns (:ns var-data)) sym))
    (let [var (resolve (:name var-data))]
      (when-not (nil? var)
        (var-get var)))))


(defn add-fn-property [acc k v env]
  ; (when (symbol? v)
  ;   (require `(~(:ns (cljs/resolve-var env v))))
  ;   (let [var (resolve (:name (cljs/resolve-var env v)))]
  ;     (println var)
  ;     (when-not (nil? var)
  ;       (println (fn? (var-get var)))
  ;       (println (var-get var)))))

    ; (println (resolve (:name (cljs/resolve-var @env v))))
    ; (println (cljs/resolve-var env v)))

  (let [custom-prop-name (next-fn-name acc)]
        ; resolved-val (and (symbol? v) (resolve-cljs-sym v env))
        ; _ (when-not (= false resolved-val)
        ;     (println v resolved-val))]

    (if false;resolved-val
      :noop;(add-value acc k resolved-val env)
      (-> acc (update :str str (add-key k env) ":var(" custom-prop-name ");")
              (update :vars assoc custom-prop-name v)))))


(defn add-scalar-property [acc k v env]
  (update acc :str str (add-key k env) ":" v ";"))

(defn add-keyword-property [acc k v env]
  (update acc :str str (add-key k env) ":" (name v) ";"))


;; TODO: Duplicated kw->css-custom-prop in memo.views.colors ns
;; TODO: Find a better solution than 'get-color'
;;       This function isn't really needed anymore, we just need to transform
;;       the keyword into the appropriate CSS custom property.
;;       -- Possibly we can just check if val is a keyword, and if it is look it up
;;          in the colors var, otherwise wrap it in (name)
;; TODO: Write documentation
;; TODO: More performant design then is-prop-valid?
;; TODO: use with-bindings to pass around &env?
;; TODO: Allow shortcut to pass number -> numberpx (see emotion, likely just a big list of rules to check before allowing conversion. e.g. line-height would not be on the whitelist.)
;; TODO: Composable styles:
;; 1. You can include symbols from other defstyled calls. e.g.
;; (defstyled foo :div
;;   [memo.views.app/bar
;;    {:font-size "20px"}])
;; This will take the passed in symbole and xform it into a class, which will be
;; appended tot he beginning of the class name.
;; E.g. `.memo__views__app__bar.memo__views__login__foo`
;; And in the class we will include both class names.
;; The specificity of including both classes in the selector will override ordering if
;; e.g. .bar is declare before .foo and .foo contains some rule(s) that overrides .bar's

(defn kw->css-custom-prop [kw]
  (str "--mochi--"
    (apply str (interpose "--"
                 [(-> kw (namespace) (or "")
                      (str/replace "." "-")
                      (str/replace ":" "-"))
                  (str/replace (name kw) ":" "-")]))))


(defn add-color-property [acc k v env]
  ; (when (= k :foobar) (println v))
  ; (println v)
  (if (= (:name (cljs/resolve-var env (first v)))
         'cards.mochi.styles/color)
    (update acc :str str (name k) ":var(" (kw->css-custom-prop (last v)) ");")
    ;; (= (first v) 'fn*)
    (add-fn-property acc k v env)))


(defn add-symbol-property [acc k v env]
  (let [resolved-v (@rules (:name (cljs/resolve-var env v)))]
    (when (nil? resolved-v)
      (throw (Exception. (str "Unknown symbol: '" v))))
    (update acc :str str (add-key k env) ":" (name resolved-v) ";")))

  ; (println v)
  ; (let [the-ns (find-ns (:ns (cljs.api/resolve env v)))]
  ;   (require '[example.app])
  ;   (println (cljs.api/resolve env v))
  ;   (println the-ns)
  ;   (println (ns-publics the-ns))
  ;   (eval (:name (cljs.api/resolve env v))))
  ; ; (println (cljs.core/resolve (:name (cljs/resolve-var env v))))
  ; ; (println (resolve-cljs-sym v env))
  ; ; "foo")
  ; @(resolve (:name (cljs/resolve-var env v))))


(defn add-value [acc k v env]
  ; (println v)
  (cond (map? v)     (add-map-rules acc k v env)
        (vector? v)  (add-vector-rules acc k v env)
        (fn? v)      (add-fn-property acc k v env)
        (list? v)    (add-color-property acc k v env) ;; function
        (symbol? v)  (add-symbol-property acc k v env)
        (keyword? v) (add-keyword-property acc k v env)
        :else        (add-scalar-property acc k v env)))


(defn add-map-rules [acc k m env]
  (let [m' (if (symbol? m)
             (let [symbol-val (@rules (:name (cljs/resolve-var env m)))]
               (when (nil? symbol-val)
                 (throw (Exception. (str "Unknown symbol: '" m))))
               symbol-val)
             m)]
    (as-> acc acc'
      (update acc' :str #(str % (add-key k env) "{"))
      (reduce
        (fn [acc [k v]]
          (add-value acc k v env))
        acc' m')
      (update acc' :str str "}"))))

;; -- Constants ---------------------------------------------------------------

(defn extract-to-css-file? []
  (and cljs.env/*compiler*
       (get-in @cljs.env/*compiler*
               [:options :closure-defines 'cards.mochi.styles/compile-css?])))


(defmacro defstyles
  [sym args styles]

  (let [class-name (sym->css-class *ns* sym)
        gen-css    (clj->css (str "." class-name) styles &env)
        vars       (:vars gen-css)
        compiled-css (:out (clojure.java.shell/sh "node" "stylis.js" (:str gen-css)))]

    (when (extract-to-css-file?)
      (println (str "cards.mochi.styles - Compiling " styles-dir "/" class-name ".css"))
      (spit (str styles-dir "/" class-name ".css") compiled-css))
    
    ;; TODO: How to make this work with props?
    `(defn ~sym [] ~class-name)))

(defmacro defvalue
  [sym value]

  (swap! rules #(assoc % (:name (cljs/resolve-var &env sym)) value))

  `(def ~sym {}))

(defmacro defstyled
  [sym el styles]

  (let [class-name (sym->css-class *ns* sym)
        gen-css    (clj->css (str "." class-name) styles &env)
        vars       (:vars gen-css)
        compiled-css (:out (clojure.java.shell/sh "node" "stylis.js" (:str gen-css)))]

    (when (extract-to-css-file?)
      (println (str "cards.mochi.styles - Compiling " styles-dir "/" class-name ".css"))
      (spit (str styles-dir "/" class-name ".css") compiled-css))

    `(def ~sym (styled {:el ~el :class ~class-name :vars ~vars}))))


(defn compile!
  {:shadow.build/stage :flush}
  [build-state]

  (when (-> build-state :shadow.build/config :closure-defines
            (get 'cards.mochi.styles/compile-css?))
    (println "cards.mochi.styles - Bundling css.")
    (let [css-files (.list (io/file styles-dir))]
      (spit styles-file "")
      (doseq [css-file css-files]
        (spit styles-file
          (slurp (str styles-dir "/" css-file))
          :append true))))
  ; (clojure.java.shell/sh "node" "postcss.js")
  build-state)

