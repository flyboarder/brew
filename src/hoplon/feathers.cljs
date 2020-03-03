(ns hoplon.feathers
  (:require [goog.object :as obj]
            [javelin.core :as j]
            [hoplon.debug :as dbg]
            [feathers.client.services :as fs]))

(def ^:private debug (dbg/debug "hoplon:feathers"))

(defn feathers-created [service fcell]
  (fs/created service
    (fn [data]
      (debug "feathers-created called with data" data)
      (-> (fs/get service (obj/get data "_id"))
        (.then #(js->clj % :keywordize-keys true))
        (.then #(swap! fcell conj %))))))

(defn- remap [coll index]
  (zipmap (map index coll) coll))

(defn feathers-updated [fcell]
  (fn [data]
    (debug "feathers-updated called with data" data)
    (let [data (js->clj data :keywordize-keys true)]
      (swap! fcell
        (fn [fdata]
          (let [fdata (remap fdata :_id)]
            (vals (assoc fdata (:_id data) data))))))))

(defn feathers-patched [fcell]
  (fn [data]
    (debug "feathers-patched called with data" data)
    (let [data (js->clj data :keywordize-keys true)]
      (swap! fcell
        (fn [fdata]
          (let [fdata (remap fdata :_id)]
            (vals (assoc fdata (:_id data) (merge (get fdata (:_id data)) data)))))))))

(defn feathers-removed [fcell]
  (fn [data]
    (debug "feathers-removed called with data" data)
    (let [data (js->clj data :keywordize-keys true)]
      (swap! fcell
        (fn [fdata]
          (remove #(= (:_id %) (:_id data)) fdata))))))

(defn- feathers-error! [error]
  (throw error))

(defn feathers-cell [service & [params]]
  (let [fcell  (j/cell nil)
        fcell! (partial reset! fcell)]
    (j/with-let [_ (j/cell= fcell fcell!)]
      (-> service
        (fs/find (clj->js params))
        (.then #(js->clj % :keywordize-keys true))
        (.then fcell!)
        (.catch feathers-error!))
      (feathers-created service fcell)
      (fs/updated service (feathers-updated fcell))
      (fs/patched service (feathers-patched fcell))
      (fs/removed service (feathers-removed fcell)))))
