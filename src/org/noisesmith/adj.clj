(ns org.noisesmith.adj)

(defprotocol Adj
  (nodes [this])
  (edges [this])
  (node [this which])
  (edges-from [this which])
  (insert [this n props])
  (connect [this from to])
  (disconnect [this from other]))

(defrecord PersistentAdj
    [all-nodes edges empty-edges]
  Adj
  (nodes [this] all-nodes)
  (edges [this] edges)
  (node [this which] (get all-nodes which))
  (edges-from [this which] (get edges which))
  (insert [this n props]
    (PersistentAdj. (assoc all-nodes n props) edges empty-edges))
  (connect [this from to]
    (assert (contains? all-nodes from))
    (assert (contains? all-nodes to))
    (PersistentAdj.
     all-nodes
     (update-in edges [from] (fnil conj empty-edges) to)
     empty-edges))
  (disconnect [this from other]
    (PersistentAdj.
     all-nodes
     (update-in edges [from] dissoc other)
     empty-edges)))

(defn rand-node
  [adj]
  (rand-nth (keys (nodes adj))))

(defn ->list
  ([adj]
     (->list adj (rand-node adj)))
  ([adj root]
     (->list adj identity root))
  ([adj f root]
     (when (some? root)
       (lazy-seq
        (cons {:node root
               :val (f (node adj root))}
              (->list adj f (rand-nth (seq (edges-from adj root)))))))))

(defn ->tree
  ([adj]
     (->tree adj (rand-node adj)))
  ([adj root]
     (->tree adj root #{root}))
  ([adj root visited]
     (->tree adj identity root visited))
  ([adj f root visited]
     (reduce
      (fn visit-child [[node-acc visited] child]
        (if (contains? visited child)
          [node-acc visited]
          (let [visited' (conj visited child)
                [node-acc' visited''] (->tree adj child visited')]
            [(assoc-in node-acc [:children child] node-acc')
             visited''])))
      [{:node root :val (f (node adj root))} visited]
      (edges-from adj root))))
