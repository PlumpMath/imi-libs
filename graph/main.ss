#!r6rs

(library (imi graph)
  (export make-graph
          graph?
          graph-nodes
          graph-edges
          graph-edge-connects

          make-node
          node?
          node-id
          node-edges

          make-edge
          edge?
          edge-weight)
  (import (rnrs)
          (imi proc predicate check)
          (imi proc predicate standard))

  (define-record-type graph
    (fields
      nodes
      edges)
    (protocol
      (lambda (new)
        (lambda (nodes edges)
          (verify-args
            'make-graph
            nodes "nodes has to be a list of nodes" (listof/p node?)
            edges "edges has to be a list of edges" (listof/p edge?))
          (new nodes edges)))))

  (define-record-type node
    (fields
      id
      edges)
    (protocol
      (lambda (new)
        (lambda (id edges)
          (verify-args
            'make-node
            edges "edges has to be a list of edges" (listof/p edge?))
          (new id edges)))))

  (define-record-type edge
    (fields
      weight
      node0
      node1)
    (protocol
      (lambda (new)
        (lambda (weight node0 node1)
          (verify-args
            'make-edge
            node0 "node0 has to be an integer identifying a node" integer?
            node1 "node1 has to be an integer identifying a node" integer?)
          (new weight node0 node1)))))


  ;;; gets the node in graph `g` which edge `e`
  ;;;  connects to node `n`
  ;;;
  ;;; g - graph?
  ;;; e - edge?
  ;;; n - node?
  ;;;  -> node?
  (define (graph-edge-connects g e n)
    (let ([id (if (= (edge-node0 e)
                     (node-id n))
                (edge-node1 e)
                (edge-node0 e))])
      (find (lambda (x)
              (= (node-id x) id))
            (graph-nodes g))))

  )
