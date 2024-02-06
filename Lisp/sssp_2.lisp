;;;; 899826 Roncoroni Daniele

;;;; sssp.lisp

;*vertices* *edges* *graphs* *visited* *distances* *previous*
;*vertices* *arcs*  *graphs* *visited* *dist*      *previous*
(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *edges* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *visited* (make-hash-table :test #'equal))
(defparameter *distances* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))

(defparameter *heaps* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
  (remhash graph-id *graphs*)
  NIL)

(defun new-vertex (graph-id vertex-id)
  (setf (gethash (list 'vertex graph-id vertex-id)
		 *vertices*)
	(list 'vertex graph-id vertex-id)))

;(defun graph-vertices (graph-id)
;  (let ((chiave ()) (valore ()))
;    (maphash (lambda (key value)
;	       (cond
;		 ((equal (second key) graph-id)
;		  (push key chiave)
;		  (push value valore))))
;	     *vertices*)
;    chiave))
(defun graph-vertices (graph-id)
  (let ((chiave '()) (valore '()))
    (maphash (lambda (key value)
	       (cond ((equal (second key) graph-id)
		      (push key chiave)
		      (push value valore))))
	     *vertices*)
    chiave))
; QUELLA PROPOSTA DA CHAT-GPT -- DA VERIFICARE!!
;(defun graph-vertices (graph-id)
;  (let ((chiave '()) (valore '()))
;    (maphash (lambda (key value)
;               (when (and (listp key) (= (length key) 2)
;			  (eql (cdr key) graph-id))
;                 (push (car key) chiave)
;                 (push (list (car key) value) valore)))
;             *vertices*)
;    (remove-duplicates chiave :test #'eql)
;    chiave))

;(edge graph-id u v weight)
(defun new-edge (graph-id vertex-id1 vertex-id2 &optional weight)
  (setf (gethash
	 (list 'edge graph-id vertex-id1 vertex-id2 weight)
	 *edges*)
	(list 'edge graph-id vertex-id1 vertex-id2 weight)))

;(defun graph-edges (graph-id)
;  (let ((chiave ()) (valore ()))
;    (maphash (lambda (key value)
;	       (cond
;		 ((equal (second key) graph-id)
;		  (push key chiave)
;		  (push value valore))))
;	     *edges*)
;    chiave))
(defun graph-edges (graph-id)
  (let ((chiave '()) (valore '()))
    (maphash (lambda (key value)
	       (cond ((equal (second key) graph-id)
		      (push key chiave)
		      (push value valore))))
	     *edges*)
    chiave))   
; QUELLA PROPOSTA DA CHAT-GPT -- DA VERIFICARE!!
;(defun graph-edges (graph-id)
;  (let ((chiave '()) (valore '()))
;    (maphash (lambda (key value)
;               (when (and (listp key) (= (length key) 2)
;			  (eql (cdr key) graph-id))
;                 (push (car key) chiave)
;                 (push (list (car key) value) valore)))
;             *edges*)
;    (remove-duplicates chiave :test #'eql)
;    chiave))

;maphash gestisce automaticamente l'iterazione attraverso chiavi e valori
;dell'hashtable
;(edge graph-id vertex-id N W)
(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((chiave '()) (valore '()))
    (maphash (lambda (key value)
	       (cond ((and (equal (second key) graph-id)
			   (equal (third key) vertex-id))
		      (push key chiave)
		      (push value valore)
		      )))
	     *edges*)
    chiave))

(defun graph-print (graph-id)
  (maphash (lambda (key value)
	     (cond
	       ((equal (second key) graph-id)
		(format t "chiave: ~S, valore: ~S~%" key value))))
	   *vertices*)
  (maphash (lambda (key value)
	     (cond
	       ((equal (second key) graph-id)
		(format t "chiave: ~S, valore: ~S~%" key value))))
	   *edges*))

(defun sssp-dist (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *distances*))

(defun sssp-visited (graph-id vertex-id)
  (setf
   (gethash (list graph-id vertex-id) *visited*)
   T))

(defun sssp-previous (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *previous*))

(defun sssp-change-dist (graph-id vertex-id new-dist)
  (setf
   (gethash (list graph-id vertex-id) *distances*)
   new-dist)
  new-dist)

(defun sssp-change-previous (graph-id vertex-id new-previous)
  (setf
   (gethash (list graph-id vertex-id) *previous*)
   new-previous)
  new-previous)

(defun sssp-dijkstra (graph-id source)
  (progn (sssp-change-dist graph-id source 0)
	 (new-heap
	  graph-id
	  (length (graph-vertices graph-id)))
	 (let
	     ((array (graph-vertex-neighbors graph-id source)))
	   (let ((lunghezza (length array)))
	     (update graph-id source array (- lunghezza 1))))
	 (sssp-visited graph-id source)
	 (sssp-dijkstra-helper graph-id)
	 NIL))

(defun sssp-dijkstra-helper (graph-id)
  (cond ((heap-not-empty graph-id)
	 (let ((vertice (second (heap-extract graph-id))))
	   (let ((array (graph-vertex-neighbors graph-id vertice)))
	     (let ((lunghezza (length array)))
	       (update graph-id vertice array (- lunghezza 1))))
	   (sssp-visited graph-id vertice)
	   (sssp-dijkstra-helper graph-id)))))

(defun update (graph-id vertice array lunghezza)
  (cond ((=> lunghezza 0)
	 (update-helper graph-id vertice array lunghezza)
	 (update graph-id vertice array (- lunghezza 1)))))

(defun update-helper (graph-id vertice-id array punto)
  (let ((vertice (fourth (nth point array)))
	(dimensione (fifth (nth point array))))
    (let ((dist-old (sssp-dist graph-id vertice))
	  (dist-new
	   (+
	    (sssp-dist graph-id vertice-id)
	    dimensione)))
      (cond ((equal dist-old NIL)
	     (progn
	       (sssp-change-dist graph-id vertice dist-new)
	       (sssp-change-previous graph-id vertice vertice-id)
	       (heap-insert graph-id dist-new vertice)))
	    ((< dist-new dist-old)
	     (progn
	       (sssp-change-dist graph-id vertice dist-new)
	       (sssp-change-previous graph-id vertice vertice-id)
	       (heap-modify-dist graph-id dist-new dist-old)))))))

(defun heap-modify-distance (heap-id key-new key-old)
  (let ((list (the-heap heap-id))
	(element (find
		  (the-heap heap-id)
		  (- (heap-size heap-id) 1)
		  key-old)))
    (cond ((equal element NIL) NIL)
	  (T
	   (let ((value (second (aref list element))))
	     (setf
	      (aref list element)
	      (list key-new value))
	     (heap-shift list
			 (- (heap-size heap-id) 1)
			 element))))))

;find-element is find
(defun find (array size chiave)
  (cond ((< size 0) NIL)
	(T
	 (let ((key (first (aref array size))))
	   (cond ((= key chiave) size)
		 (T (find array (- size 1) chiave)))))))

;sssp-shortest-path-aux is sssp-shortest-path-helper
(defun sssp-shortest-path (graph-id source vertex-id)
  (sssp-shortest-path-helper graph-id source vertex-id ()))

(defun sssp-shortest-path-helper (graph-id source vertex-id elementi)
  (let ((previous (sssp-previous graph-id vertex-id)))
    (cond ((equal vertex-id source) elementi)
	  (T (push
	      (first (all-edges graph-id previous vertex-id))
	      elementi)
	     (sssp-shortes-path-helper
	      graph-id
	      source
	      previous
	      elementi)))))

;graph-arcs-sssp is all-edges
(defun all-edges (graph-id vertex-id1 vertex-id2)
  (let ((chiave '()) (valore '()))
    (maphash (lambda (key value)
	       (cond ((and (equal (second key) graph-id)
			   (equal (third key) vertex-id1)
			   (equal (fourth key) vertex-id2))
		      (push key chiave)
		      (push value valore))))
	     *edges*)
    chiave))


					;ATTENZIONE!
;heap-actual-heap is the-heap
