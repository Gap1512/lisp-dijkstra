;;;; dijkstra.lisp

(in-package #:dijkstra)

(defvar *r* nil)

(defun dijkstra-short-path (begin end)
  (setf *r* nil)
  (paths begin end 0 `(,begin))
  (car (sort *r* #'< :key #'cadr)))

(defun linked-node (edge node)
  (with-accessors ((vertices vertices)) edge
    (car (remove-if #'(lambda (x) (eq x node)) vertices))))

(defun paths (begin end cost result)
  (if (eql begin end)
      (push `(,(reverse result) ,cost) *r*)
      (loop for edge in (nodes begin)
	    for linked = (linked-node edge begin) do
	   (unless (member linked result)
	     (paths linked end (+ (slot-value edge 'weight) cost) (cons linked result))))))

(defun nodes (node &key (graph *graph*))
  (with-accessors ((edges edges)) graph
    (sort (remove-if-not #'(lambda (n) (find node (slot-value n 'vertices))) edges)
	  #'< :key #'(lambda (x) (slot-value x 'weight)))))

(defparameter *node-size* 15)
(defparameter *node-color* "black")
(defparameter *path-color* "red")
(defparameter *entry-width* 25)
(defparameter *graph* nil)
(defparameter *begin* nil)
(defparameter *end* nil)

(defclass node ()
  ((data
    :initarg :data
    :initform (error "Must supply a value")
    :accessor data)
   (pos 
    :initarg :pos
    :initform (list 0 0)
    :accessor pos)
   (color
    :initarg :color
    :initform *node-color*
    :accessor color)))

(defclass edge ()
  ((vertices
    :initarg :vertices
    :initform (error "Must entry a list with the linked nodes")
    :accessor vertices)
   (weight
    :initarg :weight
    :initform (error "Must entry a value")
    :accessor weight)
   (color
    :initarg :color
    :initform *node-color*
    :accessor color)))

(defclass graph ()
  ((vertices
    :initarg :vertices
    :initform nil
    :accessor vertices)
   (edges
    :initarg :edges
    :initform nil
    :accessor edges)))

(defmethod ms:class-persistent-slots ((self node))
  '(data pos color))

(defmethod ms:class-persistent-slots ((self edge))
  '(vertices weight color))

(defmethod ms:class-persistent-slots ((self graph))
  '(vertices edges))

(defun create-node (canvas node)
  (with-accessors ((data data)
		   (pos pos)) node
    (let* ((x (car pos))
	   (y (cadr pos))
	   (x0 (- x *node-size*))
	   (y0 (- y *node-size*))
	   (x1 (+ x *node-size*))
	   (y1 (+ y *node-size*))
	   (rectangle (create-rectangle canvas x0 y0 x1 y1))
	   (text (create-text canvas x y data)))
      (itemconfigure canvas text :fill (slot-value node 'color))
      (itemconfigure canvas text :anchor "center")
      (itemconfigure canvas rectangle :fill "white")
      (itemconfigure canvas rectangle :outline (slot-value node 'color)))))

(defun create-edge (canvas edge)
  (with-accessors ((vertices vertices)
		   (weight weight)) edge
    (let ((node1 (car vertices))
	  (node2 (cadr vertices)))
      (with-accessors ((pos1 pos)) node1
	(with-accessors ((pos2 pos)) node2
	  (let* ((x0 (car pos1))
		 (y0 (cadr pos1))
		 (x1 (car pos2))
		 (y1 (cadr pos2))
		 (line (create-line* canvas x0 y0 x1 y1))
		 (text-position (text-location x0 y0 x1 y1))
		 (text (create-text canvas (car text-position) (cdr text-position) weight)))
	    (when (eq node1 node2)
	      (itemconfigure canvas
			     (create-oval canvas
					  (- x0 (* 3 *node-size*)) (- y0 (* 3 *node-size*))
					  x0 y0)
			     :outline (slot-value edge 'color)))
	    (itemconfigure canvas line :fill (slot-value edge 'color))
	    (itemconfigure canvas text :fill (slot-value edge 'color))
	    (itemconfigure canvas text :anchor "se")))))))

(defun create-graph (canvas graph)
  (with-accessors ((vertices vertices)
		   (edges edges)) graph
     (loop for edge in edges
       do (create-edge canvas edge))
    (loop for vertice in (reverse vertices)
       do (create-node canvas vertice))))

(defun text-location (x0 y0 x1 y1)
  (if (and (= x0 x1) (= y0 y1))
      (cons (- x0 *node-size*) (- y0 *node-size*))
      (cons (/ (+ x0 x1) 2) (/ (+ y0 y1) 2))))

(defun configure-screen (screen)
  (wm-title screen "Dijkstra"))

(defun configure-frame (frame)
  (pack frame :fill :both :expand t))

(defun configure-scrolled-canvas (scanvas)
  (pack scanvas :side :bottom :expand :both :fill :both))

(defun configure-canvas (canvas)
  (scrollregion canvas 0 0 1920 1080)
  (configure canvas :background "white")
  (configure canvas :borderwidth 1)
  (configure canvas :relief :solid))

(defun configure-buttons (&rest buttons)
  (loop for button in buttons do
    (pack button :side :left)
       (configure button :cursor "hand2")))

(defun configure-label (label)
  (pack label :side :bottom)
  (setf (text label) "Cost: 0 Path: ..."))

(defun reset-graph (canvas label)
  (clear canvas)
  (setf *graph* (make-instance 'graph)
	*begin* nil
	*end* nil)
  (setf (text label) "Cost: 0 Path: ..."))

(defun render ()
  (with-ltk ()
    (let* ((f (make-instance 'frame))
	   (sc (make-instance 'scrolled-canvas :master f))
	   (c (canvas sc))
	   (l (make-instance 'label :master f))
	   (node-button (make-instance 'button
				       :master f
				       :text "Add Node"
				       :command #'(lambda () (node-add-mode c))))
	   (edge-button (make-instance 'button
				       :master f
				       :text "Add Edge"
				       :command #'(lambda () (edge-add-mode c l))))
	   (delete-node-button (make-instance 'button
					      :master f
					      :text "Delete Node"
					      :command #'(lambda () (delete-node-mode c l))))
	   (start-button (make-instance 'button
					:master f
					:text "Start"
					:command #'(lambda () (set-begin-end-mode c :mode :begin :label l))))
	   (end-button (make-instance 'button
				      :master f
				      :text "End"
				      :command #'(lambda () (set-begin-end-mode c :mode :end :label l))))
	   (clear-button (make-instance 'button
				      :master f
				      :text "Clear"
				      :command #'(lambda () (reset-graph c l))))
	   (save-button (make-instance 'button
				      :master f
				      :text "Save Graph"
				      :command #'save-graph))
	   (load-button (make-instance 'button
				      :master f
				      :text "Load Graph"
				      :command #'(lambda () (load-graph c)))))
      (configure-screen *tk*)
      (configure-frame f)
      (configure-scrolled-canvas sc)
      (configure-canvas c)
      (configure-buttons node-button
			 edge-button delete-node-button
			 start-button end-button clear-button
			 save-button load-button)
      (configure-label l)
      (reset-graph c l))))

(defun populate-graph (&key (graph *graph*) node edge)
  (with-accessors ((vertices vertices)
		   (edges edges)) graph
  (cond
    (node (setf vertices (cons node vertices)))
    (edge (setf edges (cons edge edges))))))

(defun read-data (continuation &key text)
  (let* ((m (make-instance 'toplevel))
	 (f (make-instance 'frame :master m))
	 (l-text (if text text "Insert The Object Value"))
	 (l (make-instance 'label
			   :master f
			   :text l-text))
	 (v (make-instance 'entry
			   :master f
			   :width *entry-width*)))
    (wm-title m "Entry")
    (pack f)
    (pack l)
    (pack v)
    (bind v "<Return>"
	  (lambda (evt &aux (result (text v)))
	    (destroy m)
	    (funcall continuation result)))))


(defun search-node (x y &key (graph *graph*))
  (with-accessors ((vertices vertices)) graph
    (loop for vertice in vertices do
	 (with-accessors ((pos pos)) vertice
	   (let ((x0 (- (car pos) *node-size*))
		 (y0 (- (cadr pos) *node-size*))
		 (x1 (+ (car pos) *node-size*))
		 (y1 (+ (cadr pos) *node-size*)))
	     (when (and (< x0 x x1) (< y0 y y1))
	       (return-from search-node vertice))))))
  nil)

(defun update-color (&key node edge (graph *graph*))
  (with-accessors ((vertices vertices)
		   (edges edges)) graph
    (setf vertices (mapcar #'(lambda (n)
			       (with-accessors ((color color)) n
				 (setf color (if (find n node)
						 *path-color*
						 *node-color*))
				 n))
			   vertices)
	  edges (mapcar #'(lambda (n)
			    (setf (slot-value n 'color)
				  (if (find n edge)
				      *path-color*
				      *node-color*))
			    n)
			edges))))

(defun nodes-name (nodes)
  (if nodes
      (reduce #'(lambda (x y) (concatenate 'string x ", " y))
	      (mapcar #'(lambda (x) (slot-value x 'data)) nodes))
      "..."))

(defun update-short-path (&key (graph *graph*) label)
  (when (and graph *begin* *end*)
    (let* ((dijkstra (dijkstra-short-path *begin* *end*))
	   (nodes (car dijkstra))
	   (cost (cadr dijkstra))
	   (names (nodes-name nodes)))
      (format t "Entrou")
      (setf (text label) (concatenate 'string "Cost: " (if cost (write-to-string cost) "0") " Path: " names))
      (update-color :node nodes
		    :edge (edges-containing-nodes nodes :graph graph)
		    :graph graph))))

(defun edges-containing-nodes (nodes &key (graph *graph*))
  (with-accessors ((edges edges)) graph
    (let ((result nil)
	  (result-edges nil))
      (loop for node in nodes
	 for node-aux in (cdr nodes) do
	   (push (cons node node-aux) result))
      (loop for edge in result
	 do (with-accessors ((vertices vertices)) edge
	      (push (car (remove-if-not #'(lambda (x) (or (and (eq (car x) (car edge)) (eq (cadr x) (cdr edge)))
						 (and (eq (car x) (cdr edge)) (eq (cadr x) (car edge)))))
				    edges :key #'(lambda (x) (slot-value x 'vertices))))
		     result-edges))
	 finally (return (nreverse result-edges))))))

(defun save-graph ()
  (read-data #'(lambda (path)
		 (with-open-file (out path
				      :direction :output
				      :if-exists :supersede)
		   (print (marshal *graph*) out)))
	     :text "Insert The File Location"))

(defun load-graph (canvas)
  (read-data #'(lambda (path)
		 (handler-case
		     (with-open-file (in path
					 :direction :input)
		       (setf *graph* (unmarshal (read in)))
		       (clear canvas)
		       (create-graph canvas *graph*))
		   (error () (load-graph canvas))))
	     :text "Insert The File Location"))

(defun normal-mode (canvas)
  (configure canvas :cursor "arrow")
  (bind canvas "<ButtonPress-1>"
	(lambda (evt)
	  (declare (ignore evt)))))

(defun node-add-mode (canvas &key (graph *graph*))
  (configure canvas :cursor "cross")
  (bind canvas "<ButtonPress-1>"
	(lambda (evt)
	  (let* ((pos-x (event-x evt))
		 (pos-y (event-y evt)))
	    (read-data (lambda (data)
			      (let ((node (make-instance 'node
							 :pos (list pos-x pos-y)
							 :data data)))
				(populate-graph :node node :graph graph)
				(clear canvas)
				(create-graph canvas graph))))))))

(defun edge-add-mode (canvas label &key (graph *graph*))
  (configure canvas :cursor "arrow")
  (let ((node1 nil)
	(node2 nil))
    (bind canvas "<ButtonPress-1>"
	  (lambda (evt)
	    (let* ((pos-x (event-x evt))
		   (pos-y (event-y evt))
		   (node (search-node pos-x pos-y :graph graph)))
	      (when node
		(cond
		  ((not node1) (setf node1 node))
		  ((not node2) (progn
				 (setf node2 node)
				 (read-data (lambda (data)
					      (let ((edge (make-instance 'edge
									 :weight (parse-integer data)
									 :vertices (list node1 node2))))
						(populate-graph :edge edge :graph graph)
						(update-short-path :graph graph :label label)
						(clear canvas)
						(create-graph canvas graph))))))
		  (t (setf node1 node
			   node2 nil)))))))))

(defun delete-node-mode (canvas label &key (graph *graph*))
  (configure canvas :cursor "arrow")
  (bind canvas "<ButtonPress-1>"
	(lambda (evt)
	  (let* ((pos-x (event-x evt))
		 (pos-y (event-y evt))
		 (node (search-node pos-x pos-y :graph graph)))
	    (when node
	      (with-accessors ((vertices vertices)
			       (edges edges)) graph
		(cond
		  ((eq node *begin*) (setf *begin* nil))
		  ((eq node *end*) (setf *end* nil)))
	        (setf vertices (remove-if #'(lambda (x) (eq node x)) vertices)
		      edges (remove-if #'(lambda (x) (find node (slot-value x 'vertices))) edges))
		(update-short-path :graph graph :label label)
		(clear canvas)
		(create-graph canvas graph)))))))

(defun set-begin-end-mode (canvas &key mode label (graph *graph*))
  (configure canvas :cursor "arrow")
  (bind canvas "<ButtonPress-1>"
	(lambda (evt)
	  (let* ((pos-x (event-x evt))
		 (pos-y (event-y evt))
		 (node (search-node pos-x pos-y :graph graph)))
	    (when node
	      (if (eq mode :begin)
		  (progn
		    (setf *begin* node))
		  (progn
		    (setf *end* node)))
	      (update-short-path :graph graph :label label)
	      (clear canvas)
	      (create-graph canvas graph))))))

(defun main () (render))
