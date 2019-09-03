;;;; dijkstra.asd

(asdf:defsystem #:dijkstra
  :description "GUI Application For The Dijkstra Algorithm"
  :author "Gustavo Pacheco <gap1512@gmail.com>"
  :serial t
  :depends-on (#:ltk #:marshal)
  :components ((:file "package")
               (:file "dijkstra" :depends-on ("package")))
  :build-operation "asdf:program-op"
  :build-pathname "dijkstra"
  :entry-point "dijkstra:main")

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable T :compression T))

;;sbcl --eval "(asdf:operate :build-op :dijkstra)"
