;;;; mcst.lisp

(in-package #:mcst)

;; node - number or list
(defun node (win count)
  (list :leaf win count))

(defun node-win(node)
  (assert (eq (first node) :leaf))
  (second node))
(defun node-count(node)
  (assert (eq (first node) :leaf))
  (second node))

(defun leafp(node)
  (eq (first node) :leaf) )

(defun future-win(node)
  (if (leafp node)
      (node-win node)
      (reduce #'+ (mapcar #'future-win node))
      ))

(defun future-sim(node)
  (if (leafp node)
      (node-count node)
      (reduce #'+ (mapcar #'future-win node))))

(defun UCT(parent selected)
  (if (zerop (future-sim selected)) 0
      (+
       (/ (future-win selected) 
          (future-sim selected))
       (* 1.4 
          (sqrt
           (/ (log (future-sim parent))
              (future-sim selected)))))))

(defun select-best-node(node)
  (assert node)
  (first (reduce 
          #'(lambda(max current)(if (> (second max) (second current)) max current))
          (mapcar #'list 
                  (loop for x from 0 to (length node) :collect x)
                  (mapcar #'(lambda(el)(UCT node el)) node)))))

(defun softmax(l)
  (let* ((z (mapcar #'exp l))
         (z-sum (reduce #'+ z)))
    (mapcar #'(lambda(i) (/ i z-sum)) z)))

(defun weighted-random(l w)
  (random-elt (reduce #'append (mapcar #'(lambda(l w) (loop for x from 1 to (max 1 (round (* 100 w))) :collect l)) l w))))

(defun select-best-node(node)
  (assert node)
  (weighted-random 
   (loop for x from 0 to (1- (length node)) :collect x)
   (print (softmax (mapcar #'(lambda(el)(UCT node el)) node)))))

(defun next-best(tree explore-fn)
  " path tree expore-fn => (list path tree) "
  (let ((new-tree (if (leafp tree) (funcall explore-fn) tree)))
    (list (select-best-node new-tree) new-tree)))

(defun mcst (path tree completed-fn explore-fn apply-action)
  (if (not(zerop (print (funcall completed-fn path))))
      (progn
        ;;(format t "~%End node:~S" tree)
        (list path (node (if (> (funcall completed-fn path) 0) 1 0)
                         1)))
      (destructuring-bind (best new-tree) (next-best tree explore-fn)
        (destructuring-bind (end-path end-tree) 
            (progn 
              (funcall apply-action best)
              (mcst (list* best path) (nth best new-tree) completed-fn explore-fn apply-action))
          (list end-path (progn (setf (nth best new-tree) end-tree) new-tree))))))
;;;
(defparameter *state* '(1 1))

(defparameter *maze*  '((-0 -0 -0  -0)
                        (-0  0 10 -0)
                        (-0  0 0 -0)
                        (-0 -0 -0 -0)))

(defun available-actions(&optional (state *state*))
  (print(append
    (when (< (first state) (1- (length (first *maze*))) '(right))
    (when (> (first state) 0) '(left))
    (when (> (second state) 0) '(down))
    (when (< (second state) (1- (length *maze*))) '(up))))))

(defun explore-maze()
  (loop for x from 0 to (1-(length (available-actions))) :collect (node 0 0)))

(defun apply-action(no)
  (let ((new-state (ecase (print(nth no (available-actions)))
                     (right (list (1+ (first *state*)) (second *state*)))
                     (left  (list (1- (first *state*)) (second *state*)))
                     (down  (list (first *state*)      (1- (second *state*))))
                     (up    (list (first *state*)      (1+ (second *state*))))))
        (new-maze (copy-tree *maze*))
        (old-state (copy-tree *state*)))
    (setf (nth (first *state*) (nth (second *state*) (reverse new-maze))) "*")
    (setf *state* new-state )
    (setf (nth (first *state*) (nth (second *state*) (reverse new-maze))) "+")
    (format t "~%~{~{~4A~}~%~}" new-maze)))
  
(defun completed(path)
  (if (> (print (length path)) 20) -1
      (nth (first *state*) (nth (second *state*) (reverse *maze*)))
      ))

(defun explore (node)
  (setf *state* (list 1 1)) 
  (format t "~%Start")
  (copy-tree (destructuring-bind (path node) (mcst '() node #'completed #'explore-maze #'apply-action)
               (format t "~%Explored:~A ~A = ~A" path (length  path) (completed path))
               (print path)
               (print (completed *state*))
               (when (>  (completed *state*) 0)
                 (format t "~%----------------------------------------------------------  ~A"(completed *state*)) )
               node)))


(setf node (node 0 0))
(loop :repeat 1 do (setf node (explore node)))
