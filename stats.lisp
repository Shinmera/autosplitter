(in-package #:org.shirakumo.fraf.autosplitter)

(defvar *last-run* NIL)
(defvar *best-run* NIL)
(defvar *ideal-run* NIL)
(defvar *current-run* NIL)

(defun next-run-no ()
  (1+ (run-no *last-run*)))

(defun format-time (time &optional sign)
  (let* ((sign (if (<= 0 time) (if sign #\+ #\ ) #\-))
         (time (abs time))
         (h (floor time 3600))
         (m (mod (floor time 60) 60))
         (s (mod (floor time) 60))
         (ts (floor (* 100 (mod time 1)))))
    (cond ((< 0 h)
           (format NIL "~c~d:~2,'0d:~2,'0d.~2,'0d" sign h m s ts))
          ((< 0 m)
           (format NIL "~c~d:~2,'0d.~2,'0d" sign m s ts))
          (T
           (format NIL "~c~d.~2,'0d" sign s ts)))))

(defclass run (alloy:observable)
  ((run-no :initarg :run-no :initform (next-run-no) :accessor run-no)
   (start-time :initarg :start-time :initform (get-universal-time) :accessor start-time)
   (started :initarg :started :initform NIL :accessor started)
   (done :initarg :done :initform NIL :accessor done)
   (total :initarg :total :initform 0.0 :accessor total)
   (berries :initarg :berries :initform 0 :accessor berries)
   (deaths :initarg :deaths :initform 0 :accessor deaths)
   (areas :initarg :areas :initform (make-array 8 :initial-element 0.0) :accessor areas)))

(defmethod print-object ((run run) stream)
  (print-unreadable-object (run stream :type T)
    (format stream "~d ~a" (run-no run) (format-time (total run)))))

(defmacro define-notifier (slot)
  `(defmethod (setf ,slot) :after (value (run run))
     (alloy:notify-observers ',slot run value run)))

(define-notifier start-time)
(define-notifier started)
(define-notifier done)
(define-notifier total)
(define-notifier berries)
(define-notifier deaths)
(define-notifier areas)

(defun run-initargs (run)
  (list :run-no (run-no run)
        :start-time (start-time run)
        :started (started run)
        :done (done run)
        :total (total run)
        :berries (berries run)
        :deaths (deaths run)
        :areas (areas run)))

(defun reset (run)
  (setf (start-time run) (get-universal-time)
        (started run) NIL
        (done run) NIL
        (total run) 0.0
        (berries run) 0
        (deaths run) 0
        (areas run) (fill (areas run) 0.0)))

(defun update (run source)
  (setf (run-no run) (run-no source)
        (start-time run) (start-time source)
        (started run) (started source)
        (done run) (done source)
        (total run) (total source)
        (berries run) (berries source)
        (deaths run) (deaths source)
        (areas run) (areas source)))

(defun update-ideal (run source)
  (flet ((better (previous new)
           (if (<= previous 0.0) new (min previous new))))
    (setf (total run) (better (total run) (total source))
          (berries run) (min (berries run) (berries source))
          (deaths run) (min (deaths run) (deaths source)))
    (loop for i from 0 below (length (areas run))
          do (setf (aref (areas run) i) (better (aref (areas run) i) (aref (areas source) i))))))

(defun update-best (run source)
  (when (or (<= (total run) 0.0)
            (< (total source) (total run)))
    (update run source)))

(defun load-stats (&key (file #p "~/.config/celeste-splits/any.lisp") replace)
  (with-open-file (stream file :direction :input :if-does-not-exist NIL)
    (cond (stream
           (let* ((data (read stream))
                  (last (getf data :last))
                  (best (getf data :best))
                  (ideal (getf data :ideal)))
             (cond ((or replace (null *last-run*))
                    (update *last-run* (apply #'make-instance 'run last))
                    (update *best-run* (apply #'make-instance 'run best))
                    (update *ideal-run* (apply #'make-instance 'run ideal)))
                   (T
                    (when (< (start-time *last-run*) (getf last :start-time))
                      (update *last-run* (apply #'make-instance 'run last)))
                    (when (< (total *best-run*) (getf best :total))
                      (update *best-run* (apply #'make-instance 'run best)))
                    (update-ideal *ideal-run* (apply #'make-instance 'run ideal))))))
          (T
           (warn "Stats file does not exist!")))
    (when (null *current-run*)
      (setf *current-run* (make-instance 'run)))))

(defun save-stats (&key (file #p "~/.config/celeste-splits/any.lisp"))
  (ensure-directories-exist file)
  (with-open-file (stream file :direction :output :if-exists :supersede)
    (prin1 (list :last (run-initargs *last-run*)
                 :best (run-initargs *best-run*)
                 :ideal (run-initargs *ideal-run*))
           stream)))

(setf *last-run* (make-instance 'run :run-no 0 :start-time 0))
(setf *best-run* (make-instance 'run :run-no 0))
(setf *ideal-run* (make-instance 'run :run-no 0))
