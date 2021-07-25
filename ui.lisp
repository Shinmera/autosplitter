(in-package #:org.shirakumo.fraf.autosplitter)

(defvar +main+ NIL)
(defvar *last-check* 0)

(defclass ui (org.shirakumo.fraf.trial.alloy:ui
              org.shirakumo.alloy:smooth-scaling-ui
              org.shirakumo.alloy.renderers.simple.presentations:default-look-and-feel)
  ((alloy:target-resolution :initform (alloy:px-size 300 500))
   (alloy:base-scale :initform 1.5)))

(defmethod simple:request-font ((ui ui) (default (eql :default)) &key)
  (simple:request-font ui "NotoSansMono"))

(defclass timer (alloy:label)
  ())

(defmethod alloy:text ((timer timer))
  (format-time (alloy:value timer)))

(presentations:define-realization (ui timer)
  ((timer simple:text)
   (alloy:margins) alloy:text
   :size (alloy:un 12)
   :pattern (colored:color 1 1 1 0.5)
   :valign :middle :halign :right))

(presentations:define-update (ui timer)
  (timer
   :text alloy:text
   :pattern (if (= 0.0 alloy:value)
                (colored:color 1 1 1 0.5)
                colors:white)))

(defclass total-timer (timer)
  ())

(presentations:define-update (ui total-timer)
  (timer
   :size (alloy:un 40)
   :pattern (cond ((done *current-run*) colors:green)
                  ((started *current-run*) colors:white)
                  (T (colored:color 1 1 1 0.5)))))

(defclass diff (alloy:label)
  ())

(defmethod alloy:text ((diff diff))
  (format-time (alloy:value diff) T))

(presentations:define-realization (ui diff)
  ((timer simple:text)
   (alloy:margins) alloy:text
   :size (alloy:un 12)
   :pattern colors:transparent
   :valign :middle :halign :right))

(presentations:define-update (ui diff)
  (timer
   :text alloy:text
   :pattern (cond ((<= alloy:value -60) colors:transparent)
                  ((<= alloy:value   0) colors:green)
                  (T colors:red))))

(trial:define-shader-pass ui-pass (ui)
  ((trial:name :initform 'ui-pass)
   (trial:color :port-type trial:output :attachment :color-attachment0)
   (trial:depth :port-type trial:output :attachment :depth-stencil-attachment)))

(defmethod trial:render :before ((pass ui-pass) target)
  #++
  (gl:clear-color 1 0 1 0)
  (gl:clear-color 0 0 0 0))

(defmethod trial:stage ((pass ui-pass) (area trial:staging-area))
  (call-next-method)
  (trial:stage (simple:request-font pass :default) area)
  (trial:stage (trial:framebuffer pass) area))

(defmethod trial:compile-to-pass (object (pass ui-pass)))
(defmethod trial:compile-into-pass (object container (pass ui-pass)))
(defmethod trial:remove-from-pass (object (pass ui-pass)))

;; KLUDGE: No idea why this is necessary, fuck me.
(defmethod simple:request-font :around ((pass ui-pass) font &key)
  (let ((font (call-next-method)))
    (unless (and (alloy:allocated-p font)
                 (trial:allocated-p (org.shirakumo.alloy.renderers.opengl.msdf:atlas font)))
      (trial:commit font (trial:loader +main+) :unload NIL))
    font))

(defmethod initialize-instance :after ((pass ui-pass) &key)
  (let ((layout (make-instance 'alloy:grid-layout :col-sizes '(T) :row-sizes '(50 T 50 80) :layout-parent (alloy:layout-tree pass)))
        (focus (make-instance 'alloy:focus-list :focus-parent (alloy:focus-tree pass)))
        (run *current-run*))
    (let ((stats (make-instance 'alloy:grid-layout :col-sizes '(T 100) :row-sizes '(20) :cell-margins (alloy:margins) :layout-parent layout)))
      (alloy:enter "Best" stats)
      (alloy:represent (slot-value *best-run* 'total) 'timer :layout-parent stats)
      (alloy:enter "Previous" stats)
      (alloy:represent (slot-value *last-run* 'total) 'timer :layout-parent stats))
    (let ((stages (make-instance 'alloy:grid-layout :col-sizes '(T 60 60) :row-sizes '(20) :cell-margins (alloy:margins) :layout-parent layout)))
      (dotimes (i (length (areas run)))
        (let ((i i))
          (alloy:enter (aref *area-names* i) stages)
          (alloy:represent (lambda ((run (areas run)) (ideal (areas *ideal-run*)))
                             (- (aref run i) (aref ideal i)))
                           'diff :layout-parent stages)
          (alloy:represent (lambda ((run (areas run)))
                             (aref run i))
                           'timer :layout-parent stages))))
    (let ((stats (make-instance 'alloy:grid-layout :col-sizes '(T 60) :row-sizes '(20) :cell-margins (alloy:margins) :layout-parent layout)))
      (alloy:enter "Deaths" stats)
      (alloy:represent (slot-value run 'deaths) 'alloy:label :layout-parent stats)
      (alloy:enter "Berries" stats)
      (alloy:represent (slot-value run 'berries) 'alloy:label :layout-parent stats))
    (alloy:represent (total run) 'total-timer :layout-parent layout)))

(defmethod trial:handle :after ((ev trial:tick) (pass ui-pass))
  (refresh-run *current-run* (trial:tt ev) (trial:dt ev)))

(defmethod trial:handle :after ((ev trial:gamepad-press) (pass ui-pass))
  (when (and (eql (trial:button ev) :select)
             (not (started *current-run*)))
    (setf (total *current-run*) 0.0)
    (setf (aref (areas *current-run*) 0) 0.01)))

(let ((last-time 0))
  (defun refresh-run (run tt dt)
    (when (< 0.5 (- tt *last-check*))
      (setf *last-check* tt)
      (let ((save (parse-save)))
        (unless (eql (getf save :started) (started run))
          (v:info :splits "Starting new run...")
          (when (started run)
            (update *last-run* run))
          (reset run)
          (setf last-time -1)
          (convert-to-run :run run :save save))
        (unless (eql (getf save :done) (done run))
          (v:info :splits "Run complete! ~a" run)
          (update-ideal *ideal-run* run)
          (when (update-best *best-run* run)
            (v:info :splits "New PB!")))
        (when (/= last-time (getf save :time))
          (setf last-time (getf save :time))
          (convert-to-run :run run :save save))))
    (unless (done run)
      ;; Don't re-read file, just update times
      (and (loop with areas = (areas run)
                 for i downfrom (1- (length areas)) to 0
                 do (when (< 0.0 (aref areas i))
                      (incf (aref areas i) dt)
                      (return T)))
           (incf (total run) dt))
      (setf (areas run) (areas run)))))

(defclass main (trial:main)
  ()
  (:default-initargs :width 400 :height 600 :title "Autosplitter"))

(defmethod initialize-instance :before ((main main) &key)
  (load-stats)
  (setf *last-check* 0)
  (reset *current-run*)
  (setf +main+ main))

(defmethod trial:finalize :after ((main main))
  (save-stats)
  (setf +main+ NIL)
  (clrhash (alloy::observers *current-run*))
  (clrhash (alloy::observers *ideal-run*))
  (clrhash (alloy::observers *best-run*)))

(defmethod trial:setup-scene ((main main) scene)
  (trial:enter (make-instance 'ui-pass) scene))

(defun launch (&rest initargs)
  (apply #'trial:launch 'main initargs))

(defun main ()
  (launch))
