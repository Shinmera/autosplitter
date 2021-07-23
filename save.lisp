(in-package #:org.shirakumo.fraf.autosplitter)

(defvar *area-names*
  #("Prologue"
    "Forsaken City"
    "Old Site"
    "Celestial Resort"
    "Golden Ridge"
    "Mirror Temple"
    "Reflection"
    "The Summit"
    "Epilogue"))

(defun parse-side (node)
  (list :done (string= "true" (or (plump:attribute node "Completed") "false"))
        :berries (parse-integer (or (plump:attribute node "TotalStrawberries") "0"))
        :deaths (parse-integer (or (plump:attribute node "Deaths") "0"))
        :time (/ (parse-integer (or (plump:attribute node "BestTime") "0")) 10000000.0)
        :checkpoints (length (lquery:$ node "Checkpoints" "string"))))

(defun parse-area (node)
  (let ((sides (lquery:$ node "AreaModeStats" (map #'parse-side))))
    (list :id (parse-integer (plump:attribute node "ID"))
          :time (loop for side across sides sum (getf side :time))
          :berries (loop for side across sides sum (getf side :berries))
          :deaths (loop for side across sides sum (getf side :deaths))
          :sides sides)))

(defun save-file-path (&optional (id "0"))
  (make-pathname :name id
                 :type "celeste"
                 :directory `(:absolute :home ".local" "share" "Celeste" "Saves")))

(defun parse-save (&optional (file (save-file-path)))
  (handler-case
      (let* ((plump:*tag-dispatchers* plump:*xml-tags*)
             (save (lquery:$1 (initialize file) "SaveData"))
             (areas (lquery:$ save "Areas AreaStats" (map #'parse-area))))
        (list :started T
              :done (getf (aref (getf (aref areas 7) :sides) 0) :done)
              :berries (parse-integer (lquery:$1 save "TotalStrawberries" (text)))
              :deaths (parse-integer (lquery:$1 save "TotalDeaths" (text)))
              :time (/ (parse-integer (lquery:$1 save "Time" (text))) 10000000.0)
              :areas areas))
    (SB-EXT:FILE-DOES-NOT-EXIST ()
      (list :started NIL
            :done NIL
            :berries 0
            :deaths 0
            :time 0.0
            :areas #((:ID 0 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 1 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 2 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 3 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 4 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 5 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 6 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 7 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 8 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 1 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)))
                     (:ID 10 :TIME 0.0 :BERRIES 0 :DEATHS 0 :SIDES
                      #((:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0)
                        (:DONE NIL :BERRIES 0 :DEATHS 0 :TIME 0.0 :CHECKPOINTS 0))))))))

(defun convert-to-run (&key (save (parse-save)) (run (make-instance 'run)))
  (destructuring-bind (&key started done berries deaths time areas) save
    (setf (started run) started)
    (setf (done run) done)
    (setf (total run) time)
    (setf (berries run) berries)
    (setf (deaths run) deaths)
    (loop with target = (areas run)
          for i from 0 below (length target)
          for a-side = (aref (getf (aref areas i) :sides) 0)
          do (when (getf a-side :done)
               (setf (aref target i) (getf a-side :time))
               (when (< i (1- (length target)))
                 (setf (aref target (1+ i)) 0.01))))
    (setf (areas run) (areas run))
    run))
