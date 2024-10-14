(require 'widget)

(defun my-calculate-result ()
  "Calculate the result based on the input values."
  (let* ((value1 (string-to-number (widget-value (get-widget "Input1"))))
         (value2 (string-to-number (widget-value (get-widget "Input2"))))
         (value3 (string-to-number (widget-value (get-widget "Input3"))))
         (value4 (string-to-number (widget-value (get-widget "Input4")))))
    (+ value1 value2 value3 value4))) ; Change this calculation as needed

(defun get-widget (name)
  "Return the widget with NAME."
  (cl-find name (widget-get (widget-at) :widget) :key 'widget-get :test 'equal))

(defun my-update-result ()
  "Update the result display."
  (let ((result (my-calculate-result)))
    (setf (widget-value (get-widget "Result")) (format "Result: %d" result))
    (widget-setup)))

(defun my-create-ui ()
  "Create the input fields and result display."
  (interactive)
  (switch-to-buffer "*My UI*")
  (erase-buffer)
  (widget-insert "Input Fields:\n\n")

  ;; Create labeled input fields
  (let ((input-field-setup (lambda (label name)
                             (widget-insert (format "%s: " label))
                             (widget-create 'editable-field
                                            :size 10
                                            :name name
                                            :notify (lambda (&rest _)
                                                      (my-update-result)))
                             (widget-insert "\n"))))
    ;; Create the input fields with labels
    (funcall input-field-setup "Input 1" "Input1")
    (funcall input-field-setup "Input 2" "Input2")
    (funcall input-field-setup "Input 3" "Input3")
    (funcall input-field-setup "Input 4" "Input4")

    (widget-insert "\nResult:\n")

    (widget-create 'item
                   :tag "Result"
                   :value "Result: 0"
                   :name "Result"))

  (widget-setup)

  ;; Ensure that the local keymap is set to use Evil
  (use-local-map (copy-keymap widget-keymap))
  (evil-normal-state) ;; Switch to normal state after setup

  ;; Re-enable Evil's keybindings
  (define-key evil-normal-state-map (kbd "C-n") 'widget-forward)
  (define-key evil-normal-state-map (kbd "C-p") 'widget-backward)
  (define-key evil-normal-state-map (kbd "RET") 'widget-button-press))

;; Run the UI
(my-create-ui)
