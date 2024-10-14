(defun c2-phi-fact (angle)
  "Apply the combination 2 partial factor"
  (interactive "nEnter Angle: ")
  (let* ((phi (if (<= angle 3)
                  angle
                (/ (* angle 3.1452) 180)))  ;; Convert to radians if angle > 3
         (res (atan (/ (tan phi) 1.25))))   ;; Calculate the adjusted angle
    (message "Result: %f" (* 180 (/ res 3.1452)))))               ;; Convert the result back to degrees

(defun stroud-values (plast)
  "Calculate the Stroud F1 and F2 factors for a given Plasticity index"
  (interactive "nEnter plastcity index: ")
  (let* ((f1 (+ 4.36 (/ 8910 (* plast plast plast)))
             )
         (f2 (+ 435.0 (/ 56000 (expt plast 2.07))))
         )
    (message "PlastInd: %f, F1: %f, F2: %f" plast f1 f2)
    )
  )

(defun cu-val-to-desc (cu)
  "Convert the undrained shear strength CU to a soil strength description.
CU is the undrained shear strength value."
  (interactive "nEnter undrained shear strength (CU): ") ;; Prompt for CU value
  (let ((description (cond
                      ((= cu 0.0) 'not-clay)
                      ((< cu 20.0) 'very-soft)
                      ((< cu 40.0) 'soft)
                      ((< cu 75.0) 'firm)
                      ((< cu 150.0) 'very-stiff)
                      (t 'hard)))) ;; Default case
    (message "The soil strength description for CU = %.2f is: %s" cu description))) ;; Display the result

(defun bs8002-fine-phi-ang-crit (plast-ind)
  "Calculate the critical angle based on the plasticity index PLAST-IND.
The formula is 42 - 12.5 * log10(PLAST-IND)."
  (interactive "nEnter plasticity index: ") ;; Prompt for the plasticity index
  (let ((critical-angle (- 42 (* 12.5 (log plast-ind 10))))) ;; Calculate the critical angle
    (message "The critical angle for plasticity index %.2f is: %.2f degrees" plast-ind critical-angle))) ;; Display the result

(defun check-rad (phi)
  "Convert degrees PHI to radians."
  (* pi (/ phi 180.0)))

(defun k-active (phi)
  "Calculate the active earth pressure coefficient for angle PHI in degrees.
Return the calculated value and display it in a message."
  (interactive "nEnter angle PHI in degrees: ")
  (let ((phid (check-rad phi)))
    (let ((k-active-value (/ (- 1 (sin phid))
                             (+ 1 (sin phid)))))
      (message "k_active(%d) = %f" phi k-active-value)  ; Display the message
      k-active-value)))                                   ; Return the value

(defun k-active-with-slope (phi slope)
  "Calculate the active earth pressure coefficient with slope.
PHI is the angle in degrees and SLOPE is the slope angle in degrees.
Return the calculated value and display it in a message."
  (interactive "nEnter angle PHI in degrees: \nnEnter slope in degrees: ")
  (let ((phid (check-rad phi))
        (sloped (check-rad slope)))
    (let ((k-active-slope-value (/ (k-active phi) (tan sloped))))
      (message "k_active_with_slope(%d, %d) = %f" phi slope k-active-slope-value)  ; Display the message
      k-active-slope-value)))  ; Return the value

(defun k-passive (phi)
  "Calculate the passive earth pressure coefficient for angle PHI in degrees.
Return the calculated value and display it in a message."
  (interactive "nEnter angle PHI in degrees: ")
  (let ((phid (check-rad phi)))
    (let ((k-passive-value (/ 1 (k-active phi))))
      (message "k_passive(%d) = %f" phi k-passive-value)  ; Display the message
      k-passive-value)))                                   ; Return the value

(defun k0 (phi)
  "Calculate the k0 value for angle PHI in degrees.
Return the calculated value and display it in a message."
  (interactive "nEnter angle PHI in degrees: ")
  (let ((phid (check-rad phi)))
    (let ((k0-value (- 1 (sin phid))))
      (message "k0(%d) = %f" phi k0-value)  ; Display the message
      k0-value)))                            ; Return the value

(defun k-factors (phi)
  "Display the factors k0, k_active, and k_passive for angle PHI in degrees.
Return a list containing these values."
  (interactive "nEnter angle PHI in degrees: ")
  (let ((k0-value (k0 phi))
        (k-active-value (k-active phi))
        (k-passive-value (k-passive phi)))
    (message "phi: %f, k0: %f, k_active: %f, k_passive: %f"
             phi k0-value k-active-value k-passive-value)
    (list phi k0-value k-active-value k-passive-value)))  ; Return the values as a list
