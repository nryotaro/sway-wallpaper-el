;;; package --- Use an image as a background image -*- lexical-binding: t -*-
;;; Commentary:
;;; get the outputs by swapmsg, and display an image as a background image on an output.
;;; Code:

(require 'dash)
(require 'image-dired)

(defun sway-wallpaper--detect-outputs
    ()
  "Detect outputs by swapmsg."
  (let ((parsed (-> "swaymsg -t get_outputs"
		    shell-command-to-string
		    json-parse-string)))
    (mapcar (lambda (x) (gethash "name" x)) parsed)))

(defun sway-wallpaper--display-image
    (output image-path)
  "Display a file at IMAGE-PATH on an OUTPUT."
  (call-process-shell-command
   (concat "swaymsg output " output " bg '" (shell-quote-argument image-path) "' fill")
   nil
   t))

(defcustom sway-wallpaper-set-image-callback nil
  "If a image was set successfully, the function is called.
The function is called with the image and the output."
  :type 'function
  :group 'sway-wallpaper)

(defun sway-wallpaper-set-background-image
    ()
  "Display a background image on an outbput."
  (interactive)
  (let* ((image-path (image-dired-original-file-name))
	 (outputs (sway-wallpaper--detect-outputs))
	 (output (if (eq (length outputs) 1)
		     (car outputs)
		   (completing-read "Output: " outputs)))
	 (result (sway-wallpaper--display-image output image-path))
	 (a (setq doge result))
	 (b (setq doge2 (concat "cond: " (string (eq result 0))))))

    (if (and (eq result 0) (fboundp 'sway-wallpaper-set-image-callback))
	(sway-wallpaper-set-image-callback image-path output))))

(provide 'sway-wallpaper)
;;; sway-wallpaper.el ends here
