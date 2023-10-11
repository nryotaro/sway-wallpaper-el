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
  (message (shell-quote-argument image-path))
  (call-process-shell-command
   (concat "swaymsg output " output " bg '" (shell-quote-argument image-path) "' fill")
   nil
   t))

(defun sway-wallpaper-set-background-image
    ()
  "Display a background image on an outbput."
  (interactive)
  (let ((image-path (image-dired-original-file-name))
	(outputs (sway-wallpaper--detect-outputs)))
    (if (eq (length outputs) 1)
	(sway-wallpaper--display-image (car outputs) image-path)
      (sway-wallpaper--display-image (completing-read "Output: " outputs) image-path))))

(provide 'sway-wallpaper)
;;; sway-wallpaper.el ends here
