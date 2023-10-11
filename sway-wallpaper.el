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
   (concat "swaymsg output " output " bg '" (shell-quote-argument image-path) "' fill")))

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
	 (result (sway-wallpaper--display-image output image-path)))
    (if (and (eq result 0)
	     (functionp sway-wallpaper-set-image-callback))
	(apply sway-wallpaper-set-image-callback (list image-path output)))))

(defcustom sway-wallpaper-dirs '()
  "A list of directories that contains images."
  :type '(repeat directory)
  :group 'sway-wallpaper)

(defun sway-wallpaper-preview ()
  "Open a directory with `image-dired`."
  (interactive)
  (image-dired (completing-read "Directory: " sway-wallpaper-dirs))
  (revert-buffer))

(defun sway-wallpaper--bind-key
    ()
  "Bind a key to sway-wallpaper-set-background-image."
  (bind-key "b" 'sway-wallpaper-set-background-image image-dired-thumbnail-mode-map))

;;;###autoload
(define-minor-mode sway-wallpaper-mode
  "This minor mode provides features to display background images on a sway output."
  :global t
  :group 'sway-wallpaper
  (if sway-wallpaper-mode
      (add-hook 'image-dired-thumbnail-mode-hook 'sway-wallpaper--bind-key)
    (remove-hook 'image-dired-thumbnail-mode-hook 'sway-wallpaper--bind-key)))

(provide 'sway-wallpaper)
;;; sway-wallpaper.el ends here
