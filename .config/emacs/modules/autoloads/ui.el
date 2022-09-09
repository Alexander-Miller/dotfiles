;; -*- lexical-binding: t -*-

(defun std::ui::writeroom-hide-line-numbers (arg)
  (pcase arg
    (1  (display-line-numbers-mode -1))
    (-1 (display-line-numbers-mode +1))))

(defun std::ui::change-font-size ()
  "Select a new font size."
  (interactive)
  (let* ((current-size (plist-get std::ui::default-font-spec :size))
         (new-size (read-number (format "New Size (current: %s): " current-size))))
    (setf std::ui::default-font-spec (plist-put std::ui::default-font-spec :size new-size))
    (-let [new-font (eval `(font-xlfd-name (font-spec ,@std::ui::default-font-spec)))]
      (setf (alist-get 'font default-frame-alist) new-font)
      (set-frame-font new-font :keep-size (frame-list)))))

(defun std::ui::reload-theme ()
  "Reload the current theme."
  (interactive)
  (load-theme (car custom-enabled-themes) :no-confirm))

(defhydra std::ui::hydra (:exit t :hint t)
  ("d" #'disable-theme             "Disable Theme")
  ("l" #'load-theme                "Load Theme")
  ("r" #'std::ui::reload-theme     "Reload Theme")
  ("f" #'std::ui::change-font-size "Change font size"))
