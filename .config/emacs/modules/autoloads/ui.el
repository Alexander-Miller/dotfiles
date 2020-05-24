;; -*- lexical-binding: t -*-

(defun std::ui::writeroom-hide-line-numbers (arg)
  (pcase arg
    (1  (display-line-numbers-mode -1))
    (-1 (display-line-numbers-mode +1))))

(defun std::ui::change-font ()
  (interactive)
  (let* ((current-size (plist-get std::default-font-spec :size))
         (new-size (read-number (format "New Size (current: %s): " current-size))))
    (setf std::default-font-spec (plist-put std::default-font-spec :size new-size))
    (-let [new-font (eval `(font-xlfd-name (font-spec ,@std::default-font-spec)))]
      (setf (alist-get 'font default-frame-alist) new-font)
      (set-frame-font new-font))))
