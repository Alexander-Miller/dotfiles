;; -*- lexical-binding: t -*-

(defun std::ui::writeroom-hide-line-numbers (arg)
  (pcase arg
    (1  (display-line-numbers-mode -1))
    (-1 (display-line-numbers-mode t))))
