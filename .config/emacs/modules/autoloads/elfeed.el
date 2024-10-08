;; -*- lexical-binding: t -*-

(defun std::elfeed::draw-entry (entry)
  (let* ((tag-width   24)
         (src-width   16)
         (date        (elfeed-search-format-date (elfeed-entry-date entry)))
         (title       (or (elfeed-meta entry :title) (elfeed-entry-title entry) ""))
         (title-faces (elfeed-search--faces (elfeed-entry-tags entry)))
         (feed        (elfeed-entry-feed entry))
         (feed-title  (when feed (or (elfeed-meta feed :title) (elfeed-feed-title feed))))
         (tags        (mapcar #'symbol-name (elfeed-entry-tags entry)))
         (tags-str    (concat "[" (mapconcat 'identity tags ",") "]"))
         (tag-column (elfeed-format-column
                      tags-str
                      (elfeed-clamp (length tags-str) tag-width tag-width)
                      :left))
         (feed-column (elfeed-format-column
                       feed-title
                       (elfeed-clamp src-width src-width src-width)
                       :left)))
    (insert (std::face date 'elfeed-search-date-face) "   ")
    (insert (std::face feed-column 'elfeed-search-feed-face) " ")
    (insert (std::face tag-column 'elfeed-search-tag-face) " ")
    (insert (propertize title 'face title-faces 'kbd-help title))))

(std::defun-with-desktop
 :name std::elfeed
 :command #'elfeed
 :check (eq major-mode 'elfeed-search-mode))

(defun std::elfeed::visit-entry-dwim (&optional arg)
  "Open an entry in the browser.
If the entry is a youtube video use mpv instead, unless ARG is non-nil."
  (interactive "P")
  (if arg
      (elfeed-search-browse-url)
    (-let [entry (if (eq major-mode 'elfeed-show-mode)
                     elfeed-show-entry
                   (elfeed-search-selected :single))]
      (if (s-matches? (rx "https://www.youtube.com/watch" (1+ any))
                      (elfeed-entry-link entry))
          (let* ((quality (completing-read
                           "Max height resolution: "
                           '("best" "240" "480" "720" "1080")))
                 (format (if (string= "best" quality)
                             ""
                           (format "--ytdl-format=bestvideo[height<=%s]+bestaudio/best" quality))))
            (message "Opening %s with height ≤ %s with mpv..."
                     (std::face (elfeed-entry-link entry) 'font-lock-string-face)
                     (std::face quality 'font-lock-keyword-face))
            (elfeed-untag entry 'unread)
            (start-process "elfeed-mpv" nil "mpv" format (elfeed-entry-link entry))
            (elfeed-search-update :force))
        (if (eq major-mode 'elfeed-search-mode)
            (elfeed-search-browse-url)
          (elfeed-show-visit))))))

(defun std::elfeed::ignore-entry ()
  (interactive)
  (-let [entries (elfeed-search-selected)]
    (elfeed-tag entries 'ignore)
    (mapc #'elfeed-search-update-entry entries)
    (elfeed-search-update :force)))
