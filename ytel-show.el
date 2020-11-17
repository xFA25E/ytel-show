;;; ytel-show.el --- View youtube video information from ytel  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Valeriy Litkovskyy

;; Author: Valeriy Litkovskyy
;; Keywords: youtube multimedia
;; Version: 0.1.0
;; URL: https://github.com/xFA25E/ytel-show
;; Package-Requires: ((ytel "0.1.0") (emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Ytel-Show: browse youtube video in Emacs
;;
;; This package relies on `YTEL' package to fetch information.
;;
;;
;; Easy setup:
;;
;; (define-key ytel-mode-map (kbd "RET") #'ytel-show)
;;
;; OR
;;
;; (use-package ytel-show
;;   :after ytel
;;   :bind (:map ytel-mode-map ("RET" . ytel-show)))
;;
;;
;; Usage:
;;
;; Ytel-Show:
;; n/p - switch videos
;; G - recache video information
;;
;; Ytel-Show-Comments:
;; l - go to previous page
;; g - retry downloading failed page
;;
;; Customization:
;;
;; Default buffer name: `YTEL-SHOW-DEFAULT-BUFFER-NAME'
;; `YTEL-SHOW-COMMENTS-DEFAULT-BUFFER-NAME'.
;;
;; Thumbnail sizes: `YTEL-SHOW-IMAGE-MAX-WIDTH' `YTEL-SHOW-IMAGE-MAX-HEIGHT'
;;
;; Faces: `YTEL-SHOW-VIDEO-LIKES-FACE' `YTEL-SHOW-AUTHOR-SUBS-FACE'
;; `YTEL-SHOW-COMMENTS-OWNER-FACE'
;;
;;
;; Extensibility:
;;
;; You can easily extend this package by giving `YTEL-SHOW' function a BUFFER
;; argument and a vector of VIDEO-IDS.  Your videos will be display in a new
;; buffer.  For example: you could customize `BROWSE-URL-BROWSER-FUNCTION' to
;; detect youtube video urls and display them using YTEL-SHOW.
;;
;; Same thing with `YTEL-SHOW-COMMENTS'.  It takes ID, TITLE and a BUFFER.
;;
;; Note:
;;
;; Viewed videos are set with `YTEL-SHOW' command.  In order to load videos from
;; other *ytel* pages, you should switch page in *ytel* buffer and call
;; `YTEL-SHOW' again.
;;
;; Same thing with `YTEL-SHOW-COMMENTS' command.  It sets all the necessary
;; information.
;;
;; Caveats:
;;
;; Sometimes json-reader will fail to parse a response from invidious instance
;; or the connection will timeout.  You will see an almost empty buffer.
;; Usually,hitting the /G/ (`YTEL-SHOW-RELOAD-VIDEO-DATA') key a couple of times
;; helps.
;;
;; Sometimes invidous instance will send empty author thumbnail urls, so they
;; will not be shown.
;;
;; If the first comments page will fail to load, try hitting /g/.
;;
;; Currently, I don't have a solution for these problems.

;;; Code:


;;;; SHOW VIDEO


;;;;; IMPORTS

(require 'files)
(require 'simple)
(require 'subr-x)
(require 'seq)
(require 'shr)
(require 'ytel)


;;;;; TYPES

(cl-defstruct (ytel-show--video (:constructor ytel-show--video-create)
                                (:copier nil))
  "Cached video data."
  title thumbnail-data description published
  length views likes dislikes author-id)

(cl-defstruct (ytel-show--author (:constructor ytel-show--author-create)
                                 (:copier nil))
  "Cached author data."
  name thumbnail-data subs)


;;;;; VARIABLES


;;;;;; INTERNAL

(defvar-local ytel-show--video-ids nil
  "A vector of stored video ids.")

(defvar-local ytel-show--index 0
  "Current video index in `YTEL-SHOW--VIDEO-IDS'.")

(defvar ytel-show--video-fields
  (string-join '("videoId" "title" "videoThumbnails"
                 "description" "published" "lengthSeconds"
                 "viewCount" "likeCount" "dislikeCount"
                 "author" "authorId" "authorThumbnails" "subCountText")
               ",")
  "Requested fields in a query to indidious instance.  Used to save bandwidth.")

(defvar ytel-show--videos-cache (make-hash-table :test 'equal :size 20)
  "Hash-table with cached video data.")

(defvar ytel-show--authors-cache (make-hash-table :test 'equal :size 20)
  "Hash-table with cached author data.")


;;;;;; MAPS

(defvar ytel-show-previous-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ytel-show-previous-video)
    (define-key map (kbd "<mouse-2>") #'ytel-show-previous-video)
    map)
  "Keymap for a button that switches to previous video.")

(defvar ytel-show-next-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ytel-show-next-video)
    (define-key map (kbd "<mouse-2>") #'ytel-show-next-video)
    map)
  "Keymap for a button that switches to next video.")

(defvar ytel-show-close-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'quit-window)
    (define-key map (kbd "<mouse-2>") #'quit-window)
    map)
  "Keymap for a button that closes a window.")

(defvar ytel-show-comments-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ytel-show-comments)
    (define-key map (kbd "<mouse-2>") #'ytel-show-comments)
    map)
  "Keymap for a button that shows video comments.")

(defvar ytel-show-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "n") #'ytel-show-next-video)
    (define-key map (kbd "p") #'ytel-show-previous-video)
    (define-key map (kbd "G") #'ytel-show-reload-video-data)
    (define-key map (kbd "c") #'ytel-show-comments)
    map)
  "Keymap for `YTEL-SHOW-MODE'.")


;;;;;; CUSTOM

(defgroup ytel-show nil
  "View youtube video details from ytel."
  :group 'applications)

(defcustom ytel-show-default-buffer-name "*ytel-show*"
  "Default buffer name used when the `BUFFER' argument in `YTEL-SHOW' is nil."
  :type 'string
  :group 'ytel-show)

(defcustom ytel-show-image-max-width 800.0
  "Max image width.  This variable is used to scale images in buffer."
  :type 'float
  :group 'ytel-show)

(defcustom ytel-show-image-max-height 400.0
  "Max image height.  This variable is used to scale images in buffer."
  :type 'float
  :group 'ytel-show)

(defface ytel-show-video-likes-face
  '((t :inherit font-lock-doc-face))
  "Face used for the video likes and dislikes."
  :group 'ytel-show)

(defface ytel-show-author-subs-face
  '((t :inherit font-lock-string-face))
  "Face used for the author subs."
  :group 'ytel-show)


;;;;; FUNCTIONS

(defun ytel-show--current-video-id ()
  "Get currently showed video id."
  (aref ytel-show--video-ids ytel-show--index))

(defun ytel-show--check-response-error (response)
  "Check if `RESPONSE' is an error and signal an error."
  (if-let ((msg (alist-get 'error response)))
      (error msg)
    response))

(defun ytel-show--query-video (id)
  "Query invidous instance for video data by `ID'."
  (ytel-show--check-response-error
   (ytel--API-call (concat "videos/" id) `(("fields" ,ytel-show--video-fields)))))


;;;;;; THUMBNAILS

(defun ytel-show--repair-thumbnail-urls (thumbnails)
  "Repair broken urls in `THUMBNAILS'.
Some urls might be without \"https:\" or \"https://i.ytimg.com\" prefix.  Detect
these urls and try to repair them."
  (let* ((id (ytel-show--current-video-id))
         (single-slash-rx (rx bos "/vi/" (literal id) "/maxres.jpg" eos)))
    (seq-map
     (lambda (thumbnail)
       (let-alist thumbnail
         (cond ((string-match-p single-slash-rx .url)
                (setf (alist-get 'url thumbnail) (concat "https://i.ytimg.com" .url)))
               ((string-match-p (rx bos "//yt3.ggpht.com") .url)
                (setf (alist-get 'url thumbnail) (concat "https:" .url)))))
       thumbnail)
     thumbnails)))

(defun ytel-show--filter-thumbnails (thumbnails)
  "Filter `THUMBNAILS' from invidious response.
`THUMBNAILS' is a vector of alists with keys: url, width and height.  Return a
sequence with alists with the following properties:

  url

    non empty string that tharts with \"https?://\" and does not includes
    `YTEL-INVIDIOUS-API-URL'

  width and height

    number greater than 0"
  (seq-filter
   (lambda (thumbnail)
     (let-alist thumbnail
       (and (numberp .width) (< 0 .width)
            (numberp .height) (< 0 .height)
            (stringp .url) (not (string-empty-p .url))
            (string-match-p (rx bos "http" (? "s") "://") .url)
            (not (string-match-p (regexp-quote ytel-invidious-api-url) .url)))))
   thumbnails))

(defun ytel-show--retrieve-image (url)
  "Synchronously retrieve image from `URL'.
On 404 return :not-found."
  (let ((buffer (url-retrieve-synchronously url)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char (point-min))
          (if (search-forward "HTTP/1.1 404" nil t)
              :not-found
            (search-forward "\n\n")
            (buffer-substring (point) (point-max))))
      (kill-buffer buffer))))

(defun ytel-show--valid-image-sizes-p (width height)
  "Check whether `WIDTH' and `HEIGHT' are smaller than max allowed values."
  (or (< ytel-show-image-max-width width) (< ytel-show-image-max-height height)))

(defun ytel-show--scale-images-sizes (width height)
  "Scale `WIDTH' and `HEIGHT' to fit max allowed values."
  (min (/ ytel-show-image-max-width width) (/ ytel-show-image-max-height height)))

(defun ytel-show--thumbnail-load-data (thumbnail)
  "Load image data from `THUMBNAIL's url.
Scale it to satisfy `YTEL-SHOW-IMAGE-MAX-WIDTH' and
`YTEL-SHOW-IMAGE-MAX-HEIGHT'."
  (when thumbnail
    (let-alist thumbnail
      (let ((data (ytel-show--retrieve-image .url)))
        (cond ((eq :not-found data) data)
              ((ytel-show--valid-image-sizes-p .width .height)
               (create-image
                data nil t :scale (ytel-show--scale-images-sizes .width .height)))
              (t (create-image data nil t)))))))

(defun ytel-show--select-thumbnail (thumbnails)
  "Select thumbnail from `THUMBNAILS' with the biggest height."
  (seq-reduce
   (lambda (thumbnail next)
     (if (<= (alist-get 'height thumbnail) (alist-get 'height next))
         next
       thumbnail))
   (seq-subseq thumbnails 1) (elt thumbnails 0)))

(defun ytel-show--delete-thumbnail (thumbnail thumbnails)
  "Delete element from `THUMBNAILS' matching `THUMBNAIL's url."
  (cl-delete (alist-get 'url thumbnail) thumbnails
             :test #'string-equal
             :key (apply-partially #'alist-get 'url)))

(defun ytel-show--get-thumbnail (thumbnails)
  "Select thumbnail from `THUMBNAILS' and load it's data."
  (cl-case (length thumbnails)
    (0 nil)
    (1 (elt thumbnails 0))
    (t (let* ((thumbnail (ytel-show--select-thumbnail thumbnails))
              (data (ytel-show--thumbnail-load-data thumbnail)))
         (if (and (not (eq :not-found data)) data) data
           (ytel-show--get-thumbnail
            (ytel-show--delete-thumbnail thumbnail thumbnails)))))))

(defun ytel-show--process-thumbnails (thumbnails)
  "Repair `THUMBNAILS' | filter | select | load."
  (thread-last thumbnails
    ytel-show--repair-thumbnail-urls
    ytel-show--filter-thumbnails
    ytel-show--get-thumbnail))


;;;;;; UPDATE

(defun ytel-show--update-video (video query-response &optional recache-images)
  "Update `VIDEO' struct with the data from `QUERY-RESPONSE'.
If `RECACHE-IMAGES' is non-nil, redownload thumbnails.  Return updated `VIDEO'."
  (let-alist query-response
    (when (or recache-images (null (ytel-show--video-thumbnail-data video)))
      (when-let ((thumbnail (ytel-show--process-thumbnails .videoThumbnails)))
        (setf (ytel-show--video-thumbnail-data video) thumbnail)))

    (when .title (setf (ytel-show--video-title video) .title))
    (when .description (setf (ytel-show--video-description video) .description))
    (when .published (setf (ytel-show--video-published video) .published))
    (when .lengthSeconds (setf (ytel-show--video-length video) .lengthSeconds))
    (when .viewCount (setf (ytel-show--video-views video) .viewCount))
    (when .likeCount (setf (ytel-show--video-likes video) .likeCount))
    (when .dislikeCount (setf (ytel-show--video-dislikes video) .dislikeCount))
    (when .authorId (setf (ytel-show--video-author-id video) .authorId)))
  video)

(defun ytel-show--update-author (author query-response &optional recache-images)
  "Update `AUTHOR' struct with the data from `QUERY-RESPONSE'.
If `RECACHE-IMAGES' is supplied, redownload thumbnails.  Return updated `AUTHOR'."
  (let-alist query-response
    (when (or recache-images (null (ytel-show--author-thumbnail-data author)))
     (when-let ((thumbnail (ytel-show--process-thumbnails .authorThumbnails)))
      (setf (ytel-show--author-thumbnail-data author) thumbnail)))

    (when .author (setf (ytel-show--author-name author) .author))
    (when .subCountText (setf (ytel-show--author-subs author) .subCountText)))
  author)

(defun ytel-show--update-cache (id &optional recache-images)
  "Update cached data by video `ID'.
If `RECACHE-IMAGES' is non-nil, redownlaod thumbnails.  Return an alist with the
following keys:

  id - video `ID'

  video - video struct with cached data

  author - author struct with cached data"
  (let* ((query-response (ytel-show--query-video id))
         (video (ytel-show--update-video
                 (or (gethash id ytel-show--videos-cache)
                     (puthash id (ytel-show--video-create)
                              ytel-show--videos-cache))
                 query-response recache-images))
         (author-id (ytel-show--video-author-id video))
         (author (ytel-show--update-author
                  (or (gethash author-id ytel-show--authors-cache)
                      (puthash author-id (ytel-show--author-create)
                               ytel-show--authors-cache))
                  query-response recache-images)))
    `((id . ,id) (video . ,video) (author . ,author))))

(defun ytel-show--update-index (add &optional cycle)
  "Update `YTEL-SHOW--INDEX' by adding `ADD' to it.
If `CYCLE' is non-nil then the addition is performed with modulus."
  (let ((new-index (+ add ytel-show--index))
        (length (length ytel-show--video-ids)))
    (cond
     (cycle
      (setq ytel-show--index (mod new-index length)))
     ((<= 0 new-index (1- length))
      (setq ytel-show--index new-index)))))


;;;;;; DATA

(defun ytel-show--video-data (id)
  "Get cached data by video `ID'.
If the video is not cached, then query the invidious instance.  Return value is
described in `YTEL-SHOW--UPDATE-CACHE'."
  (if-let* ((video (gethash id ytel-show--videos-cache))
            (author (gethash (ytel-show--video-author-id video)
                             ytel-show--authors-cache)))
      `((id . ,id) (video . ,video) (author . ,author))
    (ytel-show--update-cache id)))


;;;;;; DRAW

(defun ytel-show--draw-url (url title)
  "Draw `URL' using shr with `TITLE' at current point."
  (let ((point (point)))
    (insert (format "%s" title))
    (make-text-button
     point (point)
     'follow-link t 'mouse-face 'highlight 'shr-url url 'keymap shr-map)))

(defun ytel-show--draw-button (title map &rest props)
  "Draw a `TITLE' button with `MAP' and `PROPS'."
  (let ((point (point)))
    (insert title)
    (apply #'make-text-button
           point (point)
           'follow-link t
           'mouse-face 'highlight
           'keymap map
           props)))

(defun ytel-show--format-video-likes-dislikes (likes &optional dislikes)
  "Format `LIKES' and `DISLIKES' before inserting them to buffer."
  (let ((s (concat (format "[likes:%s" likes)
                   (when dislikes (format "/%s" dislikes))
                   "]")))
    (propertize s 'face 'ytel-show-video-likes-face)))

(defun ytel-show--format-author-subs (subs)
  "Format `SUBS' before inserting them to buffer."
  (propertize (format "(%s)" subs) 'face 'ytel-show-author-subs-face))

(defun ytel-show--draw-data (data)
  "Draw video `DATA' to the buffer.
`DATA' is the value returned from `YTEL-SHOW--VIDEO-DATA'."
  (let-alist data
    (ytel-show--draw-button "Previous" ytel-show-previous-button-map)
    (insert "  -  ")
    (ytel-show--draw-button "Next" ytel-show-next-button-map)
    (insert "\n")

    (ytel-show--draw-url
     (concat "https://www.youtube.com/watch?v=" .id)
     (or (ytel-show--video-title .video) "no title"))
    (insert "  -  ")

    (when-let ((name (ytel-show--author-name .author))
               (author-id (ytel-show--video-author-id .video)))
      (ytel-show--draw-url (concat "https://www.youtube.com/channel/" author-id) name)
      (insert " "))

    (when-let ((subs (ytel-show--author-subs .author)))
      (insert (ytel-show--format-author-subs subs) "\n"))

    (when-let ((length (ytel-show--video-length .video)))
      (insert (ytel--format-video-length length) "  -  "))

    (when-let ((published (ytel-show--video-published .video)))
      (insert (ytel--format-video-published published) "  -  "))

    (when-let ((views (ytel-show--video-views .video)))
      (insert (ytel--format-video-views views) "  -  "))

    (when-let ((likes (ytel-show--video-likes .video))
               (dislikes (ytel-show--video-dislikes .video)))
      (insert (ytel-show--format-video-likes-dislikes likes dislikes) "  -  "))

    (ytel-show--draw-button "View comments" ytel-show-comments-button-map)
    (insert "  -  ")
    (ytel-show--draw-button "Close" ytel-show-close-button-map)
    (insert "\n")

    (when-let ((thumbnail-data (ytel-show--video-thumbnail-data .video)))
      (insert-image thumbnail-data) (insert "\n"))

    (when-let ((description (ytel-show--video-description .video)))
      (insert description "\n"))

    (when-let ((author-thumbnail-data (ytel-show--author-thumbnail-data .author)))
      (insert-image author-thumbnail-data) (insert "\n"))))


;;;;; COMMANDS

(defun ytel-show-revert-buffer (&rest _)
  "Revert current Ytel-Show buffer.
Erase the buffer and redraw current video's data."
  (interactive)
  (let ((inhibit-read-only t)
        (id (ytel-show--current-video-id)))
    (message "Loading %s..." id)
    (erase-buffer)
    (ytel-show--draw-data (ytel-show--video-data id))
    (goto-char (point-min))
    (message "Showing %s..." id)))

(defun ytel-show-next-video (&optional cycle)
  "Switch to the next video in `YTEL-SHOW--VIDEO-IDS' and draw it.
With overflow, cycle if `CYCLE' is provided."
  (interactive "P")
  (let ((inx ytel-show--index))
    (ytel-show--update-index 1 cycle)
    (if (= inx ytel-show--index)
        (message "Wall! Use argument to cycle!")
      (ytel-show-revert-buffer))))

(defun ytel-show-previous-video (&optional cycle)
  "Switch to the previous video in `YTEL-SHOW--VIDEO-IDS' and draw it.
With overflow, cycle if `CYCLE' is provided."
  (interactive "P")
  (let ((inx ytel-show--index))
    (ytel-show--update-index -1 cycle)
    (if (= inx ytel-show--index)
        (message "Wall! Use argument to cycle!")
      (ytel-show-revert-buffer))))

(defun ytel-show-reload-video-data (&optional recache-images)
  "Recache current video's data and redraw the current buffer.
If `RECACHE-IMAGES' is non-nil, redownload thumbnails."
  (interactive "P")
  (message "Reloading %s..." (ytel-show--current-video-id))
  (ytel-show--update-cache (ytel-show--current-video-id) recache-images)
  (ytel-show-revert-buffer))

;;;###autoload
(defun ytel-show (video-ids index &optional buffer)
  "Show video information in the Ytel-Show buffer.
This is the main entry function for this package.  Interactively, it must be
called from a `YTEL' buffer.

`VIDEO-IDS' is a vector of strings with youtube video ids to show.

`INDEX' is the index of some video in `VIDEO-IDS' that will be cached and
displayed first.

If `BUFFER' is nil, then `YTEL-SHOW-DEFAULT-BUFFER-NAME' is used.  To display
videos from `VIDEO-IDS'."
  (interactive (if (not (derived-mode-p 'ytel-mode))
                   (error "Not in the ytel buffer")
                 (list (vconcat (seq-map #'ytel-video-id ytel-videos))
                       (1- (line-number-at-pos)))))

  (unless (and (vectorp video-ids)
               (seq-every-p (lambda (s) (and (stringp s) (= 11 (length s)))) video-ids)
               (integerp index) (<= 0 index (1- (length video-ids))))
    (error "Invalid arguments to `YTEL-SHOW'"))

  (switch-to-buffer
   (get-buffer-create (or buffer ytel-show-default-buffer-name)))

  (unless (derived-mode-p 'ytel-show-mode)
    (ytel-show-mode))
  (setq-local ytel-show--video-ids video-ids ytel-show--index index)
  (ytel-show-revert-buffer))

(define-derived-mode ytel-show-mode special-mode "Ytel-Show"
  "Mode for displaying youtube video information."
  :group 'ytel-show
  (setq-local revert-buffer-function #'ytel-show-revert-buffer))

(provide 'ytel-show)


;;;; SHOW COMMENTS


;;;;; TYPES

(cl-defstruct (ytel-show-comments--comment
               (:constructor ytel-show-comments--comment-create)
               (:copier nil))
  author author-id content published likes ownerp heart replies)


;;;;; VARIABLES


;;;;;; INTERNAL

(defvar ytel-show-comments--authors-cache
  (make-hash-table :test 'equal :size 100 :rehash-size 2.5)
  "Cache used to store author thumbnails of comments.")


;;;;;;; LOCAL

(defvar-local ytel-show-comments--video-id nil
  "Video id used it `YTEL-SHOW-COMMENTS-MODE'.")

(defvar-local ytel-show-comments--video-title "video"
  "Title used at the top of the buffer.")

(defvar-local ytel-show-comments--history nil
  "Stored pages of comments.")

(defvar-local ytel-show-comments--comment-count nil
  "Communt count.")


;;;;;; MAPS

(defvar ytel-show-comments-continuation-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ytel-show-comments-follow-continuation)
    (define-key map (kbd "<mouse-2>") #'ytel-show-comments-follow-continuation)
    map)
  "Map used in continuation buttons.")

(defvar ytel-show-comments-previous-page-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'ytel-show-comments-previous-page)
    (define-key map (kbd "<mouse-2>") #'ytel-show-comments-previous-page)
    map)
  "Map used in to switch to previous page.")

(defvar ytel-show-comments-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'ytel-show-comments-previous-page)
    map)
  "Map used by `YTEL-SHOW-COMMENTS-MODE'.")


;;;;;; CUSTOM

(defcustom ytel-show-comments-default-buffer-name "*ytel-show-comments*"
  "Default buffer name for `YTEL-SHOW-COMMENTS-MODE'."
  :type 'string
  :group 'ytel-show)

(defface ytel-show-comments-owner-face
  '((t :inherit hl-line))
  "Face used for owner's text."
  :group 'ytel-show)


;;;;; FUNCTIONS

(defun ytel-show-comments--query-comments (id &optional continuation)
  "Query invidious instance for comments of video identified by `ID'.
If `CONTINUATION' is supplied, show continuation of comments."
  (ytel-show--check-response-error
   (ytel--API-call
    (concat "comments/" id)
    (when continuation `(("continuation" ,continuation))))))


;;;;;; UPDATE

(defun ytel-show-comments--parse-comment (response)
  "Parse comment from `RESPONSE'.
Return comment on success, or nil."
  (when response
    (let-alist response
      (when (and .authorId (null (gethash .authorId ytel-show-comments--authors-cache)))
        (when-let ((ytel-show--video-ids (vector ytel-show-comments--video-id))
                   (thumbnail (ytel-show--process-thumbnails .authorThumbnails)))
          (puthash .authorId thumbnail ytel-show-comments--authors-cache)))

      (ytel-show-comments--comment-create
       :author .author
       :author-id .authorId
       :content .content
       :published .published
       :likes .likeCount
       :ownerp (when .authorIsChannelOwner (not (eq :json-false .authorIsChannelOwner)))
       :heart .creatorHeart.creatorName
       :replies (when .replies (cons .replies.replyCount .replies.continuation))))))

(defun ytel-show-comments--add-to-history (id &optional continuation)
  "Add new page to comments history.
Comments are taken frome video `ID' and it's `CONTINUATION' (if supplied).
Return '(comments . continuation) on success, or nil.  Comments is a sequence of
`ytel-show-comments--comment' struct."
  (when-let ((response (ytel-show-comments--query-comments id continuation)))
    (let-alist response
      (when .commentCount
        (setf ytel-show-comments--comment-count .commentCount))
      (when-let ((comments (seq-map #'ytel-show-comments--parse-comment .comments)))
        (car (push (cons comments .continuation) ytel-show-comments--history))))))


;;;;;; DATA

(defun ytel-show-comments--comments-data (id)
  "Get comment data by `ID' or download first page of videos comments.
Return value is described in `YTEL-SHOW-COMMENTS--ADD-TO-HISTORY'."
  (or (car ytel-show-comments--history)
      (ytel-show-comments--add-to-history id)))


;;;;;; DRAW

(defun ytel-show-comments--format-owner (owner)
  "Format `OWNER's text."
  (propertize owner 'face 'ytel-show-comments-owner-face))

(defun ytel-show-comments--draw (data)
  "Draw comment `DATA' on buffer.
`DATA' is a return value of `YTEL-SHOW-COMMENTS--COMMENTS-DATA'."
  (when data
    (cl-destructuring-bind (comments . continuation) data

      (ytel-show--draw-url
       (concat "https://www.youtube.com/watch?v=" ytel-show-comments--video-id)
       ytel-show-comments--video-title)

      (when ytel-show-comments--comment-count
        (insert (format " (%s comments)\n" ytel-show-comments--comment-count)))

      (let ((length (length ytel-show-comments--history)))
        (insert (format "Page %s  -  " length))
        (ytel-show--draw-button
         (if (= 1 length) "Reload" "Go back")
         ytel-show-comments-previous-page-button-map)
        (insert "  -  "))

      (ytel-show--draw-button "Close" ytel-show-close-button-map)
      (insert "\n\n")

      (seq-doseq (comment comments)
        (when-let* ((author-id (ytel-show-comments--comment-author-id comment))
                    (thumbnail (gethash author-id ytel-show-comments--authors-cache)))
          (insert-image thumbnail))

        (when-let ((name (ytel-show-comments--comment-author comment))
                   (author-id (ytel-show-comments--comment-author-id comment)))
          (insert " ")
          (ytel-show--draw-url (concat "https://www.youtube.com/channel/" author-id) name))

        (when-let ((published (ytel-show-comments--comment-published comment)))
          (insert "  -  " (ytel--format-video-published published)))

        (when-let ((likes (ytel-show-comments--comment-likes comment)))
          (when (< 0 likes)
            (insert "  -  " (ytel-show--format-video-likes-dislikes likes))))

        (when-let ((ownerp (ytel-show-comments--comment-ownerp comment)))
          (insert "  -  " (ytel-show-comments--format-owner "OWNER")))

        (insert "\n")

        (when-let ((content (ytel-show-comments--comment-content comment)))
          (insert content "\n\n"))

        (when-let ((heart (ytel-show-comments--comment-heart comment)))
          (insert (ytel-show-comments--format-owner
                   (concat heart " likes this comment"))
                  "\n\n"))

        (when-let ((replies (ytel-show-comments--comment-replies comment)))
          (cl-destructuring-bind (count . continuation) replies
            (when continuation
              (ytel-show--draw-button
               (format "View %s replies" count)
               ytel-show-comments-continuation-map
               'continuation continuation)
              (insert "\n\n"))))

        "\n")

      (when continuation
        (ytel-show--draw-button "View next page"
                                ytel-show-comments-continuation-map
                                'continuation continuation)
        (insert "\n")))))


;;;;; COMMANDS

(defun ytel-show-comments-follow-continuation (continuation)
  "Follow `CONTINUATION'.
Add new page to history and load it.  This is used in buttons."
  (interactive
   (progn
     (mouse-set-point last-nonmenu-event)
     (if-let ((continuation (get-text-property (point) 'continuation)))
         (list continuation)
       (error "No continuation at point"))))
  (message "Loading next page")
  (ytel-show-comments--add-to-history ytel-show-comments--video-id continuation)
  (ytel-show-comments-revert-buffer))

(defun ytel-show-comments-previous-page ()
  "Go to previous page in comments history."
  (interactive)
  (message "Loading previous page")
  (pop ytel-show-comments--history)
  (ytel-show-comments-revert-buffer))

(defun ytel-show-comments-revert-buffer (&rest _)
  "Revert current ytel-show-comments buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (message "Loading %s..." ytel-show-comments--video-id)
    (erase-buffer)
    (ytel-show-comments--draw
     (ytel-show-comments--comments-data ytel-show-comments--video-id))
    (goto-char (point-min))
    (message "Showing %s..." ytel-show-comments--video-id)))

(defun ytel-show-comments (id &optional title buffer)
  "Entry function to show video `ID's comments.
`TITLE' will be shown at the top of the buffer.  `BUFFER' is a name of the
buffer the will be created.  If called interactively, these values are taken
automatically from ytel-show buffer."
  (interactive
   (if (not (derived-mode-p 'ytel-show-mode))
       (error "Not in the ytel-show buffer")
     (let ((id (ytel-show--current-video-id)))
       (list id (ytel-show--video-title (gethash id ytel-show--videos-cache))))))

  (switch-to-buffer
   (get-buffer-create (or buffer ytel-show-comments-default-buffer-name)))

  (unless (derived-mode-p 'ytel-show-comments-mode)
    (ytel-show-comments-mode))

  (unless (string= ytel-show-comments--video-id id)
    (when title (setq-local ytel-show-comments--video-title title))
    (setq-local ytel-show-comments--video-id id ytel-show-comments--history nil))
  (ytel-show-comments-revert-buffer))

(define-derived-mode ytel-show-comments-mode special-mode "Ytel-Show-Comments"
  "Mode for displaying youtube video comments."
  :group 'ytel-show
  (setq-local revert-buffer-function #'ytel-show-comments-revert-buffer))

;;; ytel-show.el ends here
