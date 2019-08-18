(define-record-type <photo>
  (make-photo title path prev next)
  photo?
  (title photo-title)
  (path  photo-path)
  (prev  photo-prev set-photo-prev)
  (next  photo-next set-photo-next))

(define (photo-resize-path photo subtitle)
  (p/ "photos" (string-join (list (photo-title photo) subtitle) "_")))

(define (photo-fullsize-path photo)
  (photo-resize-path photo "fullsize.jpg"))

(define (photo-medium-path photo)
  (photo-resize-path photo "medium.jpg"))

(define (photo-thumbnail-path photo)
  (photo-resize-path photo "thumbnail.jpg"))

(define-record-type <album>
  (make-album title photos)
  album?
  (title  album-title)
  (photos album-photos))

(define (album-page-name album)
  (string-join (list (string-downcase (album-title album)) "html") "."))

(define (photo-page-name photo)
  (string-join (list (string-downcase (photo-title photo)) "html") "."))
