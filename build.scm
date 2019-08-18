(use-modules (srfi srfi-9)
	     (sxml simple))

(define-syntax p/
  (syntax-rules (~)
    ((_ ~ i ...) (string-join (list (passwd:dir (getpwnam (getlogin))) i ...) file-name-separator-string))
    ((_ i ...) (string-join (list i ...) file-name-separator-string))))

(load-extension (p/ (getcwd) "img_convert") "init_convert")

(define (rows-from-columns old-columns old-rows new-columns)
  (floor (/ (* old-rows new-columns) old-columns)))

(define (columns-from-rows old-columns old-rows new-rows)
  (floor (/ (* old-columns new-rows) old-rows)))

(define* (proportional-resize in out #:key (cols #f) (rows #f))
  (let* ((size (img-size in))
	 (width (car size))
	 (height (cadr size)))
    (cond
     ((not (or cols rows)) #f)
     ((not cols) (resize in out (columns-from-rows width height rows) rows))
     ((not rows) (resize in out cols (rows-from-columns width height cols))))))

(include "models.scm")

(define* (list-dir dir-path #:optional (stat-type 'regular))
  (let ((dir (opendir dir-path))
	(entries '()))
    (do ((entry (readdir dir) (readdir dir)))
	((eof-object? entry))
      (if (eq? (stat:type (stat (p/ dir-path entry))) stat-type)
	  (set! entries (cons entry entries))))
    (closedir dir)
    entries))

(define (kebab-to-title str)
  (let* ((cs (char-set-delete char-set:graphic #\-))
	(tokenized (string-tokenize str cs))
	(spaced (string-join tokenized " ")))
    (string-titlecase spaced)))

(define (photos albumdir)
  (define prev '())
  (let* ((photo-paths (list-dir albumdir))
	 (photos (map
		  (lambda (photo)
		    (make-photo
		     (string-drop-right (kebab-to-title photo) 4)
		     (p/ albumdir photo)
		     '() '()))
		    photo-paths)))
    (for-each (lambda (photo)
		(set-photo-prev photo prev)
		(if (not (null? prev))
		    (set-photo-next prev photo))
		(set! prev photo))
	      photos)
    photos))

(define (albums dirname)
  (let* ((dots (string->char-set "."))
	 (no-dots-or-static
	  (lambda (dir)
	    (and
	     (not (char-set= dots (string->char-set dir)))
	     (not (equal? dir "static")))))
	 (dirs (filter no-dots-or-static (list-dir dirname 'directory))))
    (map (lambda (dir) (make-album (kebab-to-title dir) (photos (p/ dirname dir)))) dirs)))


(include "templates.scm")

(define (main)
  (let ((albs (albums "source")))
    (system "rm -rf output")
    (mkdir "output")
    (mkdir "output/photos")
    (system "cp -R source/static output/static")
    (call-with-output-file (p/ "output" "index.html")
      (lambda (port)
	(sxml->xml (album-list-page albs) port)))
    (map (lambda (alb)
	   (call-with-output-file (p/ "output" (album-page-name alb))
	     (lambda (port)
	       (sxml->xml (album-page alb) port)))
	   (map (lambda (ph)
		  (proportional-resize (photo-path ph) (p/ "output" (photo-fullsize-path ph)) #:cols 1000)
		  (proportional-resize (photo-path ph) (p/ "output" (photo-medium-path ph)) #:cols 600)
		  (proportional-resize (photo-path ph) (p/ "output" (photo-thumbnail-path ph)) #:cols 200)
		  (call-with-output-file (p/ "output" (photo-page-name ph))
		    (lambda (port)
		      (sxml->xml (photo-page ph) port))))
		(album-photos alb)))
	 albs)))

(main)
