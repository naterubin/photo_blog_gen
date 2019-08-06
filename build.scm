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

(define-record-type <photo>
  (make-photo title path)
  photo?
  (title photo-title)
  (path  photo-path))

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
  (let ((photos (list-dir albumdir)))
    (map (lambda (photo) (make-photo (string-drop-right (kebab-to-title photo) 4) (p/ albumdir photo))) photos)))

(define (albums dirname)
  (let* ((dots (string->char-set "."))
	 (no-dots-or-static
	  (lambda (dir)
	    (and
	     (not (char-set= dots (string->char-set dir)))
	     (not (equal? dir "static")))))
	 (dirs (filter no-dots-or-static (list-dir dirname 'directory))))
    (map (lambda (dir) (make-album (kebab-to-title dir) (photos (p/ dirname dir)))) dirs)))

(define (base-template content)
  `(html
    (head (title "n.s.s.r.")
	  (link (@ (href "https://fonts.googleapis.com/css?family=Libre+Baskerville")
	           (rel "stylesheet")))
	  (link (@ (href "/static/css/stylesheet.css") (rel "stylesheet"))))
    (body
     (div (@ (id "container"))
	  (div (@ (id "site-title"))
	       (h1 (a (@ (href "/")) "n.s.s.r."))
	       (p "Blah blah blah")
	       (p "&#9398;&#9402; 2019 "
		  (a (@ (href "http://naterubin.com"))Nate Rubin)))
	  (div (@ (id "content")) ,content)))))

(define (album-list-page albums)
  (base-template
   `(div (@ (clas "gallery-list"))
     ,(map
       (lambda (alb)
	 `(div (@ (class "gallery-listing"))
	       (a (@ (href ,(album-page-name alb)))
		  (h2 (@ (class "gallery-title")) ,(album-title alb)))
	       (p "")
	       (img (@ (src ,(photo-medium-path (car (album-photos alb))))))))
       albums))))

(define (album-page album)
  (base-template
   `(div (@ (id "gallery-block"))
	 (h1 (@ (id "gallery-title")) ,(album-title album))
	 (div (@ (id "gallery"))
	      (div
	       ,(map
		 (lambda (ph)
		   `(div (@ (class "responsive-thumbnail"))
			 (div
			  (a (@ (href ,(photo-page-name ph)))
			     (img (@ (src ,(photo-thumbnail-path ph))
				     (style "width: 100%; height: auto; box-sizing: border-box;")))))))
		 (album-photos album)))))))

(define (photo-page photo)
  (base-template
   `(a (@ (href ,(photo-fullsize-path photo)))
       (img (@ (src ,(photo-medium-path photo)))))))

(define (album-page-name album)
  (string-join (list (string-downcase (album-title album)) "html") "."))

(define (photo-page-name photo)
  (string-join (list (string-downcase (photo-title photo)) "html") "."))

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
