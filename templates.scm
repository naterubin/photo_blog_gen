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
   `(div (@ (id "photo-page-content"))
     (a (@ (href ,(photo-fullsize-path photo)) (id "block-image"))
	(img (@ (src ,(photo-medium-path photo)))))
     (div (@ (id "prev"))
	  ,(if (not (null? (photo-prev photo)))
	       `(a (@ (href ,(photo-page-name (photo-prev photo)))) "Prev")
	       '()))
     (div (@ (id "next"))
	  ,(if (not (null? (photo-next photo)))
	       `(a (@ (href ,(photo-page-name (photo-next photo)))) "Next")
	       '())))))
