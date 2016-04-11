;; -*- lexical-binding: t -*-

(require 'cl)
(require 'request)

(defvar pizza-endpoint nil)
(setf pizza-endpoint "https://pizza.de")

;; /restaurants/?delivery_area=00000

(defvar pizza-search-mode-map nil)
(setf pizza-search-mode-map (let ((map (make-sparse-keymap)))
                              (define-key map (kbd "n") 'pizza-mode-next-item)
                              (define-key map (kbd "p") 'pizza-mode-prev-item)
                              (define-key map (kbd "RET") 'pizza-mode-item-action)
                              map))

(defun pizza-search-buffer (delivery-area)
  (get-buffer-create (concat "*Pizza near " delivery-area "*")))

(defun pizza-restaurant-buffer (restaurant-name)
  (get-buffer-create (concat "*Pizza at " restaurant-name "*")))

(defun pizza-search (delivery-area)
  (interactive (list (completing-read "Search: " '() nil nil)))
  (switch-to-buffer (pizza-search-buffer delivery-area))
  (let ((buffer (current-buffer)))
    (delete-region (point-min) (point-max))
    (insert "Looking up restaurants around " delivery-area)
    (request (concat pizza-endpoint "/restaurants/?delivery_area=" delivery-area)
             :type "GET"
             :parser 'json-read
             :success (function* (lambda (&key data &allow-other-keys)
                                   (with-current-buffer buffer
                                     (delete-region (point-min) (point-max))

                                     (pizza-insert-restaurants data delivery-area)
                                     ))))
    (use-local-map pizza-search-mode-map)
    ))

(defun pizza-insert-restaurants (data delivery-area)
  (let ((pagination (assoc 'pagination data))
        (restaurants (cdr (assoc 'data data))))
    (insert (format "Found %s restaurants around %s" (cdr (assoc 'count pagination)) delivery-area) "\n")
    (loop
     for restaurant across restaurants
     for name = (decode-coding-string (cdr (assoc 'name restaurant)) 'utf-8)
     for id =  (cdr (assoc 'id restaurant))
     do (insert (propertize name
                            'pizza-restaurant-id id
                            'pizza-restaurant-name name))
     do (insert "\n"))
    (insert (format "%s" data))))

(defun pizza-mode-item-action ()
  (interactive)
  (if (get-text-property (point) 'pizza-restaurant-id)
      (pizza-restaurant-mode (get-text-property (point) 'pizza-restaurant-id)
                             (get-text-property (point) 'pizza-restaurant-name)
                             )))

(defun pizza-restaurant-mode (restaurant-id restaurant-name)
  (switch-to-buffer (pizza-restaurant-buffer restaurant-name))
  (let ((buffer (current-buffer)))
    (delete-region (point-min) (point-max))
    (insert "Looking up menu at " restaurant-name)
    (request (concat pizza-endpoint "/restaurants/" restaurant-id "/")
             :type "GET"
             :parser 'json-read
             :success (function* (lambda (&key data &allow-other-keys)
                                   (with-current-buffer buffer
                                     (delete-region (point-min) (point-max))
                                     (pizza-insert-restaurant data)
                                     ;(insert (format "%s" data))
                                     ))))))

(defun pizza-insert-restaurant (restaurant)
  (insert (decode-coding-string (cdr (assoc 'name restaurant)) 'utf-8) "\n")
  (let ((menu (cdr (assoc 'menu restaurant))))
    (loop
     for section across (cdr (assoc 'sections menu))
     do (insert (decode-coding-string (cdr (assoc 'name section)) 'utf-8) "\n")
     do (loop for item across (cdr (assoc 'items section))
              for sizes = (cdr (assoc 'sizes item))
              do (insert "  "
                         (decode-coding-string (cdr (assoc 'name item)) 'utf-8)
                         " - ")
              do (if (= 1 (length sizes))
                     (insert (format "%s" (cdr (assoc 'price (aref sizes 0)))))
                   (loop for size across sizes
                         do (insert "%s %s" (cdr (assoc 'price size 0)) (cdr (assoc 'name sie)))))
              do (insert "\n")))))
