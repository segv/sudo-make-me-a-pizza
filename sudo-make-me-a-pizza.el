;; -*- lexical-binding: t -*-

(require 'cl)
(require 'request)

(defvar pizza-endpoint nil)
(setf pizza-endpoint "https://pizza.de")

;; /restaurants/?delivery_area=00000

(defvar pizza-search-mode-map nil)
(setf pizza-search-mode-map (let ((map (make-sparse-keymap)))
                              (define-key map (kbd "RET") 'pizza-mode-goto-restaurant)
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
    ;;(insert (format "%s" data))
    (goto-char (point-min))))

(defun pizza-mode-goto-restaurant ()
  (interactive)
  (pizza-restaurant-mode (get-text-property (point) 'pizza-restaurant-id)
                         (get-text-property (point) 'pizza-restaurant-name)))

(defvar pizza-restaurant-mode-map nil)
(setf pizza-restaurant-mode-map (let ((map (make-sparse-keymap)))
                                  (define-key map (kbd "+") 'pizza-restaurant-mode-add-item)
                                  (define-key map (kbd "-") 'pizza-restaurant-mode-remove-item)
                                  (define-key map (kbd "g") 'pizza-restaurant-mode-refresh-list)
                                  (define-key map (kbd "TAB") 'widget-forward)
                                  (define-key map (kbd "C-TAB") 'widget-backward)
                                  map))

(defun pizza-restaurant-mode (restaurant-id restaurant-name)
  (switch-to-buffer (pizza-restaurant-buffer restaurant-name))
  (let ((buffer (current-buffer)))
    (delete-region (point-min) (point-max))
    (widget-insert "Looking up menu at " restaurant-name)
    (use-local-map pizza-restaurant-mode-map)
    (request (concat pizza-endpoint "/restaurants/" restaurant-id "/")
             :type "GET"
             :parser 'json-read
             :success (function* (lambda (&key data &allow-other-keys)
                                   (with-current-buffer buffer
                                     (delete-region (point-min) (point-max))
                                     (pizza-insert-restaurant data)
                                     ;;(insert (format "%s" data))
                                     ))))))
(defvar pizza-restaurant-mode-order nil)
(make-variable-buffer-local 'pizza-restaurant-mode-order)

(defvar pizza-restaurant-mode-restaurant nil)
(make-variable-buffer-local 'pizza-restaurant-mode-restaurant)

(defun pizza-insert-restaurant (restaurant)
  (setf pizza-restaurant-mode-order '((items . ()))
        pizza-restaurant-mode-restaurant restaurant)
  (insert (decode-coding-string (cdr (assoc 'name restaurant)) 'utf-8))
  (insert "\n")
  (insert "Delivery Address:\n")
  (let ((menu (cdr (assoc 'menu restaurant))))
    (loop
     for section across (cdr (assoc 'sections menu))
     do (insert (decode-coding-string (cdr (assoc 'name section)) 'utf-8) "\n")
     do (loop for item across (cdr (assoc 'items section))
              do (pizza-restaurant-mode-insert-item item))
     finally (goto-char (point-min)))))

(defun pizza-restaurant-mode-insert-item (item)
  (let ((sizes (cdr (assoc 'sizes item))))

    (loop for size across sizes
          for start = (point)
          do (insert "    ")
          do (set-text-properties start
                                  (point)
                                  (list 'pizza-restaurant-mode-menu-item-quantity-marker t))
          do (insert (decode-coding-string (cdr (assoc 'name item)) 'utf-8)
                     " - ")
          do (when (< 1 (length sizes))
               (insert "%s " (cdr (assoc 'name size))))
          do (insert (format "%s" (cdr (assoc 'price size))))
          do (set-text-properties start (point) (list 'pizza-restaurant-mode-menu-item
                                                      (list (cons 'item item)
                                                            (cons 'size size)
                                                            (cons 'quantity 0)
                                                            (cons 'start-point start))))
          do (insert "\n"))))

(defun pizza-restaurant-mode-add-item (&optional delta)
  (interactive (list 1))
  (let ((item (get-text-property (point) 'pizza-restaurant-mode-menu-item)))
    (if item
        (progn
          (incf (cdr (assoc 'quantity item)) delta)
          (goto-char (cdr (assoc 'start-point item)))
          (delete-region (point) (next-single-property-change (point) 'pizza-restaurant-mode-menu-item-quantity-marker))
          (insert (format " %dx " (cdr (assoc 'quantity item))))
          (set-text-properties start
                               (point)
                               (list 'pizza-restaurant-mode-menu-item-quantity-marker t )))
      (message "No item at point"))))

(defun pizza-restaurant-mode-remove-item ()
  (interactive)
  (pizza-restaurant-mode-add-item -1))

(defun pizza-restaurant-mode-order ()
  (interactive)

  (goto-char (point-min))
  (while (< (1+ (point)) (point-max))
    (let ((prev-item (get-text-property (point) 'pizza-restaurant-mode-menu-item))
          (item (get-text-property (1+ (point)) 'pizza-restaurant-mode-menu-item)))
      (when (and (not prev-item) item)
        (when (< 0 (cdr (assoc 'quantity item)))
          (let ((item (cdr (assoc 'item item)))
                (quantity (cdr (assoc 'quantity item))))
            (message "%s %s" quantity
                     (decode-coding-string (cdr (assoc 'name item)) 'utf-8))))))
    (forward-char 1)))
