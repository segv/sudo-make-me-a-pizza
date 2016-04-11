# sudo-make-me-a-pizza

order food from within emacs.

## Installation

Eventually it'll be on melpa, till then, it's just one file, download and `(add-to-list 'load-path "/path/to/sudo-make-me-a-pizza")`

## Usage

```
M-x pizza-search RET <a zip code>
```

hit `RET` on one of the restaurants. 

(NOT YET IMPLEMENTED): from the restaurant buffer use `+` and `-` to
add remove items to the cart, use `C-c C-c` to submit the order (NB:
this is binding and asks for no confirmation)
