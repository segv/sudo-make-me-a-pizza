# sudo-make-me-a-pizza

order food from within emacs.

NB: This is some of the ugliest code i've ever written. sorry.

## Installation

Eventually it'll be on melpa, till then, it's just one file, download and `(add-to-list 'load-path "/path/to/sudo-make-me-a-pizza")`

## Usage

```
M-x pizza-search RET <a zip code>
```

hit `RET` on one of the restaurants. 

from the restaurant buffer use `+` and `-` to add remove items to the
cart.

(NOT YET IMPLEMENTED): type in delivery address in "Delivery address"
field, use `C-c C-c` to submit the order (NB: this is binding and asks
for no confirmation)
