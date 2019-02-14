-------------------------------------------------------------------------------

This project is no longer maintained.

-------------------------------------------------------------------------------


_idris-js_
==========

Toy JavaScript backend for Idris.

Based on Edwin Brady’s [PHP backend](https://github.com/edwinb/idris-php).  Written in order to arrive at a [GNU _bash_ backend](https://github.com/mietek/idris-bash).

Barely functional.


Example
-------

    $ idris pythag.idr --codegen js -o pythag.js
    $ time node pythag.js
    [(3, (4, 5)), (6, (8, 10)), (5, (12, 13)), (9, (12, 15))]

    real    0m0.080s
    user    0m0.076s
    sys     0m0.019s

Input: [`pythag.idr`](pythag.idr)

Output: [`pythag.js`](https://gist.github.com/mietek/2b8f086259b925039459)


About
-----

Made by [Miëtek Bak](https://mietek.io/).  Published under the BSD license.
