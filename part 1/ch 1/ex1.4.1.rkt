#lang plai

;; Exercise 1.4.1
;;
;; An XML document is well-formed if it satisfies a list of syntax rules
;; provided in the XML specification.
;;
;; XHTML is a family of XML markup languages that mirror or extend
;; versions of HTML.
;;
;; An XHTML document is valid if it conforms to the XHTML specification.
;;
;; In XHTML, the following fragment is well-formed:
;;
;; <em><p>This is a paragraph.</p></em>
;;
;; But, it is invalid since the <em> element is an inline level element
;; and as such is not allowed to contain a <p> element which is a block
;; level element.
;;
;; In XHTML, the following fragment is not well-formed:
;;
;; <p>This is a <em>paragraph.</p></em>
;;
;; since the <em> element is not properly nested within the <p> element.