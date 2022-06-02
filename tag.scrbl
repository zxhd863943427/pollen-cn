#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) txexpr pollen/tag pollen/render xml pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template xml))

@title{Tag}

@defmodule[pollen/tag]

使用标签的便利函数。


@defproc[
(default-tag-function
[id txexpr-tag?]
[kw-attr-name keyword?]
[kw-attr-value string?] ... ...)
(-> txexpr?)]
为@racket[_id] 创建一个默认标签函数。新标签函数采用一组可选的 X 表达式属性 (@racket[txexpr-attrs?])，后跟 X 表达式元素 (@racket[txexpr-elements?])。根据这些，标签函数使用@racket[_id] 作为标签创建一个带标签的 X 表达式。

@examples[
(require pollen/tag)
(define beaucoup (default-tag-function 'em))
(beaucoup "Bonjour")
(beaucoup '((id "greeting")) "Bonjour")
]

以这种方式输入属性可能很麻烦。因此，为方便起见，新的标记函数提供了一种替代方法：任何关键字参数及其值都将被解释为属性。

@examples[
(require pollen/tag)
(define beaucoup (default-tag-function 'em))
(beaucoup #:id "greeting" #:class "large" "Bonjour")
]

您还可以为 @racket[default-tag-function] 本身提供关键字参数，它们将成为每次使用标签函数的默认属性。

@examples[
(require pollen/tag)
(define beaucoup-small (default-tag-function 'em #:class "small"))
(beaucoup-small #:id "greeting" "Bonjour")
]

Pollen 还使用此函数为未定义的标签提供默认行为。见@racket[#%top]。

请注意，虽然默认标记函数通常用于生成标记的 X 表达式，但它们不会对输入实施任何限制，因此它们也不保证您实际上会获得有效的标记 X 表达式作为输出。这是故意的——默认标签函数是为了方便编码，它们的输出很可能会被其他标签函数处理，所以在这里提出错误还为时过早。

@examples[
(require pollen/tag)
(define strange (default-tag-function 'div #:class "bizarre"))
(code:comment @#,t{Invalid data types for elements})
(strange + *)
(code:comment @#,t{Double "class" attribute})
(strange #:class "spooky")
]



@defform[
(define-tag-function
(tag-id attr-id elem-id) body ...)]
用于制作自定义标签功能的辅助函数。处理解析杂务，包括将关键字参数转换为属性（在@racket[default-tag-function] 中描述），并正常解析其他属性和元素。

@examples[
(require pollen/tag)
(define-tag-function (tag-name attrs elems) 
  `(new-name ,(cons '(zim "zam") attrs) ,@elems))
(tag-name "Hello world")
(tag-name '((key "value")) "Hello world")
(tag-name #:key "value" "Hello world")
]