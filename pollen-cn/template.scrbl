#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/render txexpr xml pollen/pagetree sugar/coerce pollen/template pollen/template/html pollen/setup))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/template pollen/template/html xml))

@title{Template}

@defmodule[pollen/template]

模板的便利函数。当使用模板渲染时，这些会自动导入到 @racket[eval] 环境中（参见 @racket[render]）。


@section{HTML}

@defmodule[pollen/template/html]

特定于 HTML 模板的函数。

@defproc[
(->html
[xexpr-or-xexprs (or/c xexpr? (listof xexpr?))]
[#:tag html-tag (or/c #f txexpr-tag?) #f]
[#:attrs html-attrs (or/c #f txexpr-attrs?) #f]
[#:splice? splice-html? boolean? #f])
string?]
将 @racket[_xexpr-or-xexprs] 转换为 HTML 字符串。类似于 @racket[xexpr->string]，但与 HTML 规范一致，出现在 @code{script} 或 @code{style} 块中的文本不会被转义。

@examples[#:eval my-eval
(define tx '(root (script "3 > 2") "Why is 3 > 2?"))
(xexpr->string tx)
(->html tx)
]

可选的关键字参数 @racket[_html-tag] 和 @racket[_html-attrs] 允许您为生成的 HTML 设置外部标记和属性。如果 @racket[_xexpr-or-xexprs] 已经有一个外部标签或属性，它们将被替换。

@examples[#:eval my-eval
(define tx '(root ((id "huff")) "Bunk beds"))
(->html tx)
(->html tx #:tag 'div)
(->html tx #:attrs '((id "doback")))
(->html tx #:tag 'div #:attrs '((id "doback")))
]

而如果 @racket[_xexpr-or-xexprs] 没有标签或属性，它们将被添加。如果您提供没有标签的属性，您将收到错误消息。

@examples[#:eval my-eval
(define x "Drum kit")
(->html x)
(->html x #:tag 'div)
(->html x #:tag 'div #:attrs '((id "doback")))
(->html x #:attrs '((id "doback")))
]


如果生成的 HTML 有一个外部标签，@racket[_splice-html?] 选项将把它去掉。否则此选项无效。

@examples[#:eval my-eval
(define tx '(root (p "Chicken nuggets")))
(->html tx)
(->html tx #:splice? #t)
(define x "Fancy sauce")
(->html x)
(code:comment @#,t{This next one won't do anything})
(->html x #:splice? #t)
(code:comment @#,t{Adds the outer tag, but then #:splice? removes it})
(->html x #:tag 'div #:attrs '((id "doback")) #:splice? #t)
]



注意不要将现有的 HTML 字符串传递给这个函数，因为尖括号会被转义。如果这是你想要的，那很好，但你可能不想要。

@examples[#:eval my-eval
(define tx '(p "You did " (em "what?")))
(->html tx)
(->html (->html tx))
]

正如输入合约所暗示的，此函数可以采用单个 @racket[xexpr?] 或 @racket[xexpr?] 列表，并具有预期的结果。

@examples[#:eval my-eval
(define tx '(p "You did " (em "what?")))
(->html tx)
(define txs '("You " "did " (em "what?")))
(->html txs)
(->html #:tag 'p txs)
]
