#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup pollen/tag))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/setup))

@title{Top}

@defmodule[pollen/top]

你可能永远不会直接调用这个模块。但它被隐式导入到每个 Pollen 标记文件中。如果你不知道它的作用，你可能会对你得到的一些行为感到惊讶。

@defform[(#%top . id)]{

在标准 Racket 中，@racket[#%top] 是最后的函数，当@racket[_id] 未绑定到任何值时调用。因此，它通常会报告语法错误。
 
@examples[
(code:comment @#,t{Let's call em without defining it})
(em "Bonjour")
(code:comment @#,t{(em "Bonjour") is being converted to ((#%top . em) "Bonjour")})
(code:comment @#,t{So calling ((#%top . em) "Bonjour") will give the same result})
((#%top . em) "Bonjour")
]

然而，在 Pollen 标记环境中，这种行为很烦人。因为当您编写 X 表达式时，您不一定要提前定义所有标签。

所以 Pollen 重新定义了 @racket[#%top]。为方便起见，Pollen 的 @racket[#%top] 版本假定未定义的标签应该只引用以该标签开头的 X 表达式（并使用 @racket[default-tag-function] 来提供此行为）：

@examples[
(code:comment @#,t{Again, let's call em without defining it, but using pollen/top})
(require pollen/top)
(em "Bonjour")
(code:comment @#,t{(em "Bonjour") is still being converted to ((#%top . em) "Bonjour")})
(code:comment @#,t{But now, ((#%top . em) "Bonjour") gives a different result})
((#%top . em) "Bonjour")
]

好消息是，这种行为意味着您可以在标记中使用任何您想要的标签，而无需提前定义它。您仍然可以稍后将函数附加到标签，这将自动取代@racket[#%top]。

@examples[
(define (em x) `(span ((style "font-size:100px")) ,x))
(em "Bonjour")
]

坏消息是，您永远不会收到“（unbound identifier）未绑定标识符”错误。这些未绑定的标识符将愉快地通过编译并转换为标签。

@examples[
(require pollen/top)
(define (em . xs) `(span ((style "font-size:100px")) ,@xs))
(code:comment @#,t{There's a typo in my tag})
(erm "Bonjour")
]

@margin-note{如果您更喜欢普通的 Racket 式行为，其中未绑定的标识符会引发错误，请将您的项目中的 @racket[setup:allow-unbound-ids?] 定义为 @racket[#false]。}

这不是一个bug。这只是 Pollen 的 @racket[#%top] 工作方式的自然结果。但是，它有时会使调试变得困难。假设我的标记依赖于 @racket[very-important-function]，而我没有正确导入它。

@examples[
(require pollen/top)
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
(very-important-function "Bonjour")
]

因此，未定义的函数bug未被报告。同样，这不是 Pollen 中的错误——它无法区分故意未定义的标识符和无意中未定义的标识符。如果要保证调用的是已定义的标识符，请使用 @racket[def/c]。}


@defform[(def/c id)]{如果是已定义的标识符，则调用 @racket[_id]，否则引发错误。这种形式反转了 @racket[#%top] 的行为（换句话说，它恢复了默认的 Racket 行为）。

回想一下之前的这个例子。在标准 Racket 中，您会收到未定义标识符错误。

@examples[
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
(very-important-function "Bonjour")
]

但是对于 @racketmodname[pollen/top] 来说，该问题不会被视为错误。

@examples[
(require pollen/top)
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
(very-important-function "Bonjour")
]

通过添加@racket[def/c]，我们恢复了通常的行为，保证我们得到@racket[very-important-function] 的定义版本或什么都没有。

@examples[
(require pollen/top)
(module vif racket/base
    (define (very-important-function . xs) `(secrets-of-universe ,@xs)))
(code:comment @#,t{Forgot to (require 'vif)})
((def/c very-important-function) "Bonjour")
]

}
