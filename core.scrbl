#lang scribble/manual

@(require scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/render txexpr xml pollen/pagetree sugar/coerce pollen/core pollen/setup))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/core xml))

@title{Core}

@defmodule[pollen/core]

这些函数会自动导入到每个 Pollen 源文件中（意思是，就好像它们已包含在您的 @filepath{pollen.rkt} 中一样）。



@section{Metas}

每个元表中唯一自动定义的键是 @racket['#,pollen-here-path-key] ，它持有源文件的绝对路径。例如，你可以用@racket[(select-from-metas '#,pollen-here-path-key metas)]来检索这个值。
有关元数据的完整介绍，请参阅 @secref["Inserting_metas"] 。


@defform[(define-meta name value)]
将 @racket[_value] 添加到当前文档的元数据中，使用 @racket[_name] 作为键。

您可以使用 @racket[(select-from-metas _name metas)] 检索元值——甚至在你定义它的同一个文档中。




@section{Splicing}

@defform[(\@ arg ...)]
拼接标签表示一个列表应该被合并到它的包含表达式中。拼接标签为@racket['\@]。

@examples[#:eval my-eval
(module splicer pollen/markup
'(div "one" (\@ "two" "three") "four"))
(require 'splicer)
doc
]

当你想在只能返回一个 X 表达式的情况下返回一个列表时，拼接标签很有用。例如， @secref["Tag_functions"] 只能返回一个 X-表达式。但是如果我们把 X-表达式列表包裹在一个拼接标签中，它们的行为就像一个单一的 X-表达式。之后，Pollen 会将列表元素合并到周围的表达式中（如上所示）。

@examples[#:eval my-eval
(require pollen/tag)

(code:comment @#,t{wrong: function returns a list of X-expressions})
(define-tag-function (multi attrs elems)
  '("foo" "bar"))

(code:comment @#,t{right: function returns a list of X-expressions})
(code:comment @#,t{as elements inside a splicing tag})
(define-tag-function (multi2 attrs elems)
  '(\@ "foo" "bar"))
]


尽管拼接标签在外观上与 @racket[unquote-splicing] 的 @litchar{@"@"} 的缩写符号相同，并且具有相似的目的，但它不是一回事。拼接标签不是变量——它只是 Pollen 在生成输出时特别对待的一个符号。


@defform[(when/splice condition pollen-args)]
如果 @racket[_condition] 为真，则将 @racket[_pollen-args] 放入文档中。在模板文件中，通常这样调用：

@verbatim{◊when/splice[@racketvarfont{condition}]{The text to insert.}}

插入的文本可以包含其自己的嵌套 Pollen 命令。

@racket[when/splice] 比 @racket[when] 更方便，因为 @racket[when] 只会使用大括号之间的最后一个参数。相比之下， @racket[when/splice] 将花括号之间的所有内容视为一个块。


@deftogether[(
@defform[(for/splice (for-clause ...) body-or-break ... body)]
@defform[(for*/splice (for-clause ...) body-or-break ... body)])]
与 @racket[for/list] 和 @racket[for*/list] 类似，但将生成的列表拼接到文档中。

@history[#:added "1.4"]


@section{Data helpers}

从 Pollen 源文件中检索数据的函数。这些不是唯一的选择——当然，您可以使用任何常用的 Racket 函数。


@defproc[
(get-doc
[doc-source (or/c pagenode? pathish?)])
(or/c txexpr? string?)]
从 @racket[_doc-source] 中检索 @racket[doc] 导出，它可以是路径、路径字符串或可以解析为源路径的页面节点。如果无法解析 @racket[_doc-source] ，则引发错误。

如果 @racket[_doc-source] 是相对路径或页面节点，则将其视为相对于 @racket[current-project-root] 。如果这不是您想要的，您需要将其显式转换为完整路径（例如，使用 @racket[path->complete-path] 或 @racket[->complete-path] ）。

如果 @racket[setup:main-export] 被重写了一个项目特定的值，那么它将被检索出来。


@defproc[
(get-metas
[meta-source (or/c pagenode? pathish?)])
hash?]
从 @racket[_meta-source] 检索 @racket[metas] 导出，它可以是路径、路径字符串或可以解析为源路径的页面节点。如果无法解析 @racket[_meta-source] ，则引发错误。

如果 @racket[_meta-source] 是相对路径或页面节点，则将其视为相对于 @racket[current-project-root] 。如果这不是您想要的，您需要将其显式转换为完整路径（例如，使用 @racket[path->complete-path] 或 @racket[->complete-path] ）。

如果 @racket[setup:meta-export] 被重写了一个项目特定的值，那么它将被检索出来。


@deftogether[(

@defproc[
(select
[key symbolish?]
[value-source (or/c hash? txexpr? pagenode? pathish?)])
(or/c #f xexpr?)]

@defproc[
(select*
[key symbolish?]
[value-source (or/c hash? txexpr? pagenode? pathish?)])
(or/c #f (listof xexpr?))]

)]
在 @racket[_value-source] 中查找 @racket[_key] 的匹配项。 @racket[_value-source] 可以是 1) @racket[metas] 的哈希表，2) 表示 @racket[doc] 的标记 X 表达式，或 3) 标识源文件的页面节点或路径，该源文件提供 @racket[metas] 和 @racket[doc] 。在这种情况下，首先在 @code{metas} 中查找 @racket[_key] （使用 @racket[select-from-metas] ），然后在 @code{doc} 中（使用 @racket[select-from-doc] ） .

用 @racket[select] ，你会得到第一个结果；用 @racket[select*] ，你会得到所有结果。

In both cases, you get @racket[#f] if there are no matches.

注意，如果 @racket[_value-source] 是一个相对路径或 pagenode，它将被视为相对于 @racket[current-project-root] 。如果这不是你想要的，你需要明确地将其转换为完整路径（例如，用 @racket[path->complete-path] 或 @racket[->complete-path] ）。

@examples[#:eval my-eval
(module nut-butters pollen/markup
'(div (question "Flavor?")
  (answer "Cashew") (answer "Almond")))
(code:comment @#,t{Import doc from 'nut-butters submodule})
(require 'nut-butters)
(select 'question  doc)
(select 'answer  doc)
(select* 'answer  doc)
(select 'nonexistent-key doc)
(select* 'nonexistent-key doc)
]


@defproc[
(select-from-doc
[key symbolish?]
[doc-source (or/c txexpr? pagenodeish? pathish?)])
(or/c #f (listof xexpr?))]
在 @racket[_doc-source] 中查找 @racket[_key] 的值。 @racket[_doc-source] 参数可以是 1) 表示 @racket[doc] 的标记 X 表达式或 2) 标识提供 @racket[doc] 的源文件的页面节点或源路径。如果 @racket[_key] 的值不存在，你会得到 @racket[#f] 。

注意，如果 @racket[_doc-source] 是一个相对路径或 pagenode，它将被视为相对于 @racket[current-project-root] 。如果这不是你想要的，你需要明确地将其转换为完整路径（例如，用 @racket[path->complete-path] 或 @racket[->complete-path] ）。

@examples[#:eval my-eval
(module gelato pollen/markup
'(div (question "Flavor?")
  (answer "Nocciola") (answer "Pistachio")))
(code:comment @#,t{Import doc from 'gelato submodule})
(require 'gelato)
(select-from-doc 'question  doc)
('answer . select-from-doc . doc)
(select-from-doc 'nonexistent-key doc)
]



@defproc[
(select-from-metas
[key symbolish?]
[meta-source (or/c hash? pagenodeish? pathish?)])
any/c]
在 @racket[_meta-source] 中查找 @racket[_key] 的值。 @racket[_meta-source] 参数可以是 1) 表示 @racket[metas] 的哈希表或 2) 标识提供 @racket[metas] 的源文件的页面节点或源路径。如果 @racket[_key] 的值不存在，你会得到 @racket[#f] 。

注意，如果 @racket[_meta-source] 是一个相对路径或 pagenode，它将被视为与 @racket[current-project-root] 相对。如果这不是你想要的，你需要明确地将其转换为完整路径（例如，用 @racket[path->complete-path] 或 @racket[->complete-path] ）。

@examples[#:eval my-eval
(define metas (hash 'template "sub.xml.pp" 'target "print"))
(select-from-metas 'template  metas)
('target . select-from-metas . metas)
(select-from-metas 'nonexistent-key metas)
]

@section[#:tag "core"]{Parameters}

@defparam[current-metas val (or/c #f hash?) #:value #f]
保存当前 Pollen 源的 @racket[metas] 。在标签函数中，您可以在函数体中引用 @racket[(current-metas)] ，而不是将 @racket[metas] 作为参数传递。同样，如果您的标签函数调用其他标签函数，它们都可以调用 @racket[(current-metas)] 而不是传递值。

@racket[(current-metas)] 也会在模板中工作，持有当前被渲染到模板中的源的 @racket[metas] 。

默认值是 @racket[#f] 。这意味着没有 metas 值可用。明智地处理这种情况是您的责任。

@history[#:added "1.4"]
