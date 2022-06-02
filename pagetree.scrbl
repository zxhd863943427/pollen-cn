#lang scribble/manual

@(require "mb-tools.rkt" scribble/eval pollen/cache pollen/setup (for-label racket (except-in pollen #%module-begin) pollen/setup txexpr pollen/decode pollen/file sugar pollen/pagetree))

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/pagetree txexpr))

@title{Pagetree}

@defmodule[pollen/pagetree]

书籍和其他长文件通常是以结构化的方式组织的--它们至少有一个页面的序列，但更多的是它们有一些章节，其中有一些子序列。 Pollen 项目中的单个页面并不了解它们与其他页面的连接方式。理论上，你可以在源文件中维护这些信息。但这将是对人类精力的一种浪费。

取而代之的是，使用pagetree。一个 @italic{pagetree} 是一个简单的抽象，用于定义和处理 @italic{pagenodes} 的序列。通常，这些pagenodes是你项目中输出文件的名称。

``所以它是一个网页文件名列表？'' 有点。当我想到网页时，我会想到磁盘上的实际文件。与 Pollen 的动态渲染方向保持一致，页面节点可能（并且经常这样做）引用尚不存在的文件。此外，通过引用输出名称而不是源名称，您可以灵活地更改与特定页面节点关联的源类型（例如，从preprocessor 源文件到 Pollen markup）。

页面树可以是平面的或分层的。平面页面树只是页面节点的 @seclink["Lists__Iteration__and_Recursion" #:doc '(lib "scribblings/guide/guide.scrbl")]{list}。分层页面树还可以包含递归嵌套的页面节点列表。但是你不需要注意这种区别，因为pagetree函数并不关心你使用哪种类型。我也不关心。

在整个 Pollen 系统中都有Pagetrees。它们主要用于导航--例如，计算特定页面的 "上一页"、"下一页 "或 "向上 "链接。一个特殊的pagetree， @filepath{index.ptree} ，被项目服务器用来排列仪表板中的文件。Pagetrees也可以用来为某些操作定义成批的文件，例如@secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl") ]。你可能也会发现它们的其他用途。



@section{Making pagetrees with a source file}

一个pagetree源文件要么以@code{#lang pollen}开头并使用@racketfont{@(format ".~a" pollen-pagetree-source-ext)}扩展名，要么以@code{#lang pollen/ptree}开头，然后可以有任何文件扩展名。

与其他 Pollen 源文件不同，由于 pagetree 源不呈现为输出格式，因此文件名的其余部分由您决定。

这是一个平面页面树。每一行都被认为是一个单独的页面节点（空白行被忽略）。请注意，在 pagetree 源代码中不需要 Pollen 命令语法或引用：

@fileblock["flat.ptree" @codeblock{
#lang pollen

index.html
introduction.html
main_argument.html
conclusion.html
}]

这是 DrRacket 中的输出：

@repl-output{'(pagetree-root index.html introduction.html main_argument.html conclusion.html)}

与通常的 Pollen 策略保持一致，这是一个 @seclink["X-expressions" #:doc '(lib "pollen/scribblings/pollen.scrbl")]{X-expression}。 @racket[pagetree-root] 只是一个包含页面树的任意标签。

升级到分层页面树很简单。同样的基本规则也适用——每行一个页面节点。但这一次，您添加 Pollen 命令语法：在页面节点前面的菱形 @litchar{◊} 将其标记为嵌套列表的顶部，并且该列表的子页面节点位于 @litchar{@"{"} 之间花括号@litchar{@"}"}，像这样：

@fileblock["hierarchical.ptree" @codeblock{
#lang pollen

toc.html
◊first-chapter.html{
    foreword.html
    introduction.html}
◊second-chapter.html{
    ◊main-argument.html{
        facts.html
        analysis.html}
    conclusion.html}
bibliography.html
}]

我们的分层页面树的输出：

@repl-output{'(pagetree-root toc.html (first-chapter.html foreword.html introduction.html) (second-chapter.html (main-argument.html facts.html analysis.html) conclusion.html) bibliography.html)}

使用源文件的一个好处是，当你在 DrRacket 中运行它时，它会自动使用 @racket[validate-pagetree] 进行检查，确保页面树中的每个元素都符合 @racket[pagenode?] ，并且所有的页面节点是唯一的。

这个页面树有一个重复的页面节点，所以它无法运行：

@fileblock["duplicate-pagenode.ptree" @codeblock{
#lang pollen

index.html
introduction.html
main_argument.html
conclusion.html
index.html
}]

相反，你会得到一个错误：

@errorblock{validate-pagetree: members-unique? failed because item isn’t unique: (index.html)}

页面节点可以引用子目录中的文件。只需将 pagenode 写为相对于 pagetree 所在目录的路径：

@fileblock["flat.ptree" @codeblock{
#lang pollen

foreword.html
◊facts-intro.html{
    facts/brennan.html
    facts/dale.html
}
◊analysis/intro.html{
    analysis/fancy-sauce/part-1.html
    analysis/fancy-sauce/part-2.html
}
conclusion.html
}]

@section{Making pagetrees by hand}

因为页面树只是一个 X 表达式，所以您可以使用任何 Pollen 或 Racket 工具来制作 X 表达式以合成一个页面树。例如，这里有一些生成与上面的 @filepath{flat.ptree} 源文件相同页面树的 Racket 代码：

@fileblock["make-flat-ptree.rkt" @codeblock{
#lang racket
(require pollen/pagetree)
(define node-names '(index introduction main_argument conclusion))
(define pt `(pagetree-root 
  ,@"@"(map (λ (n) (string->symbol (format "~a.html" n))) node-names)))
(if (pagetree? pt) pt "Oops, not a pagetree")
}]

请注意，在手动构建页面树时需要更加小心。页面节点是符号，而不是字符串，因此必须使用 @racket[string->symbol] 。使用 pagetree 源文件的一个好处是它会为您处理这些事务。

@section{Nesting pagetrees}

您可以将其他页面树放在页面树中。由于每个页面树都是一个 X 表达式，因此您可以像嵌套普通 X 表达式一样嵌套页面树。假设你有这个页面树：

@fileblock["sub.ptree" @codeblock{
#lang pollen
three
four
}]

并且您想将其添加到现有的页面树中：

@fileblock["index.ptree" @codeblock{
#lang pollen
one
two
five
six}]

你可以 @racket[require] @filepath{sub.ptree} 来导入它的 @racket[doc] 。请务必使用 @racket[prefix-in] 以便导入的 @racket[doc] 以一个不同的名称结束，该名称与已经是当前文件一部分的 @racket[doc] 不冲突：

@fileblock["index.ptree" @codeblock{
#lang pollen
◊(require (prefix-in sub: "sub.ptree"))
one
two
◊sub:doc
five
six}]

你会得到这个：

@repl-output{'(pagetree-root one two three four five six)}

Pollen 为你做了一点点事务，就是它会自动丢弃导入的 pagetree 的根节点，并在插入点将剩余的节点拼接到父 pagetree 中。否则你会得到这个，这可能不是你想要的：

@repl-output{'(pagetree-root one two (pagetree-root three four) five six)}

但如果您确实希望导入的页面树位于子节点下，只需像往常一样添加一个包含页面节点：

@fileblock["index.ptree" @codeblock{
#lang pollen
◊(require (prefix-in sub: "sub.ptree"))
one
two
◊subtree{
  ◊sub:doc
}
five
six}]

这会给你：

@repl-output{'(pagetree-root one two (subtree three four) five six)}

如果您想组合多个页面树， @racket[require] 可能会变得很麻烦，因为您必须同时处理多个 @racket[doc] 导入（可以使用 @racket[prefix-in] 来完成，但它仍然需要处理）。相反，您可以使用 @racket[dynamic-require] 将每个导入的页面树放在您想要的位置。

@fileblock["index.ptree" @codeblock{
#lang pollen
one
two
◊(dynamic-require "sub.ptree" 'doc)
five
six
◊(dynamic-require "sub-two.ptree" 'doc)
nine
ten
}]

嵌套页面树不会规避针对重复页面节点的常规规则。所以这个试图嵌套 @filepath{sub.ptree} 两次的页面树将不起作用：

@fileblock["index.ptree" @codeblock{
#lang pollen
one
two
◊(dynamic-require "sub.ptree" 'doc)
◊(dynamic-require "sub.ptree" 'doc)
five
six
}]

@section{The automatic pagetree}

在 Pollen 需要一个页面树但找不到的情况下，它会自动从目录中的文件列表合成一个页面树。当 @secref["The_project_dashboard"] 位于不包含显式 @filepath{index.ptree} 的目录中时，这种情况最常见。这样，您就可以开始一个项目，而不必停下来为 @racketfont{.ptree} 做家务。

像往常一样，便利是有代价的。 Pollen 不知道您目录中的哪些文件与项目相关，因此它包含所有这些文件。例如，如果您在 Mac OS 桌面上启动项目服务器，您会看到类似 @filepath{Thumbs.db} 和 @filepath{desktop.ini} 的内容。

此外，尽管您可以将 @racket[next] 或 @racket[siblings] 之类的页面树导航函数与自动页面树一起使用，但这些函数的结果很容易包含不相关的文件。因此，如果您需要进行页面树导航，这可能就是您想要开始使用显式页面树的地方。


@section{Using pagetrees for navigation}

通常，您将从模板内部调用 pagetree-navigation 函数，使用特殊变量 @racket[here] 作为起点。有关此技术的更多信息，请参阅@secref["Pagetree_navigation" #:tag-prefixes '("tutorial-2")]。

@section{Using @filepath{index.ptree} in the dashboard}

当您使用项目服务器查看目录中的文件时，服务器将首先查找名为 @filepath{index.ptree} 的文件。如果它找到这个 pagetree 文件，它将使用它来构建仪表板。如果没有，那么它将使用目录列表合成一个页面树。有关此技术的更多信息，请参阅 @secref["The_project_dashboard"] 。

@section{Using pagetrees with @exec{raco pollen render}}

@exec{raco pollen render} 命令用于从其源重新生成输出文件。如果您将页面树传递给@exec{raco pollen render}，它将自动渲染页面树中列出的每个文件。

例如，许多项目都有辅助页面，这些页面实际上不属于主导航流。您可以在单独的页面树中收集这些页面：

@fileblock["utility.ptree" @codeblock{
#lang pollen

404-error.html
terms-of-service.html
webmaster.html
[... and so on]
}]

因此，当您在模板中使用页面树导航功能时，可以使用主页面树，并将导航限制为主要编辑内容。但是当你渲染项目时，你可以将两个页面树都传递给@exec{raco pollen render}。

For more on this technique, see @secref["raco_pollen_render" #:doc '(lib "pollen/scribblings/pollen.scrbl")].


@section{Functions}

@subsection{Predicates & validation}


@defproc[
(pagetree?
[possible-pagetree any/c])
boolean?]
测试 @racket[_possible-pagetree] 是否是有效的页面树。它必须是一个 @racket[txexpr?] ，其中所有元素都是 @racket[pagenode?] ，并且每个元素在 @racket[_possible-pagetree] 中都是唯一的（不包括根节点）。

@examples[#:eval my-eval
(pagetree? '(root index.html))
(pagetree? '(root duplicate.html duplicate.html))
(pagetree? '(root index.html "string.html"))
(define nested-ptree '(root 1.html 2.html (3.html 3a.html 3b.html)))
(pagetree? nested-ptree)
(pagetree? `(root index.html ,nested-ptree (subsection.html more.html)))
(code:comment @#,t{Nesting a subtree twice creates duplication})
(pagetree? `(root index.html ,nested-ptree (subsection.html ,nested-ptree)))
]

@defproc[
(validate-pagetree
[possible-pagetree any/c])
pagetree?]
与 @racket[pagetree?] 类似，但如果 @racket[_possible-pagetree] 无效，则会引发一个描述性错误，否则会返回 @racket[_possible-pagetree] 本身。

@examples[#:eval my-eval
(validate-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(validate-pagetree `(root (,+ son.html daughter.html) uncle.html))
(validate-pagetree '(root (mama.html son.html son.html) mama.html))
]


@defproc[
(pagenode?
[possible-pagenode any/c])
boolean?]
测试 @racket[_possible-pagenode] 是否是一个有效的页面节点。 pagenode 可以是任何不是空格的 @racket[symbol?] 。页面树的每一片叶子都是一个页面节点。实际上，您的页面节点可能是输出文件的名称。

@margin-note{页面节点是符号（而不是字符串），因此页面树将是有效的标记 X 表达式，这是一种更方便的验证和处理格式。}

@examples[#:eval my-eval
(code:comment @#,t{Three symbols, the third one annoying but valid})
(map pagenode? '(symbol index.html |   silly   |))
(code:comment @#,t{A number, a string, a txexpr, and a whitespace symbol})
(map pagenode? '(9.999 "index.html" (p "Hello") |    |))
]


@defproc[
(pagenodeish?
[v any/c])
boolean?]
如果 @racket[_v] 可以用 @racket[->pagenode] 转换，则返回 @racket[#t] 。

@examples[#:eval my-eval
(map pagenodeish? '(9.999 "index.html" |    |))
]


@defproc[
(->pagenode
[v pagenodeish?])
pagenode?]
Convert @racket[_v] to a pagenode.

@examples[#:eval my-eval
(map pagenodeish? '(symbol 9.999 "index.html" |  silly  |))
(map ->pagenode '(symbol 9.999 "index.html" |  silly  |))
]



@subsection{Navigation}


@defparam[current-pagetree pagetree pagetree?]{
如果未明确指定另一个参数，则定义页面树导航功能（例如， @racket[parent] 、 @racket[children] 等）使用的默认页面树的参数。初始化为 @racket[#f] 。}


@defproc[
(parent
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f pagenode?)]
在 @racket[_pagetree] 中找到 @racket[_p] 的父页面节点。如果没有，或者到达页面树的根目录，则返回 @racket[#f] 。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(parent 'son.html)
(parent 'daughter.html)
(parent "uncle.html")
(parent (parent 'son.html))
]

@defproc[
(children
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f (listof pagenode?))]
在 @racket[_pagetree] 中找到 @racket[_p] 的子页面节点。如果没有，则返回 @racket[#f] 。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(children 'mama.html)
(children 'uncle.html)
(children 'root)
(map children (children 'root))
]


@defproc[
(siblings
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f (listof pagenode?))]
在 @racket[_pagetree] 中找到 @racket[_p] 的同级页面节点。结果包括 @racket[_p] 本身。但如果 @racket[_pagetree] 是 @racket[#f] ，该函数仍将返回 @racket[#f] 。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html)))
(siblings 'son.html)
(siblings 'daughter.html)
(siblings 'mama.html)
]

@defproc[
(other-siblings
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f (listof pagenode?))]
与 @racket[siblings] 类似，但结果不包括 @racket[_p] 本身。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html)))
(other-siblings 'son.html)
(other-siblings 'daughter.html)
(other-siblings 'mama.html)
]


@deftogether[(

@defproc[
(previous
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f pagenode?)]

@defproc[
(previous*
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f (listof pagenode?))]
)]
在 @racket[_p] 之前返回页面节点。对于 @racket[previous*] ，依次返回 @racket[_p] 之前的所有页面节点。在这两种情况下，如果没有任何页面节点，则返回 @racket[#f] 。根页面节点被忽略。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(previous 'daughter.html)
(previous 'son.html)
(previous (previous 'daughter.html))
(previous 'mama.html)
(previous* 'daughter.html)
(previous* 'uncle.html)
]

@deftogether[(

@defproc[
(next
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f pagenode?)]

@defproc[
(next*
[p (or/c #f pagenodeish?)]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
(or/c #f (listof pagenode?))]
)]
在 @racket[_p] 之后立即返回页面节点。对于 @racket[next*] ，依次返回 @racket[_p] 之后的所有页面节点。在这两种情况下，如果没有任何页面节点，则返回 @racket[#f] 。根页面节点被忽略。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(next 'son.html)
(next 'daughter.html)
(next (next 'son.html))
(next 'uncle.html)
(next* 'mama.html)
(next* 'daughter.html)
]

@subsection{Utilities}


@defproc[
(get-pagetree
[pagetree-source (or/c pagetree? pathish?)])
pagetree?
]
从 @ext[pollen-pagetree-source-ext] 源文件中获取一个pagetree，即 @racket[_pagetree-source] 。如果 @racket[_pagetree-source] 已经是一个页面树，只需将其传递出去即可。


@defproc[
(pagetree->list
[pagetree (or/c pagetree? pathish?)])
list?
]
将 @racket[_pagetree] 转换为一个简单的列表。因为使用 @racket[flatten] ，所以等价于对 @racket[_pagetree] 的进行预排序深度优先遍历。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(pagetree->list (current-pagetree))
]


@defproc[
(in-pagetree?
[pagenode pagenodeish?]
[pagetree (or/c pagetree? pathish?) (current-pagetree)])
boolean?
]
报告 @racket[_pagenode] 是否在 @racket[_pagetree] 中。

@examples[#:eval my-eval
(current-pagetree '(root (mama.html son.html daughter.html) uncle.html))
(in-pagetree? 'son.html)
(in-pagetree? 'alcoholic-grandma.html)
]


@defproc[
(path->pagenode
[p pathish?]
[starting-path pathish? (current-project-root)])
pagenode?
]
将路径 @racket[_p] 转换为页面节点——意思是，使其相对于 @racket[_starting-path] ，通过 @racket[->output-path] 运行，并将其转换为符号。不会告诉您生成的页面节点是否实际存在于当前页面树中（想要达到这个目的，请使用 @racket[in-pagetree?] ）。
