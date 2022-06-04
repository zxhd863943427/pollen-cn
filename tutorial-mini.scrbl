#lang scribble/manual

@(require scribble/eval (for-label pollen/unstable/pygments pollen/decode plot pollen/setup pollen/tag racket/base pollen/template txexpr racket/list racket/string))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/template pollen/tag xml racket/list txexpr))


@title[#:tag "mini-tutorial"]{一些独立的小教程}

较小的、独立的任务。

@section{Syntax highlighting}

Pollen 和 Racket 都没有自己的语法高亮库，但你可以使用一个外部的语法高亮器。有两个选择。Pygments 或 Highlight.js。

Highlight.js 是一个 JavaScript 库，因此是为了在浏览器中运行。所以，如果你的目标是 HTML，而且你不介意把这项工作交给浏览器，那就没问题。它也更容易设置。

Pygments 是一个 Python 库（尽管你不需要知道任何 Python 就可以在 Pollen 中使用它）。它与 Pollen 一起运行（在一个单独的进程中），因为页面被渲染，所以语法高亮可以被植入你的编译文件。如果你想针对除 HTML 之外的其他格式，这是一个更好的选择。它需要更多的设置，但我怀疑任何已经做到这一步的人都会有问题。

@subsection[#:tag "pygments-with-pollen"]{Using Pygments with Pollen}

我使用@link["http://pygments.org/"]{Pygments} 在@link["https://beautifulracket.com/"]{@italic{Beautiful Racket}} 中完成了语法高亮。文章底部提供了指向源的链接。

@itemlist[#:style 'ordered

@item{确保你已经安装了@code{pygments}。 @link["http://pygments.org/download/"]{Instructions here.} 非常简单——例如，在我的 Mac OS 机器上，它只需要在命令行中使用 @code{easy_install pygments}。}

@item{@racketmodname[pollen/unstable/pygments]辅助模块提供了一个名为@racket[highlight]的函数。要使@racket[highlight]在你的源文件中可用，你可以在源文件中加入一行@code{◊(require pollen/unstable/pygments)}，或者把它放在@racket["pollen.rkt"]中，然后从那里@racket[provide]（获得）它。}

@item{要调用 Pygments，请使用 @racket[highlight]，方法是在括号中提供类似 @racket['python] 的语言名称（注意名称前面的引号），然后在大括号之间突出显示要突出显示的代码。

@codeblock{
#lang pollen
◊(require pollen/unstable/pygments)
◊highlight['python]{
for x in range(3):
    print x
}
}

当你运行这个文件时，你应该会看到类似这样的内容，解析后的语法会标记为 X 表达式：

@repl-output{
'(div ((class "highlight")) (table ((class "sourcetable")) (tbody (tr (td ((class "linenos")) (div ((class "linenodiv")) (pre "1\n2"))) (td ((class "code")) (div ((class "source")) (pre (span ((class "k")) "for") " " (span ((class "n")) "x") " " (span ((class "ow")) "in") " " (span ((class "nb")) "range") (span ((class "p")) "(") (span ((class "mi")) "3") (span ((class "p")) "):") "\n    " (span ((class "k")) "print") " " (span ((class "n")) "x") "\n")) "\n")))) "\n")
}
}

@item{为了获得高亮，你仍然需要在你的 CSS 中添加样式来改变代码的外观。如果您查看 @link["http://unitscale.com/mb/technique/styles.css.pp.html"]{我的演示文章中的 CSS 源代码}，我只是在底部粘贴了一个 Pygments 主题（即 @link["https://github.com/richleland/pygments-css"]{我复制了一份主题} 然后编辑了其中一些变量的值）。
}

]
 
我承认最后一步并不方便。但我还没有弄清楚如何让它变得更容易。用一堆 Pygments 主题来填充 Pollen 发行版是没有意义的。此外，即使方便，它们也不能编辑/可编程，这是整个练习的重点。

无论如何，这就是它在 @code{unstable} 类别中的原因——它有效，但我认为它可以做得更好。

@subsection{Using Highlight.js with Pollen}

因为@link["https://highlightjs.org/"]{Highlight.js} 是基于浏览器的，所以不需要 Pollen 的任何高级别的配合。您只需将其添加到您的项目中，例如图像或网络字体或其他链接资产。


@itemlist[#:style 'ordered

@item{下载@link["https://highlightjs.org/usage/"]{Highlight.js} 库。}

@item{将这些行添加到您的@filepath{template.html}（或其他模板）的@code{<head>} 部分：

@terminal{
<link rel="stylesheet" href="/path/to/styles/default.css">
<script src="/path/to/highlight.js"></script>
<script>hljs.initHighlightingOnLoad();</script>
}
}

@item{加载这些资源后，Highlight.js 将使用标记 @tt{<pre><code class="language-name">...</code></pre>} 自动语法高亮任何代码。所以在 Pollen 标记中，你可以直接这样写：

@codeblock{
#lang pollen/markup
◊pre{◊code[#:class "python"]{
for x in range(3):
    print x
}}}}

@item{或者，如果您想匹配 @racketmodname[pollen/unstable/pygments] 的符号，您可以编写一个 @tt{highlight} 函数，该函数可以自动扩展为适用于 Highlight.js 的标记：

@codeblock{
#lang pollen/markup
◊(define (highlight lang . xs)
   `(pre (code ((class ,(format "~a" lang))) ,@"@"xs))) 
◊highlight['python]{
  for x in range(3):
    print x
}}}
]

如上所述，我承认让 Pollen 自动将必要的标记放入你的 HTML 的@tt{<head>}中会更方便。但作为一个倾向问题，我更倾向于尽量减少魔法行为。



@section{Math typesetting with MathJax}

@link["http://www.mathjax.org"]{MathJax} 是一个 JavaScript 库，用于实现 TeX 的数学排版算法。将 MathJax 添加到 Pollen 项目中很容易，然后在你的源文件中可以用 Pollen 命令符号调用它。

@itemlist[#:style 'ordered

@item{如果您想在本地运行库，无需网络连接，请下载 @link["http://docs.mathjax.org/en/latest/start.html"]{MathJax} 库。你也可以使用像 @link["https://cdnjs.com/"]{cdnjs} 这样的 MathJax CDN 并通过网络链接到库（我将在下面的示例中使用此选项）。}

@item{将这些行添加到您的@filepath{template.html}（或其他模板）的@code{<head>} 部分。首先加入 MathJax 库本身：

@terminal{
<script type="text/javascript"
  src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
}

然后，添加需要的任何配置选项。例如，这将激活 $ 符号作为内联方程分隔符：

@terminal{
<script type="text/x-mathjax-config">
  MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});
</script>
}
}

@item{制作添加分隔符的 Pollen 标签函数以及将触发 JavaScript 排版的 @tt{mathjax} 包装标记。例如，假设我们想用@code{◊${equation ...}} 表示内联方程，并用@code{◊$${equation ...}} 表示块方程。我们的标签函数可能如下所示：

@codeblock{
#lang pollen
◊(define ($ . xs)
  `(mathjax ,(apply string-append `("$" ,@"@"xs "$"))))
◊(define ($$ . xs)
  `(mathjax ,(apply string-append `("$$" ,@"@"xs "$$"))))
}
}
]


把它放在一起，这是两个文件中的一个最小工作示例（显然在一个更大的项目中，你会将这些标签函数移动到一个 @filepath{pollen.rkt} 文件）：

@fileblock["equation.html.pm"
@codeblock{
#lang pollen
◊(define ($ . xs)
  `(mathjax ,(apply string-append `("$" ,@"@"xs "$"))))
◊(define ($$ . xs)
  `(mathjax ,(apply string-append `("$$" ,@"@"xs "$$"))))

◊h1{I wonder if ◊${2^{\aleph_\alpha} = \aleph_{\alpha+1}}?}
}]



@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
<script type="text/javascript"
src="https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.0/MathJax.js?config=TeX-AMS-MML_HTMLorMML">
</script>
<script type="text/x-mathjax-config">
MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$']]}});
</script>
</head>
<body>
◊(->html doc)
</body>
</html>
}]

顺便说一句，没有 @code{pollen/math} 模块，因为这项任务似乎并不复杂，不值得这样做。我刚才描述的就是你所需要的一切。
