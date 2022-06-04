#lang scribble/manual

@(require scribble/eval (for-label pollen/decode pollen/core racket/math plot pollen/setup pollen/tag racket/base pollen/template txexpr racket/list racket/string))
@(require "mb-tools.rkt")

@(define my-eval (make-base-eval))
@(my-eval `(require pollen pollen/decode pollen/template pollen/tag xml racket/list txexpr))


@title[#:tag "third-tutorial"]{第三个教程： Pollen 标记和标签函数}

现在，你要做的是好事情了。在本教程中，你将使用 Pollen 来发布一篇用 Pollen 标记编写的多页文章。你将了解到:

@itemlist[


@item{使用 Pollen 标记添加标签和属性}

@item{将行为附加到标签函数}

@item{@filepath{pollen.rkt} 文件}

@item{通过 Pollen 标记使用 @racket[decode]}

@;item{@exec{raco pollen render} 和 @exec{raco pollen publish}}


]

如果您想以最短的时间了解 Pollen ，请尝试@secref["quick-tour"]。

@section[#:tag-prefix "tutorial-3"]{先决条件}

我假设您已经完成了@seclink["second-tutorial"]{second tutorial} 并且您了解 Pollen 创作模式的原理 — 创建源文件，将它们转换为 X 表达式，然后将它们与模板组合制作输出文件。

因为现在是时候加快步伐了。你已经学会了如何用 Pollen 做一些方便的事情。但是我们还没有开发出编写环境和编程语言的完全融合。我答应过你@secref["the-book-is-a-program"]，对吧？所以让我们做一些编程。


@section[#:tag "pollen-vs-xml"]{可选阅读： Pollen 标记与 XML}

如果对 XML 不感兴趣，您可以跳过本节。但是 Pollen 标记是在我尝试提出一种更可用于编写的 XML 替代方案的过程中演变而来的。因此，如果您熟悉 XML，那么对比可能会有所帮助。

@subsection{XML 的问题}

在@seclink["second-tutorial"]{second tutorial} 中，我认为 Markdown 是作者的限制格式。为什么？因为 Markdown 只是 HTML 标签的简写。因此，它存在三个问题：它不是语义的，它只涵盖有限的 HTML 标记子集，并且它不能被作者扩展。 

这些问题部分是 HTML 本身的限制。这些限制本应通过 XML 来解决——@italic{X} 代表 @italic{extensible}。原则上，XML 允许您定义您喜欢的任何标签并在文档中使用它们。

那么，为什么 XML 还没有占领世界呢？在实践中，XML的承诺比它提供的要多。对于任何试图使用XML作为创作格式的作家来说，其原因是显而易见的。

@itemlist[

@item{@bold{繁琐的语法}。 不幸的是， XML 依靠的是与 HTML 相同的角括号符号。如果你认为 HTML 源代码难以阅读，那么 XML 就更糟糕了。由于写作的大部分内容都涉及到阅读，这个特点也是一个主要的错误。}

@item{@bold{验证的开销}。XML 的组成部分是 @defterm{validation} 的概念，它保证文档符合某些形式标准，通常在 @italic{schema} 中定义。为了从 XML 中获得完整的价值，你通常希望使用验证。但这样做会给作为作者的你带来更多的工作，并消除了 XML 的大部分表达潜力。}

@item{@bold{自虐式文档处理}。 我指的是XSLT，它是转换XML文档的首选方法。我知道一点 XSLT，所以我承认有一种方法可以解决它的疯狂。但这仍然是疯狂的。}

]

关于 XML，我们可以说的最好的一点是它的意图是好的。它指向正确的目标。但它的好处隐藏在恶劣的人体工程学之下。


@subsection{Pollen 标记的不同之处}

 Pollen 标记可以被视为一种获得 XML 好处而又不会引起头痛的方法。与 XML 一样，Pollen 标记允许您自由地标记您的文本。但与 XML 不同：

@itemlist[

@item{@bold{简单的语法}。Pollen 标记遵循 Pollen 命令的常规约定。}

@item{@bold{无结构验证}。 您可以按任何顺序使用任何您想要的标签，而且您无需提前定义它们。您的文档仍然有效。}

@item{@bold{ Racket 处理}。  Pollen 标记可以使用 Racket 函数为其附加行为，可以在你使用它们之前，也可以在之后。}

]



@subsection{``但我真的需要 XML……''}

您可以拥有 XML。使用 Pollen 标记生成 XML 文件，然后将这些文件输入现有的 XML 处理管道完全没有问题。换句话说，使用 Pollen 标记，您可以将 XML 视为一种输出格式，而不是一种输入格式。 

在本教程中，我将使用 HTML 模板呈现 Pollen 标记。但是您可以轻松地将相同的工作流程与 XML 模板一起使用，从而得到 XML 文件。


@section{用 Pollen 标记写作}

Pollen 标记是一个自由格式的标记系统，可让您在文本中添加任意 @defterm{tags} 和 @defterm{attributes}。任意，我的意思是你不需要将你的标签限制在现有的规范中（例如，HTML 允许的标签）。你可以——但这是一个选项，而不是一个要求。

我喜欢将 Pollen 标记视为一种不仅可以捕获文本，还可以捕获我的@bold{关于文本的想法}的方式。其中一些是低层次的想法（“这个文本应该是斜体”）。有些是高层次的想法（``此文本是页面的主题''）。有些只是给我自己的笔记。简而言之，我所知道的关于文本的一切都会成为文本的一部分。

这样一来，Pollen 标记就成为了本书的源代码。让我们试试看。

@subsection{创建 Pollen 标记文件}

我们将使用 Pollen 标记来制作最终将成为 HTML 的源文件。与我们在 @seclink["second-tutorial"]{second tutorial} 中学习的创作模式工作流程一致，我们将从所需的输出文件名 @filepath{article.html} 开始，然后附加新的 Pollen 标记后缀，即@filepath{.pm}。

在 DrRacket 中，像这样开始一个名为 @filepath{article.html.pm} 的新文件（像往常一样，您可以使用任何您喜欢的示例文本）：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend RacketCon this year.
}]

与通常的创作模式策略一致，当您运行此文件时，您将获得一个以 @code{root} 开头的 X 表达式：

@repl-output{'(root "I want to attend RacketCon this year.")}

请记住，即使文件的第一行是 @racketmodfont{#lang} @racketmodname[pollen] — 与上一个教程相同 — 新的 @filepath{.pm} 后缀表明 Pollen 应该将源解释为 Pollen 标记。

例如，看看如果你把 Markdown 源代码放在 Pollen 标记文件中会发生什么，就像这样:

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I am **so** excited to attend __RacketCon__ this year.
}]

Markdown 语法将被忽略，并传递到输出：

@repl-output{'(root "I am **so** excited to attend __RacketCon__ this year.")}

恢复为非 Markdown 源文件，让我们继续。


@subsection{Tags & tag functions}

Pollen 标记使用的是我们在 @secref["添加_Pollen_命令"] 中第一次看到的 Pollen 命令语法。在此之前，我们用这种语法来调用 @racket[define] 和 @racket[->html] 等函数。这种语法上的一致性是故意的，因为Pollen 标记是用来调用一种特殊的函数，叫做 @defterm{tag 函数}，这种函数默认是给文本添加一个标签。

要看这是如何工作的，把你的 @filepath{article.html.pm} 文件恢复到原来的状态。

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend RacketCon this year.
}]


我们可以使用 Pollen 标记添加任何标签，但现在，让我们从一个旧的最爱开始：@code{em}，它在 HTML 中用于增加文本的重点。我们以菱形字符 (◊) 开头，后跟标签名称 @code{em}，然后是花括号中的文本来应用标签，如下所示：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊em{RacketCon this year}.
}]

在 DrRacket 中运行此文件并查看生成的 X 表达式：

@repl-output{'(root "I want to attend " (em "RacketCon this year") ".")}


听到可以在彼此之间嵌套标签，您不会感到惊讶：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊em{RacketCon ◊strong{this} year}.}]

与预期的结果：

@repl-output{'(root "I want to attend " (em "RacketCon " (strong "this") " year") ".")}

@subsection{Attributes（属性）}

@defterm{属性}就像标签的标签。每个属性都是一个键值对，其中键是任意名称，值是字符串。任何看过 HTML 的人都熟悉它们：

@terminal{<span class="author">Prof. Leonard</span>}

这里，@code{class} 是 @code{span} 的属性，其值为 @code{"author"}。这就是它作为 X 表达式的样子：

@repl-output{'(span ((class "author")) "Prof. Leonard")}

您可以向标签添加任意数量的属性（首先作为 HTML，然后作为 X 表达式）：

@terminal{<span class="author" id="primary" living="true">Prof. Leonard</span>}

@repl-output{'(span ((class "author")(id "primary")(living "true")) "Prof. Leonard")}

在 Pollen 标记中，属性具有相同的逻辑，但语法略有不同。为了与您刚刚看到的标记符号保持一致，@code{span} 标记以通常的方式添加：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
◊span{Prof. Leonard}}]

然后，您有两个添加属性的选项。详细方式对应于属性在 X 表达式中的显示方式：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
◊span['((class "author")(id "primary")(living "true"))]{Prof. Leonard}
}]

每个键值对都在小括号里，然后键值对的列表也在小括号里，前面有一个 @racket[单引号] (@litchar{'})，表示文本应该按字面意思使用。

但这很无聊，因此 Pollen 还允许您使用 Racket 样式的 @seclink["keyword-args" #:doc '(lib "scribblings/guide/guide.scrbl")]{关键字参数} 在标签函数中指定属性：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
◊span[#:class "author" #:id "primary" #:living "true"]{Prof. Leonard}
}]

在这种形式中，每个属性名称都以@litchar{#:} 为前缀，表示一个关键字参数。和以前一样，属性值在关键字名称后面的引号中。

@margin-note{默认情况下，此关键字表示法适用于任何标签。当您制作自定义标签函数时，如果您希望标签函数以相同的方式支持关键字表示法，请使用 @racket[define-tag-function]（而不是通常的 @racket[define]）。}

这两种形式都会产生相同的 X 表达式：

@repl-output{'(span ((class "author")(id "primary")(living "true")) "Prof. Leonard")}


既然您知道如何制作标签和属性，您可能想知道 Pollen 标记是否可以用作快速而肮脏的 HTML 符号系统。当然——对于一个快速而肮脏的项目，为什么不呢。回想一下，@secref["X_表达式"] 只是 HTML 中使用的标准尖括号表示法的替代表示法。所以如果你想要这样的 HTML：

@terminal{<div class="red" style="font-size:150%">Important <em>News</em></div>}

您可以像这样在 Pollen 标记中编写它：

@code{◊div[#:class "red" #:style "font-size:150%"]{Important ◊em{News}}}

然后将它（使用@racket[->html] 函数）转换成上面的HTML。因此，您已经知道（并且喜欢？）的标签可以在 Pollen 标记中使用，但击键次数更少，操作也更简单。

不过，如果 Pollen 标记只是 HTML 标签的一个替代符号系统，那就太无聊了。正如我在上面提到的，这仅仅是使用它的最简单方法。

本着 XML 精神，Pollen 标记允许您使用任何您想要的标记。这就不那么无聊了。

@subsection{可选阅读：自定义标签有什么用？}

XML jocks 可以跳过这一部分，因为您已经知道了。但是，如果您一直生活在 Markdown / HTML 的低地，请继续阅读。

从广义上讲，标签是一种用额外信息注释文本的方法，我将其称为@defterm{metadata}（使用该术语的一般意义，而不是任何复杂的计算机方式）。元数据是使作者能够利用@defterm{语义标记}和@defterm{格式独立}的好处写一本书的关键工具。

@subsubsection{Semantic markup（语义标记）}

@defterm{语义标记} 意味着根据文本的含义向文本添加元数据，而不仅仅是其预期的视觉外观。因此，与其用@code{em} 标签标记@code{RacketCon}，就像我们在上面所做的那样来指示单词的外观，也许我们应该用 @code{event} 标签来标记它，来表示它是什么 @italic{类型} 的东西。

语义标记让作者可以指定在纯视觉术语中会模棱两可的区别，从而捕捉更多的意义和意图。例如，在书籍中，斜体样式通常应用于许多不相关类型的信息：强调的单词、电影标题、首次使用的术语、标题、说明和标签等。在非语义格式化方案下，也许有人会将它们全部标记为@code{em}。但在语义方面，可以酌情标记它们为@code{movie-title}、@code{first-use}、@code{heading}。

这有两个主要好处。首先，通过分离外观和意义，作者可以以有用的方式管理书籍的内容。例如，如果每个电影标题都被标记为@code{movie-title} 而不是@code{italic}，那么生成书中提到的所有电影的列表（为了作者的利益）或页面将很简单电影参考索引（为了读者的利益）。但如果没有这种语义标记，就无法将电影标题与任何其他斜体文本区分开来。

@subsubsection{格式独立}

自定义标签的第二个好处是 @defterm{格式独立}，或者改变文本的呈现以适应特定设备或上下文的能力。 

 当文本被特定格式的可视标签（例如 HTML 标签）包裹时，文档标记就会与单一的输出格式纠缠在一起。如果您只需要一种输出格式，那很好。

 但越来越多的图书作者被要求以多种格式发布他们的作品：纸质和 PDF，还有网络、电子书或其他本地数字格式，它们连接到具有不同显示功能的设备。

 @margin-note{是的，我知道其中许多格式都是基于 HTML 的变体。但是您可以在桌面网络浏览器中使用的 HTML 与您可以在 Kindle @code{.mobi} 文件中使用的 HTML 完全不同。 @code{.mobi} 文件还有其他技术要求，例如 @code{.ncx} 和 @code{.opf} 文件。因此，尽管存在一些遗传亲属关系，但这些类似 HTML 的格式最好被理解为独立的目标。}

使用显示驱动模型来管理这种复杂性是一个糟糕的想法——任何尝试过它的人都可以证明这一点。从一种基于显示的文件类型转换为另一种（例如，将文字处理器转换为 HTML，或将 HTML 转换为 PDF）是一种令人沮丧的做法，也是一种令人望而却步的期望。

这并不奇怪。长期以来，文本处理一直由这种显示驱动的模式主导。大多数文字处理器，如Microsoft word和Pages，都是围绕这种模式构建的。在大多数文档最终都要打印在纸上（或者像 PDF 这样的纸张模拟器）的时代，它工作得很好。HTML是技术上的飞跃，但不是概念上的飞跃：它主要代表web浏览器中可用的显示选项。

@margin-note{房间后面有几个 TeX 粉丝在挥舞着他们的手臂。是的，TeX 做对了很多事情。然而，在实践中，它从未成为电子出版的核心工具（公平地说，在编写 TeX 时，电子出版还不存在）。但是 Pollen 中的很多想法都是从 TeX 中提取的。}

要使一个文件具有格式独立性，必须满足两个条件。

首先，该文档必须能够被其他程序读取，这样它们才能将与格式无关的标记转换为特定格式的渲染（例如，将 @code{movie-title} 等语义标签映射到 @code{em}）等视觉标签上。大多数文字处理器格式（例如 Word 的 @code{.docx}）不利于创作，因为这些格式不透明且具有专有性。我们没有必要去讨论政治上的反对意见。作为一个实际问题，它们的限制性是毋庸置疑的——如果你不能把你的数据从你的文件中取出来，你就被困住了。

其次，文档本身必须以一种独立于任何一种格式的特殊性的方式来表示。例如，HTML 是一种糟糕的创作格式，因为它鼓励作者在他们的文本中乱扔诸如 @code{h1} 和 @code{span} 之类的 HTML 术语。这些在 HTML 之外没有任何意义，因此总是会导致转换问题。 @seclink["the-case-against-markdown"]{同样适用于 Markdown}，它只是伪装成了 HTML。



第一个条件的解决方案是使用基于文本的标记而不是基于专有文件类型的。第二个条件的解决方案是让作者为文档定义自定义标签，而不是相反。 Pollen 标记包含了这两个想法。


@subsection{使用自定义标签}

您可以使用与任何其他标签相同的语法插入自定义标签。假设您想使用 @code{event} 标签来标记事件。你会像这样插入它：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊event{RacketCon} this year.}]

这个标记将变成这个 X 表达式：

@repl-output{'(root "I want to attend " (event "RacketCon") " this year.")}

这相当于这个 HTML-ish 标记：

@terminal{<root>I want to attend <event>RacketCon</event> this year.</root>}

事实上，Pollen 没有注意到自定义标签、标准 HTML 标签或任何其他类型的标签之间的差异。它们都只是标记标签。如果您想将自己限制在特定的标签词汇中，您当然可以。如果你想设置 Pollen 来强制执行这些限制，你也可以这样做。但默认情况下，Pollen 不会施加这样的限制。一般来说，你可以选择任何你想要的标签名称，它都可以工作。

不要相信我的话。不如看看当你写完这个并运行它时，会发生什么：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊long-and-impractical-tag-name{RacketCon} this year.}]

这条规则有一个小但重要的例外。如果你想知道为什么我有时称它们为@defterm{tag functions} 而不仅仅是@defterm{tags}，那是因为在底层，每个标签都是作为一个函数实现的。此函数的默认行为只是将文本包装在具有给定名称的标签中。

将标签视为函数的好处将在本教程的后面部分变得显而易见。但是这种方法的代价是标签与 Pollen （以及 Racket）中可用的其他函数占用了相同的命名空间。这意味着，如果您尝试使用已用于现有函数的标签名称，您将收到错误消息。

例如，假设我们尝试使用名为 @code{length} 的自定义标签：

@fileblock["article.html.pm" @codeblock{
#lang pollen
 
The Panama Canal is ◊length{77km} across.}]

When we run this file, we get an error:

@errorblock{length: contract violation;
  expected: list?
  given: "77km"}

问题是 Racket 已经有一个名为 @racket[length] 的函数。根据 Pollen 命令符号的通常规则，您的命令被解释为尝试调用 @racket[length] 函数，而不是应用名为 @tt{length} 的标签。

在实践中，命名空间冲突很少见。但如有必要，它们很容易解决（对于最简单的方法，请参阅@secref["Invoking_tag_functions"]）。


@subsection{选择自定义标签}

你刚才看到，使用自定义标签很容易。另一方面，选择自定义标签，与其说是科学，不如说是艺术。作为作者，这取决于你。一些准则:

@itemlist[


@item{@bold{你永远不会做错。}我想确保你知道语义标记的情况。但是，如果直接使用 HTML 标签会让您的生活更轻松，那就继续吧。}

@item{@bold{反复迭代标签。}不必担心第一次就正确处理所有标签。就像你写完后再重写一样，添加现在看起来的标签，然后改变或增加它们，因为……}

@item{@bold{标签是从写作中出现的。} 试图事先指定所有的标签是不太可能的。在你写作的过程中，你会了解到一些关于文本的东西，这些东西会建议你使用新的标签。}

@item{@bold{最好的标签系统是你会坚持使用的。} 标签不是免费的午餐。始终如一地插入它们需要付出努力。不要为过于雄心勃勃的标签方案而烦恼，这会让您感到厌烦而不是帮助。}

@item{@bold{对于样板，标签比文本快。} 如果您发现自己以某种方式反复格式化某些文本（例如，列表和表格），则提取内容并将其包装在封装样板的标签中。}

]

And most important:

@itemlist[


@item{@bold{标签是函数。}正如我@seclink["Tags___tag_functions"]{上面提到的}，每个标签背后都有一个函数，它使用标签的内容作为输入。默认标签函数只输出标签及其内容。但是您可以用任何类型的函数替换它。因此，在实践中，您可以将大量劳动力转移到标签上。 }
]



 正如我们将在下一节中看到的，这是您的书真正变得可编程的地方。

@section[#:tag "tags-are-functions"]{标签是函数}

@(noskip-note)

如果您使用过 HTML 或 XML，则标签只是标签：您在文档中输入的内容看起来与输入时相同。标签可用于选择文档元素或指定样式（通过 CSS）。但它们对文档内容没有任何更深层次的影响。

在 Pollen 中情况并非如此。在底层，Pollen 只是在 Racket 编程语言中编写代码的另一种方式。而标签，不是惰性标记（inert markers），实际上是函数。

我想你们中的大多数人都知道函数（function）是什么，但为了安全起见——在编程中，@defterm{function} 是一段代码，它接受一些输入，处理它，然后返回一个值。要求一个函数处理一些数据被称为@defterm{调用 (calling)}这个函数。 

让我们看看 Pollen 标签的三个黄金法则:

@itemlist[#:style 'ordered


@item{@bold{每个 Pollen 标签都调用一个同名的函数。}}

@item{@bold{该函数的输入值是标签的属性和元素。}}

@item{@bold{整个标签（标签名称、属性和元素）会被调用函数的返回值替换。}}

]

规则 #3 的推论：因为一个标签代表一个单一的 X 表达式，一个标签函数也必须返回一个单一的 X 表达式。如果要返回多个元素，则必须将它们全部包装在单个 X 表达式中。 

@margin-note{推论的推论：您可以使用 Pollen 的特殊拼接运算符 (@racket[\@]) 作为返回值的标记，将其元素提升到包含的 X 表达式中。}

您已经在 Pollen 文档中看到过最简单的函数：@seclink["Tags___tag_functions"]{默认标签函数}，它模拟标准标记标签的行为。

 让我们在黄金法则的帮助下重温前面的例子：


@fileblock["article.html.pm" @codeblock{
#lang pollen
 
I want to attend ◊em{RacketCon ◊strong{this} year}.}]

运行此源文件时会发生什么？ Pollen 从内到外使用输入 @code{"this"} 调用标签函数 @code{strong}。结果是@code{(strong "this")}。然后 Pollen 使用三个输入值 @code{"RacketCon " (strong "this") " year"} 调用标签函数 @code{em}，从而产生 @code{(em "RacketCon " (strong "this") " year")}。最后，Pollen 使用文档中的所有内容调用标签函数 @code{root}，结果是：

@repl-output{'(root "I want to attend " (em "RacketCon " (strong "this") " year") ".")}

@subsection{将行为附加到标签}

有时这种默认行为就足够了。但其他时候，您会想要更改标签的行为。为什么？以下是一些有用的示例，说明您作为作者可以使用自定义标签函数进行哪些操作：

@itemlist[

@item{自动检测交叉引用并添加超链接。}

@item{从外部源中提取数据。}

@item{生成表格、图形和其他精美的布局对象。}

@item{根据给定条件更改内容。}

@item{自动检测换行符、段落和列表。}

@item{插入模板文本。}

@item{任何烦人或重复的事情。}

@item{数学计算。}

@item{… 以及您喜欢用编程语言做的任何其他事情。}
]


你如何改变标签的行为？两步：

@itemlist[#:style 'ordered 
@item{写一个新函数。}
@item{给它起一个标签的名称。}]

一旦你这样做了，当你使用标签时，这个新的行为将被自动调用。

例如，让我们在上面的示例中重新定义 @code{strong} 标签以简单地打印 @racket{BOOM}：

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (strong word) "BOOM")

I want to attend ◊em{RacketCon ◊strong{this} year}}]

当你运行这个文件时，你会得到：

@repl-output{'(root "I want to attend " (em "RacketCon " "BOOM" " year"))}

这是如何运作的？让我们看看我们的新函数定义。像往常一样，我们从菱形字符 (@litchar{◊}) 开始表示 Pollen 命令。然后我们使用@racket[define] 来引入一个函数定义。接下来是函数的名称，它需要与我们的标签名称 @code{strong} 匹配。表达式@racket[(strong word)] 的意思是``这个函数的名字是@racket[strong]，它接受一个单词作为输入，我们将其称为@racket[word]。'' 最后我们有返回值，即@racket["BOOM"]。

@margin-note{此示例使用 Racket 样式的命令定义函数。在这个简单的例子中，你也可以使用 Pollen 风格的命令，例如，@code{◊define[(strong word)]{BOOM}}。但总的来说，使用 Racket 式命令定义函数更加灵活。}

让我们再次运行这个文件，但回到黄金法则来了解会发生什么。从内到外工作：

@fileblock["article.html.pm" @codeblock{
#lang pollen

◊(define (strong word) "BOOM")

I want to attend ◊em{RacketCon ◊strong{this} year}}]

@itemlist[#:style 'ordered
@item{Pollen 使用输入 @code{"this"} 调用函数 @code{strong} — 和以前一样。但这一次，@racket[strong] 函数的结果不是 X 表达式 @code{(strong "this")}，而只是 @racket{BOOM}。}

@item{然后 Pollen 使用三个输入值 @code{"RacketCon " "BOOM" " year"} 调用函数 @code{em}。因为@code{em} 仍然是一个默认的标签函数，它产生X 表达式@code{(em "RacketCon " "BOOM" "year")}。}

@item{最后，Pollen 使用文档中的所有内容调用 @code{root} 函数。}
]

结果：

@repl-output{'(root "I want to attend " (em "RacketCon " "BOOM" " year"))}

这个例子当然是人为的。但基本思想——使用标签名称定义函数——是 Pollen 可编程性的基础。 @bold{如果你明白这一点和黄金法则，你就会得到一切。}


@section[#:tag-prefix "tutorial-3"]{名词: 幕间休息}

上面是很多沉重的材料。但它也涵盖了 Pollen 中最重要的思想：@bold{每个标签都是一个函数}。恭喜你走到了这一步。

@margin-note{有经验的程序员可能想绕道到 @secref["programming-pollen"] 来了解更多关于标签函数的可能性。}

好消息是，当我们将这些新原则付诸实践时，本教程的其余部分会感觉更轻松。

抱歉，本教程比其他教程要长，但确实——这就是让 Pollen 与众不同的东西。如果你现在还没有热情，你应该@link["http://www.buzzfeed.com/search?q=puppies"]{去缓解一下心情}。 

否则，就准备好摇滚吧。


@section{组织函数}

在到目前为止的标签函数示例中，我们已经在使用它的源文件中定义了每个函数。这对于特定于特定文件的快速小函数很好。

但更多时候，您会希望使用现有代码库中可用的函数，并存储您自己的函数，以便其他源文件可以使用它们。

@margin-note{现在，我们只是从 Pollen 标记文件中调用函数。但正如您将在@seclink["fourth-tutorial"]{fourth tutorial} 中看到的，可以从任何类型的 Pollen 源文件调用任何函数。}

@subsection{使用 Racket 的函数库}

通过使用 @racket[require] 命令加载库，可以使用 Racket 扩展库中的任何函数。这将使其函数和值在当前源文件中可用，并使用通常的 Pollen 命令语法。例如，假设我们想使用来自@racketmodname[racket/math] 的值@racket[pi] 和函数@racket[sinh]：

@fileblock["article.html.pm" @codeblock{
#lang pollen
◊(require racket/math) 
π is close to ◊(number->string pi). 
The hyperbolic sine of π is close to ◊(number->string (sinh pi)).
}]

结果：

@repl-output{
'(root "Pi is close to " "3.141592653589793" "." "\n" "The hyperbolic sine of pi is close to " "11.548739357257748" ".")}

一个警告——你仍然在一个 Pollen 标记文件中，所以你调用的任何函数的返回值都必须产生一个字符串或一个 X 表达式，以便可以将其合并到文档中。这就是我们使用 @racket[number->string] 包装数值的原因。 （这类似于 @seclink["Setting_up_a_preprocessor_source_file"]{first tutorial} 中引入的限制，其中预处理器文件中使用的函数必须生成文本。）

如果你的函数产生不兼容的结果，你会得到一个错误。例如，看看当我们从上面的示例中删除 @racket[number->string] 时会发生什么。

@fileblock["article.html.pm" @codeblock{
#lang pollen
◊(require racket/math)
π is close to ◊|pi|. 
The hyperbolic sine of π is close to ◊(sinh pi).
}]

这将在 DrRacket 中产生错误：

@errorblock{
pollen markup error: in '(root "Pi is close to " 3.141592653589793 "." "\n" "The hyperbolic sine of pi is close to " 11.548739357257748 "."), 3.141592653589793 is not a valid element (must be txexpr, string, symbol, XML char, or cdata)}

但是，如果此代码作为 Pollen 预处理器文件运行，则不会产生错误，因为预处理器会自动将数字转换为字符串。如果您想验证这一点，请将后缀更改为 @code{.pp} 并再次运行该文件。


@subsection[#:tag-prefix "tutorial-3"]{介绍 @filepath{pollen.rkt}}

@(noskip-note)

随着您越来越习惯于使用标签函数将行为附加到标记，您可能希望创建一些可以在多个源文件之间共享的函数。 @filepath{pollen.rkt} 文件是同目录（包括子目录）的 Pollen 源文件自动导入的特殊文件。所以@filepath{pollen.rkt} 提供的每个函数和值都可以在这些 Pollen 文件中使用。

首先，使用@filepath{pollen.rkt} 不是强制性的。在 Pollen 源文件中，您始终可以使用 @racket[require] 导入函数和值（如上一节所示）。 @filepath{pollen.rkt} 只是更容易将一组通用定义导入到项目中的每个 Pollen 源文件。

其次，请注意@filepath{.rkt} 后缀，@filepath{pollen.rkt} 是包含 Racket 代码而不是 Pollen 代码的源文件。这是默认设置，因为虽然 Pollen 的表示法对于基于文本的源文件更方便，但 Racket 的表示法在您处理代码时更方便。

@margin-note{您仍然可以在 Racket 源文件中使用 Pollen 表示法。见@racketmodname[pollen/mode]。} 

第三，@filepath{pollen.rkt} 始终适用于同一目录中的 Pollen 源文件。但这是文件的最小范围，而不是最大范围。嵌套在子目录中的 Pollen 源文件将首先在自己的目录中查找@filepath{pollen.rkt}。但是如果他们找不到它，他们会在父目录中查找，然后是下一个父目录，依此类推。因此，默认情况下，项目根文件夹中的@filepath{pollen.rkt} 将应用于项目中的所有源文件。但是，当您将新的 @filepath{pollen.rkt} 添加到子目录时，它将应用于该子目录及以下的所有文件。

@margin-note{尽管特定于子目录的 @filepath{pollen.rkt} 将取代上层父目录中的那个，但您仍然可以使用 @racket[(require "../pollen.rkt")] 从上面提取定义，并使用 @racket[provide] 将它们传播到当前子目录中。例如，@racket[(provide (all-from-out "../pollen.rkt"))] 将重新导出父目录中的所有内容。}

让我们看看这在实践中是如何工作的。在与@filepath{article.html.pm} 相同的目录中，创建一个新的@filepath{pollen.rkt} 文件，如下所示：

@fileblock["pollen.rkt" @codeblock{
#lang racket
(provide author)
(define author "Trevor Goodchild")
}]

这里我们使用@racket[define] 函数（我们之前见过）设置@racket[author] 等于@racket["Trevor Goodchild"]。注意最后一步：与标准 Racket 规则一致，我们必须显式地 @racket[provide] （公开）新值，以便其他文件可以看到它（与 Python 不同，你在 Racket 中 @racket[define] 的东西默认是私有的，而不是公共的）。

然后更新旧的 @filepath{article.html.pm} 以使用我们的新 @racket[author] 值：

@fileblock["article.html.pm" @codeblock{
#lang pollen

The author is ◊|author|.
}]

在 DrRacket 中运行它，你会得到：

@repl-output{'(root "The author is " "Trevor Goodchild" ".")}

留在同一目录中，创建第二个 Pollen 源文件：

@fileblock["barticle.html.pm" @codeblock{
#lang pollen

The author is really ◊|author|?
}]

运行这个，你会得到：

@repl-output{'(root "The author is really " "Trevor Goodchild" "?")}

这就是它的全部内容。你可以看到 @filepath{pollen.rkt} 提供的值是如何在两个 Pollen 源文件中自动出现的。

您可以以相同的方式导入函数，包括标签函数。例如，为@racket[em] 添加一个函数：

@fileblock["pollen.rkt" @codeblock{
#lang racket
(require txexpr)
(provide author em)
(define author "Trevor Goodchild")
(define (em . elements) 
  (txexpr 'extra-big empty elements))
}]


我们这里有一个新的符号。请注意，我们将标签函数定义为@racket[(em . elements)] 而不是@racket[(em word)]。在最后一个输入参数之前使用点使其成为 @defterm{rest 参数}。这会将所有剩余的输入参数（无论有多少）放入一个列表中。一般来说，这是标签函数的最佳实践，因为您通常不会事先知道有多少元素将作为输入传递给函数（有关此的更多信息，请参阅@secref["the-text-body"] ）。

@racket[txexpr] 函数是来自@racket[txexpr] 包（与 Pollen 一起安装）中的一个实用程序。它从标签、属性列表和元素列表构建新的 X 表达式。

然后我们在源文件中使用我们的新标签函数：

@fileblock["article.html.pm" @codeblock{
#lang pollen

The ◊em{author} is ◊em{◊|author|}.
}]

与预期的结果：

@repl-output{'(root "The " (extra-big "author") " is " (extra-big "Trevor Goodchild") ".")}

顺便说一句，如果你只想@racket[provide] @filepath{pollen.rkt} 中的所有内容，你可以使用@racket[all-defined-out] 简写：

@fileblock["pollen.rkt" @codeblock{
#lang racket
(require txexpr)
(provide (all-defined-out)) ; provides `author` and `em`
(define author "Trevor Goodchild")
(define (em . elements) 
  (txexpr 'extra-big empty elements))
}]



@section{使用 @tt{root} 标签函数解码标记}

如您所见，运行 Pollen 标记文件时获得的 X 表达式总是以名为 @code{root} 的标签开头。您可以像任何其他标签一样将自定义标签函数附加到 @code{root} — 通过创建一个新函数并将其命名为 @code{root}。 

例如，您可以做一些简单的事情，比如更改输出 X 表达式的名称：

@fileblock["article.html.pm" @codeblock|{
#lang pollen
◊(require txexpr)
◊(define (root . elements)
   (txexpr 'content empty elements))

The ◊code{root} tag is now called ◊code{content}.
}|]

导致：

@repl-output{'(content "The " (code "root") " tag is now called " (code "content") ".")}

与文档中的其他标签不同，@code{root} 包含文档的全部内容。因此，您附加到 @code{root} 的函数可以对所有内容进行操作。

出于这个原因，使用附加到@code{root} 的标签函数可以做的最有用的事情之一就是@defterm{decoding} 页面的内容。通过解码，我的意思是在计算完页面内的全部标签之后发生的任何内容后处理。

解码是自动完成以下操作的好方法：

@itemlist[

@item{基于空格检测换行符、段落和列表项。}

@item{断字。}

@item{排版优化，例如智能引号、破折号和连字。}

@item{为索引或交叉引用收集数据。}

@item{任何文件增强函数，a）可以用程序处理，b）你不希望在你的源文件中硬编码。}

]

举个例子，让我们以我最喜欢的一种——换行和段落检测为例。在 XML 和 HTML 创作中，您必须手动插入每个 @code{<br />} 和 @code{<p>} 标记。这非常乏味，使源文件混乱，并使编辑成为一件苦差事。

相反，让我们制作一个解码器，它允许我们在源代码中用一个换行符表示换行符，用双换行符表示一个段落换行符。这是一些带有单换行符和双换行符的示例内容：

@fileblock["article.html.pm" @codeblock|{
#lang pollen

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

因为我们还没有解码器，这些换行符只是被通过了：

@repl-output{'(root "The first line of the 'first' paragraph." "\n" "And a new line." "\n" "\n" "The second paragraph --- isn't it great.")}

当此 X 表达式转换为 HTML 时，换行符将持续存在：

@terminal{<root>The first line of the 'first' paragraph.\nAnd a new line.\n\nThe second paragraph --- isn't it great.</root>}

但在 HTML 中，原始换行符显示为一个空格。因此，如果您在项目服务器中查看此文件，您将看到：

@browser{
The first line of the 'first' paragraph. And a new line. The second paragraph --- isn't it great.
}


这不是我们想要的。

因此，我们需要制作一个解码器，将源代码中的换行符转换为 HTML 输出端的换行符和段落符。为此，我们使用 @racket[decode-elements] 函数，它提供了处理文档中内容类别的钩子。

将基本的@racket[decode-elements] 添加到源文件中，如下所示：

@fileblock["article.html.pm" @codeblock|{
#lang pollen
◊(require pollen/decode txexpr)
◊(define (root . elements)
   (txexpr 'root empty (decode-elements elements)))

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

在这里，我们将保留标签名称@code{root}，将属性保留为@code{empty}，并通过我们解码后的元素列表。

@margin-note{Racket jocks：您也可以使用 @racket[quasiquote] 和 @racket[unquote-splicing] 语法将其编写为 @code|{`(root ,@(decode-elements elements))}|。 @racket[txexpr] 包只是完成任务的另一种方式。}

如果你运行这个文件，有什么变化？对——没什么变化。这是因为默认情况下，@racket[decode-elements] 会让内容原封不动地通过。

我们通过给@racket[decode-elements] 一个处理函数的名称并将它附加到我们想要处理的内容类型来改变它。在这种情况下，我们很幸运——@racket[decode] 模块已经包含了一个 @racket[decode-paragraphs] 函数（也可以检测换行符）。我们使用关键字参数 @racket[#:txexpr-elements-proc] 添加这个函数，它是“用于处理标记 X 表达式的元素的函数”的缩写：

@fileblock["article.html.pm" @codeblock|{
#lang pollen
◊(require pollen/decode txexpr)
◊(define (root . elements)
   (txexpr 'root empty (decode-elements elements
     #:txexpr-elements-proc decode-paragraphs)))

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

现在，当我们运行该文件时，X 表达式已更改为包含两个 @racket[p] 标签和一个 @racket[br] 标签：

@repl-output{'(root (p "The first line of the 'first' paragraph." (br) "And a new line.") (p "The second paragraph --- isn't it great."))}

这意味着当我们转换为 HTML 时，我们会得到我们想要的标签：

@terminal{<root><p>The first line of the 'first' paragraph.<br />And a new line.</p><p>The second paragraph --- isn't it great.</p></root>}

所以当我们在项目服务器中查看这个时，换行符和分段符显示正确：

@browser{
The first line of the 'first' paragraph.
And a new line. 

The second paragraph --- isn't it great.
}


当然，在实践中，您不会将解码函数放在单个源文件中。您可以通过将其放在@filepath{pollen.rkt} 中使其对所有源文件可用。现在让我们这样做：

@fileblock["pollen.rkt" @codeblock{
#lang racket
(require pollen/decode txexpr)
(provide root)
(define (root . elements)
   (txexpr 'root empty (decode-elements elements
     #:txexpr-elements-proc decode-paragraphs)))
}]

我们还将@filepath{article.html.pm} 的源代码恢复到其原始的简化状态：

@fileblock["article.html.pm" @codeblock|{
#lang pollen

The first line of the 'first' paragraph.
And a new line.

The second paragraph --- isn't it great.
}|]

这一次，@filepath{article.html.pm} 将从@filepath{pollen.rkt} 中拉入@racket[root] 的标签函数。否则，代码没有改变，所以在项目服务器中的结果将是相同的：

@browser{
The first line of the 'first' paragraph.
And a new line. 

The second paragraph --- isn't it great.
}

但是等等，那些直引号看起来很糟糕。另外，一个破折号的三个连字符？野蛮。

让我们升级我们的解码器来处理这些。在@racket[pollen/misc/tutorial] 中，我储存了我们需要的两个函数：@racket[smart-quotes] 和@racket[smart-dashes]。 .

然而，这一次，我们将把它们附加到@racket[decode-elements] 的另一部分。 Smart-quote 和 smart-dash 转换只需要查看 X 表达式中的字符串。因此，不是将这些函数附加到@racket[decode-elements] 的@racket[#:txexpr-elements-proc] 参数，而是将它们附加到@racket[#:string-proc]，这样我们就可以指定一个应用于字符串的函数：

@fileblock["pollen.rkt" @codeblock{
#lang racket/base
(require pollen/decode pollen/misc/tutorial txexpr)
(provide root)
(define (root . elements)
   (txexpr 'root empty (decode-elements elements
     #:txexpr-elements-proc decode-paragraphs
     #:string-proc (compose1 smart-quotes smart-dashes))))
}]

因为@racket[#:string-proc]只接受一个函数（而不是两个），我们需要使用　@racket[compose1]　将　@racket[smart-quotes]　和　@racket[smart-dashes]　合并成一个函数（@racket[compose1]，来自Racket库，创建一个新函数，从右到左应用其参数列表中的每个函数）。

现在，如果我们在 DrRacket 中运行@filepath{article.html.pm}，我们可以看到新解码器函数的效果。引号是卷曲的，三个连字符变成一个破折号：

@repl-output{'(root (p "The first line of the ‘first’ paragraph." (br) "And a new line.") (p "The second paragraph—isn’t it great."))}

当然，这也显示在项目服务器中：

@browser{
The first line of the ‘first’ paragraph.
And a new line.

The second paragraph—isn’t it great.    
}

顺便说一句，通过@code{root} 标签解码通常是最方便的，但您不必那样做。解码只是您可以在任何标签函数中执行的特殊操作。所以你可以制作一个只影响页面上某个标签的解码器。或者您可以为不同的标签制作多个解码器。使用带有@code{root} 的解码器的优点是它可以影响所有内容，并且由于它附加到根节点，它始终是最后一个被调用的标签函数。


@section{把它们放在一起}

对于最后一个示例，我们将结合我们在前三个教程中学到的内容。虽然这个项目仍然很简单，但它总结了 Pollen 的所有主要概念。

它还提供了一个配方，您可以根据自己的项目进行调整，无论大小。例如，@italic{@link["http://practicaltypography.com"]{Butterick's Practical Typography}} 和 @italic{@link["http://typographyforlawyers.com"]{Typography for Lawyers}} 遵循此核心结构。

当我们浏览这些成分时，我将回顾每种成分的用途。将这些文件保存到项目服务器运行的单个项目目录中。

@subsection[#:tag-prefix "tutorial-3"]{@filepath{pollen.rkt} 文件}

该文件提供了自动导入同一目录下的 Pollen 源文件的功能。它是用标准的 Racket 编写的。 @filepath{pollen.rkt} 文件是可选的——没有它，你的标签将被视为默认标签函数。但是您可能会发现它是一种在您的项目中提供标签函数的便捷方式，包括附加到@code{root} 的@racket[decode] 函数。

在这里，我们将使用我们在上一节中设计的@filepath{pollen.rkt} 来为我们的源文件设置解码：

@fileblock["pollen.rkt" @codeblock{
#lang racket/base
(require pollen/decode pollen/misc/tutorial txexpr)
(provide root)
(define (root . elements)
   (txexpr 'root empty (decode-elements elements
     #:txexpr-elements-proc decode-paragraphs
     #:string-proc (compose1 smart-quotes smart-dashes))))
}]


@subsection{模板}

当您对内容使用 Pollen 创作模式时（使用 Markdown 语法或 Pollen 标记），您的源文件将生成 X 表达式。要将此 X 表达式转换为完成的文件，您需要使用模板。

默认情况下，当 Pollen 找到名为 @filepath{filename.ext.pm} 或 @filepath{filename.ext.pmd} 的源文件时，它将在您的项目目录中查找名为 @filepath{template.ext} 的模板，其中@filepath{.ext} 是匹配的输出扩展名。

在这个项目中，我们希望以 HTML 结尾，因此我们的源文件将被称为@filepath{filename.html.pm}，因此我们需要创建一个@filepath{template.html}。让我们使用我们在第二个教程中制作的修改版本。正如我们当时所做的那样，让我们​​添加空扩展名以清楚地表明它是一个输入文件，所以全名是@filepath{template.html.p}：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
<meta charset="UTF-8">
<title>◊select['h1 doc] by T. S. Eliot</title>
<link rel="stylesheet" type="text/css" media="all" href="styles.css" />
</head>
<body>◊(->html doc #:splice? #t)
◊(define prev-page (previous here))
◊when/splice[prev-page]{
<div id="prev">← <a href="◊|prev-page|">◊(select 'h1 prev-page)</a></div>}
◊(define next-page (next here))
◊when/splice[next-page]{
<div id="next"><a href="◊|next-page|">◊(select 'h1 next-page)</a> →</div>}
</body>
</html>
}]

@subsection{The pagetree}

页面树定义了一组输出文件之间的顺序和层次关系。模板使用页面树来计算导航链接（例如，上一个、下一个、上一个等）。页面树是可选的 - 如果您的项目中不需要导航，则不需要页面树。

但在这个项目中，我们确实需要导航。所以我们将像这样添加一个@filepath{index.ptree} 文件：

@fileblock["index.ptree"
@codeblock{
#lang pollen

burial.html
chess.html
sermon.html
}]

@subsection{A CSS stylesheet using the preprocessor}

我们上面的模板文件引用了一个名为 @filepath{styles.css} 的 CSS 文件。解析链接文件时，项目服务器不区分静态文件和动态文件。如果有一个名为 @filepath{styles.css} 的静态文件，它将使用它。

或者，如果您制作了一个名为@filepath{styles.css.pp} 的预处理器源文件，它将动态呈现到请求的@filepath{styles.css} 文件中。预处理器将对任何扩展名为@filepath{.pp} 的文件进行操作——因此名为@filepath{filename.ext.pp} 的预处理器源将被渲染为@filepath{filename.ext}。 （推论是预处理器功能可以添加到@italic{any} 类型的基于文本的文件中。）

预处理器源文件，如创作源文件，可以访问@filepath{pollen.rkt} 中的所有内容，因此您可以共享常用函数和变量。

让我们使用我们在第一个教程中制作的动态 CSS 文件的改进版本。

@fileblock["styles.css.pp"
@codeblock{
#lang pollen

◊(define inner 2)
◊(define edge (* inner 2))
◊(define color "gray")
◊(define multiplier 1.3)

body {
    margin: ◊|edge|em;
    border: ◊|inner|em double ◊|color|;
    padding: ◊|inner|em;
    font-size: ◊|multiplier|em;
    line-height: ◊|multiplier|;
}

h1 {
    font-size: ◊|multiplier|em;
}

#prev, #next {
    position: fixed;
    top: ◊|(/ edge 2)|em;
}

#prev {
    left: ◊|edge|em;
}

#next {
    right: ◊|edge|em;
}
}]

@subsection{The content source files using Pollen markup}

有了脚手架，我们就需要内容。我们的页面树包含三个输出文件——@filepath{burial.html}、@filepath{chess.html} 和 @filepath{sermon.html}。我们将使用 Pollen 标记制作这些输出文件。因此，我们将创建三个源文件并通过将 @filepath{.pm} 源扩展名添加到每个输出名称来命名它们 - 因此 @filepath{burial.html.pm}、@filepath{chess.html.pm}、和@filepath{sermon.html.pm}，如下（并向 T. S. Eliot 道歉）：


@fileblock["burial.html.pm" @codeblock[#:keep-lang-line? #f]{
#lang scribble/text
#lang pollen

◊h1{I. The Burial of the Dead}

"You gave me hyacinths first a year ago;
They called me the hyacinth girl."
--- Yet when we came back, late, from the Hyacinth garden,
Your arms full, and your hair wet, I could not
Speak, and my eyes failed, I was neither
Living nor dead, and I knew nothing,
Looking into the heart of light, the silence.
◊em{Od' und leer das Meer.}

Madame Sosostris, famous clairvoyante,
Had a bad cold, nevertheless
Is known to be the wisest woman in Europe,
With a wicked pack of cards.
}]

@fileblock["chess.html.pm" @codeblock[#:keep-lang-line? #f]{
#lang scribble/text
#lang pollen

◊h1{II. A Game of Chess}

And still she cried, and still the world pursues,    
"Jug Jug" to dirty ears.     
And other withered stumps of time    
Were told upon the walls; staring forms
Leaned out, leaning, hushing the room enclosed.  
Footsteps shuffled on the stair,     
Under the firelight, under the brush, her hair   
Spread out in fiery points   
Glowed into words, then would be savagely still.
 
"My nerves are bad to-night. Yes, bad. Stay with me.     
Speak to me. Why do you never speak? Speak.  
What are you thinking of? What thinking? What?   
I never know what you are thinking. Think."
}]

@fileblock["sermon.html.pm" @codeblock[#:keep-lang-line? #f]{
#lang scribble/text
#lang pollen

◊h1{III. The Fire Sermon}

"Trams and dusty trees.  
Highbury bore me. Richmond and Kew   
Undid me. By Richmond I raised my knees  
Supine on the floor of a narrow canoe." 
 
"My feet are at Moorgate, and my heart   
Under my feet. After the event   
He wept. He promised 'a new start.'  
I made no comment. What should I resent?"
}]

@subsection{The result}

现在访问项目服务器并查看@filepath{burial.html}，它应该看起来像这样（该框将展开以适合您的浏览器窗口）：

@image/rp["burial.png" #:scale 0.8]

单击顶部的导航链接可在页面之间移动。我鼓励您更改源文件、样式表、模板或@filepath{pollen.rkt}，看看这些更改如何立即影响项目服务器中的页面呈现。 （您也可以更改 @filepath{index.ptree} 中的页面顺序，但在这种情况下，您需要重新启动项目服务器才能看到更改。）

这个页面不是网页设计的奇迹。但它在一个示例中向您展示了：

@itemlist[

@item{正在解码的 Pollen 标记——分段符、换行符、智能引号、智能破折号——通过@filepath{pollen.rkt} 将@racket[decode] 函数附加到@code{root} 节点。}

@item{Pollen 预处理器生成的 CSS 文件，它使用 @racket[define] 设置的数值及其数学转换来计算 CSS 元素的位置。}

@item{使用@filepath{template.html.p} 中的条件语句（@racket[when/splice]）根据需要出现和消失的导航链接，页面顺序由@filepath{index.ptree} 和名称定义使用@racket[select] 从每个源文件的@code{h1} 标记中提取的链接数量。}

]

@section{Third tutorial complete}

好吧，这是个庞大的教程。祝贺你顺利完成。

但你的收获是，你现在了解了 Pollen 发布系统的所有核心概念，包括最重要的概念： Pollen 标记的灵活性，以及标签和函数之间的联系。



