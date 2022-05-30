#lang scribble/manual

@title[#:tag "big-picture"]{总览}

Pollen 发布系统的关键组件和概念以及它们如何组合在一起的摘要。如果您已经完成了@secref["quick-tour"]，这将为您所看到的内容提供一些背景信息。如果您先阅读本文，即将发布的 @seclink["first-tutorial"]{tutorials} 会更有意义。


@section[#:tag "the-book-is-a-program"]{这本书是一个程序}

这是 Pollen 的核心设计原则。与此原则一致，Pollen 在功能、工作流程和项目结构上都采用了软件开发的习惯。

@itemlist[

@item{@bold{你是一个程序员。} 不要恐慌。但是让我们承认这一点——如果你的书是一个程序，那么你在某种程度上是在编程它。您无需了解任何编程即可开始使用 Pollen。但是你必须愿意学习一些编程思想。 （而那些使用过其他基于模板的 HTML 生成器的人可能不得不忘记一些事情。）}

@item{@bold{花粉项目由源文件+静态文件组成。} A @italic{源文件} 是一个可以编译产生特定输出的文件。 @italic{static file} 可以直接使用（例如，SVG 文件或网络字体）。通常，您书籍的文本内容将存在于源文件中，而其他元素将是静态文件。}

@item{@bold{源代码控制是个好主意。} 由于 Pollen 项目是软件项目，因此可以使用源代码控制和协作系统轻松管理它们，例如 @link["http://github.com"]{GitHub}。如果您是一名作家，请不要害怕这些系统——学习曲线通过修订和编辑跟踪得到回报，这比使用 Word 或 PDF 文件要容易得多。}

]

@section{一种语言，多种方言}

@itemlist[

@item{@bold{一切都是球拍。} Pollen 系统完全采用 Racket 编程语言构建。您的一些源文件将在 Racket 中。其他人将使用其中一种花粉语言方言。但在幕后，一切都变成了球拍代码。所以如果你打算在 Pollen 中做任何严肃的工作，你也需要学习一些关于 Racket 的基础知识（例如@other-doc['(lib "scribblings/quick/quick.scrbl")]）。}

@item{@bold{Pollen 语言基于 Scribble。} Scribble 是 Racket 语言的一种变体，它颠覆了通常的编程语法：Scribble 源文件不是嵌入文本内容的代码，而是嵌入代码的文本（这个想法来自@link["https://en.wikipedia.org/wiki/TeX"]{TeX})。 Pollen 语言改编自 Scribble。所以大多数关于 Scribble 的事情对 Pollen 也是正确的（参见 @other-doc['(lib "scribblings/scribble/scribble.scrbl")]）。}


@item{@bold{花粉语言是一组方言。} 花粉方言具有共同的句法和结构。但是它们在细节上有所不同，这使得它们更好地适应某些类型的源文件（例如，Pollen 的一种方言可以理解 Markdown；其他方言则不能）。使用适合手头任务的任何一种。}

]


@section{开发环境}

Pollen 开发环境具有三个主要部分：DrRacket 代码编辑器、项目服务器和命令行。

@itemlist[

@item{@bold{使用 DrRacket 编辑源文件。} DrRacket 是 Racket 的 GUI 代码编辑器。当然，您也可以使用通用文本编辑器。但是 DrRacket 可以让您立即运行您的源代码并查看它是否有效。} 

@item{@bold{使用 Pollen 项目服务器预览和测试网页。} Pollen 有一个名为 @seclink["Using_the_project_server"]{@defterm{project server}} 的内置开发 Web 服务器。启动项目服务器后，您可以在任何 Web 浏览器中预览您的网页，从而以最大的准确性对其进行测试。}

@item{@bold{编写文档。} 项目服务器可以识别和渲染 Scribble 文件，因此您可以在编写文档时将其用作预览工具。}

@item{@bold{从命令行渲染和部署。} 您的 Pollen 项目最终会呈现为一组静态文件（通常是 HTML 和相关资产）。这可以从命令行控制，因此您可以将其集成到其他脚本中。} 


]


@section{一种特殊的HTML数据结构}

与其他编程语言不同，Pollen（和 Racket）在内部用称为 @secref["X-expressions"] 的东西表示 HTML。 X 表达式只是一个表示 HTML @defterm{element} 的列表，表示具有开始标签、结束标签和介于两者之间的内容的事物。与 HTML 元素一样，X 表达式可以嵌套。与 HTML 元素不同，X 表达式没有结束标记，它们使用括号来表示开始和结束，文本元素放在引号内。

例如，考虑这个 HTML 元素：

@nested[#:style 'code-inset]{@verbatim{<body><h1>Hello world</h1><p>Nice to <i>see</i> you.</p></body>}}

作为 Racket X 表达式，可以这样写：

@nested[#:style 'code-inset]{@verbatim{(body (h1 "Hello world") (p "Nice to " (i "see") " you."))}}

更多关于 X 表达式的内容将被提及。但是几个优点应该已经很明显了。首先，没有多余的尖括号，X 表达式可以说比等效的 HTML 更具可读性。其次，X 表达式优于将 HTML 视为简单字符串，因为它保留了元素的内部结构。第三，X 表达式是 Racket 中的原生数据类型。


@section{花粉命令语法}

如上所述，Pollen 源文件不是嵌入了文本的代码，而是嵌入了代码的文本。 （有关更多信息，请参见 @secref["pollen-command-syntax"]。）

@itemlist[

@item{@bold{如果你会写文字，你就可以用 Pollen 编程。}这是真的。正如您在@secref["quick-tour"] 中已经发现的，这是一个有效的 Pollen 程序：
@codeblock{
#lang pollen
Bonjour, tout le monde: comment ça va?
}}

@item{@bold{命令以 ◊ 开头。} 一个简单的规则：如果 Pollen 源文件中的某些内容以 @litchar{◊} 开头，则将其视为命令；否则它被视为普通文本。}

@item{@bold{在花粉模式或球拍模式下编写命令。} 命令可以使用两种等效的符号系统：Pollen 面向文本的命令语法或标准 Racket 语法。}


@item{@bold{球拍中的所有东西也都在花粉中。} Pollen 不是一种淡化的``模板语言''。Racket 是一种完全配置的编程语言，并且每个 Racket 功能都可以在 Pollen 中使用。}

]


@section{预处理器}

The @italic{预处理器}是花粉中最简单的处理模式。

@itemlist[

@item{@bold{文本输出。} 预处理器扫描任何 Pollen 命令的源文件，解析它们，并将整个文件作为文本输出。}

@item{@bold{使用任何文本文件。} 您可以将预处理器与 HTML、CSS、Markdown、JavaScript、XML、SVG 或任何其他基于文本的文件（包括其他编程语言的源文件）一起使用。我希望这会让你有点震惊。}

@item{@bold{快速开始。} 因为它适用于任何文本文件，所以预处理器是试用 Pollen 的一种简单方法，因为您可以将其混合到现有项目的工作流程中，甚至只是一个文件。}

]

@section{模板化的源文件}

如果您想将特定页面格式应用于多个内容来源（就像在书中一样），您可以使用 Pollen @defterm{templates}。

@itemlist[

@item{@bold{模板可以是任何格式。}通常花粉模板是 HTML。但他们不必如此。模板可以生成任何类型的文件——基于文本的 (XML) 或非基于文本的 (PDF)。}

@item{@bold{Markdown 创作模式。} Pollen 具有内置的 Markdown 解析器，因此您可以将 Markdown 源导入 Pollen 出版物。}

@item{@bold{花粉标记。} 花粉标记允许您自由定义自己的标记标签，并将行为附加到它们。}

@item{@bold{混合源类型。}每个文本源在进入模板之前都会转换为X表达式。因此，在一个项目中可以有多个方言的源文件。}

]



@section{页面树}

类似于目录，@defterm{pagetree} 是一个特殊的 Pollen 源文件，它被转换为页面的分层列表。


@itemlist[

@item{@bold{导航} 页面树用于在 HTML 模板中提供导航链接（如 previous, next, up, top ）。}

@item{@bold{组织。} 可以使用多个页面树将项目划分为应单独处理的页面子集。}


]



