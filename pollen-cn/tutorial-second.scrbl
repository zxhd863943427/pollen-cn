#lang scribble/manual

@(require (for-label pollen/render racket/base pollen/core txexpr pollen/setup pollen/template pollen/pagetree sugar))

@(require "mb-tools.rkt")

@title[#:tag "second-tutorial"]{第二个教程：Markdown、模板和页面树}

在本教程中，您将使用 Pollen 发布用 Markdown 编写的多页文章。您将了解：

@itemlist[

@item{在预处理器中使用 Markdown 文件}

@item{X 表达式}

@item{Markdown 创作模式}

@item{模板}

@item{页面树}

]

如果您想以最短的时间了解 Pollen ，请尝试@secref["quick-tour"]。

@section[#:tag-prefix "tutorial-2"]{先决条件}

我假设您已经完成了@seclink["first-tutorial"]{first tutorial} 并且您了解如何在 DrRacket 中创建源文件并在项目服务器中查看它们。我不会像以前那样详细说明这些任务。

@section[#:tag "the-case-against-markdown"]{选读：针对 Markdown 的案例}

我知道人们喜欢 Markdown。我希望人们也喜欢 Pollen ，所以这就是 Pollen 支持 Markdown 的原因。 

但是我要表明我自己的观点：

我对 Markdown 在作家中的受欢迎程度感到困惑。我同意这是一种标记基本 HTML 的聪明且易读的方式。当然，这对于诸如网络评论之类的东西来说非常有用，在这些评论中，速度和简单性是主要优点。

然而，在较长形式的写作中，它的缺点变得很明显。与编程语言一样，最好的写作工具最大限度地提高表达的可能性，并最大限度地减少约束。但是 Markdown 受到很大限制。首先也是最糟糕的，Markdown 不是语义化的。它只知道格式，在这方面，它对 Microsoft Word 等工具并没有太大的改进。其次，即使作为格式化符号工具，它也仅限于 HTML 中允许的已经很小的一组格式化标签的一小部分。最后，作者不能自己扩展。

正如@secref["Backstory"] 中所解释的，Pollen 的动画原则是，20 年后，我们应该超越将 HTML 视为源格式的思维。由于 Markdown 只是伪装得很好的 HTML，所以投票给 Markdown 实际上是继续维持现状的投票（尽管尖括号较少）。对我来说，这还不够好。我已经准备好扩展工具以适应我的想法。我拒绝继续削减我的想法以适应工具。

综上所述，如果你真的更喜欢 Markdown，我不想从你的手指上撬开它。 Pollen 有很好的 Markdown 支持（完全归功于 Greg Hendershott 为 Racket 提供的出色的 @link["https://github.com/greghendershott/markdown/"]{Markdown 解析器}）。它使 Markdown 更有用。 

但是，让我们做个交易吧，Markdown 粉丝。在与您进行了超过一半的会面之后，您是否至少会考虑 @seclink["pollen-vs-xml"]{Pollen markup} 可能比 Markdown 更适合您？因为它可以标记您大脑中的任何内容，而不仅仅是 HTML 的子集？如果@secref["这本书是一个程序"]，那本书的源代码应该更像你的大脑，而不像 HTML（或 XML 或 LaTeX 或 ...）？

这就是我要说的。


@section{ Pollen 中的 Markdown：两种选择}

在 Pollen 中使用 Markdown 有两种方法：

@itemlist[#:style 'ordered
@item{通过预处理器发送 Markdown 文件。}
@item{使用 Markdown 创作模式。}]

如果您想最终得到一组可以传递给下游 HTML 转换器（或其他 Markdown-to-______ 转换器）的 Markdown 文件，预处理器方法会更好。

如果您希望最终获得 Markdown 以外的内容（例如，完成的 HTML 文件），则创作模式方法更好。

但我们将两者都考虑。

@subsection{使用 Markdown 和预处理器}

由于 Markdown 是基于文本的格式，因此您可以使用 Pollen 预处理器将编程功能添加到现有的 Markdown 文件中。 （如果您需要复习，请参阅 @seclink["first-tutorial"]{first tutorial} 中的 @secref["Working_with_the_preprocessor"]。）

假设我们有一个名为 @filepath{brennan.md} 的 Markdown 文件，我们想与预处理器一起使用。在 DrRacket 中创建这个文件，保存它，然后在那个目录中启动项目服务器。

@fileblock["brennan.md" 
@codeblock[#:keep-lang-line? #f]{
#lang pollen
My name is _Brennan_, and I enjoy:

+ boring sauce

+ 24 fish nuggets
}]

您将能够在项目服务器中看到此文件，但目前，它只是一个静态文件。 Pollen 没有对它做任何事情。

让我们改变它。与通常的预处理器实践一致，添加@tt{#lang pollen} 作为第一行，并附加@filepath{.pp} 文件扩展名，因此我们新的预处理器源文件如下所示：

@fileblock["brennan.md.pp" 
@codeblock{
#lang pollen

My name is _Brennan_, and I enjoy:

+ boring sauce

+ 24 fish nuggets
}]

返回项目服务器，您将看到新的文件名。当你点击它时，Pollen 会渲染一个新的@filepath{brennan.md} 文件，但它看起来和你之前的一样。

现在我们将使用 Pollen 命令更改一些值：

@fileblock["brennan.md.pp" 
@codeblock{
#lang pollen

◊(define sauce-type "fancy")
◊(define nugget-type "chicken")
◊(define nugget-quantity (* 2 2 3))

My name is _Brennan_, and I enjoy:

+ ◊sauce-type sauce

+ ◊nugget-quantity ◊nugget-type nuggets
}]

当您在项目服务器中重新加载此文件时，@filepath{brennan.md} 将重新生成，现在看起来像这样：

@terminal{
My name is _Brennan_, and I enjoy:

+ fancy sauce

+ 12 chicken nuggets}



除了通过预处理器运行 Markdown 文件，您还可以在 Pollen 中使用 Markdown 创作模式。如果您想最终得到呈现的 HTML 文件，这是更好的选择。

但首先，让我们停下来澄清一下创作模式的一般概念。


@subsection{创作模式}

尽管预处理器模式很有用，但它限制您将不同位置的文本块插入到现有文件中。

相比之下，Pollen 的@defterm{创作模式} 将整个源文件解析为称为@defterm{X-expression} 的特殊数据结构。然后，您可以以任何您喜欢的方式处理整个 X 表达式，并使用 @defterm{模板} 输出您喜欢的任何格式（或多种格式）。

与预处理器相比，创作模式提供了更多的抽象性和灵活性。当然，它也需要更多的努力来设置。

Pollen 提供了两种创作模式：一种使用 Markdown 语法（我们将在本教程后面介绍），另一种使用自由格式标记语法（我们将在 @seclink["third-tutorial"]{第三个教程} 中学习）。在这两种情况下，基本过程是相同的：1) 将源代码编译为 X 表达式，然后 2) 使用模板将该 X 表达式转换为目标文件格式。

@subsection{X 表达式}

@(noskip-note)

我尽可能避免使用书呆子术语。但在这种情况下，这个东西在整个 Racket 文档中被称为 @defterm{X-expression} ，这是有充分理由的。所以我也用这个词。现在最好让你适应环境。

X 表达式是一种在代码中表示基于标记的数据的方式。 X 表达式是基于 Lisp 的语言（如 Pollen 和 Racket）的固有语言。它们不存在于 Python、JavaScript 或 Ruby 中。

让我们从您熟悉的部分开始。我所说的“基于标记的数据”是指 HTML、XML 和 SVG 之类的东西。这个想法是您拥有由@defterm{tags} 包围的基于文本的数据。每个标签也可以有自己的@defterm{attributes}，由键和值组成。标签可以包含其他标签，从而创建树状结构。是吗？你知道我的意思：

@terminal{<body>
  <h1>Hello world</h1>
  <p class="first">Nice to <i>see</i> you.</p>
</body>}

X 表达式只是这些数据结构的一种简化的、通用的表示法——就像 Markdown 是 HTML 的一种简化的表示法一样。要查看关系，我们将把一个转换为另一个。

首先，我们把尖括号改成圆括号，并且只在标签外面使用：

@terminal{(body 
  (h1 Hello world /h1) 
  (p class="first" Nice to (i see /i) you. /p) 
/body)}

然后我们去掉了多余的结束标签，因为每个结束括号都足以标记标签的结尾：

@terminal{(body
  (h1 Hello world)
  (p class="first" Nice to (i see) you.))}

但是，这会在标签名称和内容之间产生歧义。所以我们将内容放在双引号内：

@terminal{(body
  (h1 "Hello world") 
  (p class="first" "Nice to" (i "see") "you."))}

至于@code{class} 属性，我们需要将它与标记标签和内容区分开来，所以我们将它移到另一对括号中：

@terminal{(body
  (h1 "Hello world") 
  (p ((class "first")) "Nice to" (i "see") "you."))}

空格并不重要，因此 X 表达式也可以不用换行符来编写：

@terminal{(body (h1 "Hello world") (p ((class "first")) "Nice to" (i "see") "you."))}

跳过一些无聊的细节，基本上就是这样。

那么为什么它被称为 X 表达式呢？ Lisp 语言由称为 S 表达式的单元构建而成，如下所示：

@terminal{(and (txexpr? x) (memq (get-tag x) (setup:block-tags)))}

S 表达式使用前缀表示法，其中每对括号包含一个列表。列表中的第一个元素命名一个函数，其他元素是该函数的参数。 （这是对@secref["Racket_basics__if_you_re_not_familiar_"] 的评论。）X 表达式只是采用 S 表达式表示法来表示标记，因此得名（@defterm{X} 是 @defterm{XML-like} 的缩写）。

对于处理基于标记的数据，X 表达式与其他方法相比具有一些明显的优势：

@itemlist[

@item{@bold{可读性。} X 表达式保留了基于标记的数据的语义，同时摒弃了臭名昭著的冗长。}


@item{@bold{树和字符串的混合体。} 大多数编程语言将基于标记的数据表示为字符串或 XML 树。两者都不是一个好的选择。该字符串不捕获数据的任何内部结构。 XML 树捕获结构，但隐藏数据元素的顺序性质。 X 表达式显示两者。}

@item{@bold{基于表达式的编程语言的理想匹配。} 除了一些符号细节之外，X 表达式通常只是 S 表达式的子集，它们是 Racket 的构建块。在 Racket 中处理 X 表达式可最大限度地提高灵活性并最大限度地减少 @link["https://projects.csail.mit.edu/gsb/old-archive/gsb-archive/gsb2000-02-11.html"]{yak-saving} 。}
]

@margin-note{鉴于 XML-ish 数据结构和 Lisp-ish 编程语言之间的密切关系，我无法解释为什么在 Internet 时代它们没有更频繁地配对。它们就像花生酱和果冻。}

在 Pollen 的创作模式中，您的源文件被编译成 X 表达式，然后将其注入模板并转换为输出。作为第一个示例，我们将看看 Markdown 创作模式。


@subsection{Markdown 创作模式}

让我们开始整理我们的多页文章。为简单起见，我将使用不切实际的简短示例文本。但是你可以使用任何你想要的 Markdown 内容。

我们希望使用 Markdown 创作模式来制作最终为 HTML 的文件。与 Pollen 文件命名约定一致（请参阅 @secref["Saving___naming_your_source_file"]），我们将从所需的输出文件名 @filepath{article.html} 开始，然后附加 Markdown 创作后缀，即 @filepath{ .pmd}。因此，在 DrRacket 中，创建一个名为 @filepath{article.html.pmd} 的新文件并在其中放入一些 Markdown：

@fileblock["article.html.pmd"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

在项目服务器中预览此文件之前，单击 DrRacket 中的@onscreen{Run} 按钮以查看文件生成的内容。你会看到这样的东西：

@repl-output{
'(root
  (h1 ((id "deep-thought")) "Deep Thought")
  (p "I am " (strong "so") " happy to be writing this."))
}

您现在应该能够将其识别为 X 表达式。在创作模式下，Pollen 将您的 Markdown 编译为相应的 HTML 实体，但随后将数据作为 X 表达式而不是完成的 HTML 提供。

根据您在上一节中学到的内容，很明显，这个 X 表达式将转换为如下所示的 HTML：

@terminal{
<root>
  <h1 id="deep-thought">Deep Thought</h1>
  <p>I am <strong>so</strong> happy to be writing this.</p>
</root>
}

``但是这个@code{root} 标签是什么？那不是 HTML。'' 包含其他 X 表达式的 X 表达式必须具有根标记。因此，本着显而易见的精神，Pollen 在创作模式下生成的每个 X 表达式都将以 @code{root} 开头。如果您不需要它，可以丢弃它（我们将在下面的 @secref[#:tag-prefixes '("tutorial-2")]{Templates} 中介绍它）。尽管您将在 @seclink["third-tutorial"]{third tutorial} 中了解到，@code{root} 还为进一步处理创建了一个有用的 hook ——它不是一个多余的附件。


@subsection{回顾：创作模式与预处理器模式}

在继续之前，让我们快速回顾一下创作模式与预处理器模式的不同之处。为此，让我们采用相同的 Markdown 内容，但这次将其放入名为 @filepath{article.md.pp} 的预处理器源文件中。

@fileblock["article.md.pp"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

当你在 DrRacket 中运行这个文件时，你会看到：

@repl-output{
Deep Thought
============

I am **so** happy to be writing this.
}

这个结果是有道理的，对吧？回顾一下：当您在预处理器模式下使用 Markdown 源时，Pollen 会为您提供 Markdown。当你在创作模式下使用 Markdown 源代码时，Pollen 会给你一个 X 表达式。

那么如何将 X 表达式转换为 HTML 呢？继续阅读。

@section[#:tag-prefix "tutorial-2"]{模板}

在 Pollen 中，@defterm{template} 允许您将 X 表达式转换为目标输出格式。如果您使用过其他网络发布系统，那么模板可能是一个熟悉的想法。 Pollen 中的模板既相似又不同。

首先，两个主要的相似之处：

@itemlist[

@item{在最简单的情况下，模板包含您希望在多个输出文件中重复使用的 @bold{boilerplate material}。例如，在一组 HTML 页面中，您的布局和导航元素可能保持不变，而内容会发生变化。在这种情况下，您可以将布局和导航放在模板中，并将内容保留在 Pollen 源文件中。当您想添加新页面时，您可以创建一个新的源文件并将其与现有模板一起使用。此外，如果要全局更改布局和导航，只需更改模板即可，而不是更改源文件。}

@item{ Pollen 模板和其他模板一样，也可以有@bold{条件元素}——这意味着，您可以在模板中嵌入简单的代码，允许它们根据页面中的内容进行更改。例如，一个模板可以显示或隐藏“上一页”和“下一页”链接，这取决于实际上是前一页还是下一页。 （如果这让您感到兴奋，请跳到 @seclink["third-tutorial"]{third tutorial}。）}

]

还有两个主要的不同之处：

@itemlist[

@item{这里@bold{没有特殊的模板语言}，还有神奇的语法等等需要你去学习。相反，您可以在模板中使用所有相同的 Pollen 命令，就像在创作模式或预处理器模式下一样。}

@item{在模板中，您必须将 X 表达式 @bold{显式转换} 为目标格式。这是一个功能，而不是一个错误。通过避免对目标格式的假设，Pollen 模板可用于生成任何类型的文件（甚至是 PDF 等二进制格式）。但是这种灵活性的代价是你需要告诉 Pollen 你想要什么。}
]


@margin-note{``所以模板也是 Pollen 源文件？''不完全是。更准确地说，它是 Pollen 源的片段，通过添加来自您的一个源文件的 X 表达式来完成。因此，您可以放入模板中的代码有一些额外的限制，尽管有简单的解决方法。}


要了解它是如何工作的，让我们回到上一节开始的源文件：

@fileblock["article.html.pmd"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

上次，我让你在 DrRacket 中运行这个文件来查看它产生的 X 表达式。这次，将其加载到项目服务器中。你会看到这样的东西：

@browser{
@bold{@larger{Deep Thought}}

I am @bold{so} happy to be writing this.
}

在这里，您可以看到源代码中的 X 表达式与 HTML 模板相结合，该模板为完成的 HTML 添加了必要的样板：

@terminal{
<html><head><meta charset="UTF-8" /></head><body><root><h1 id="my-article">Deep Thought</h1><p>I am <strong>so</strong> happy to be writing this.</p></root>
</body></html>
}

但是等等——模板是从哪里来的？如果您尝试在没有首先设置模板的情况下将文件呈现为 HTML，Pollen 会帮助您并将其@defterm{fallback template} 用于 HTML。后备模板只是作为最后手段使用的最小模板。在一般情况下，看到后备模板可能表明存在问题（例如，Pollen 找不到您要求的模板）。现在，我们可以依靠它来预览我们的文件。

尽管如此，我们还是可以通过学习 fallback 模板来学习一些关于如何制作 HTML 模板的知识。

@subsection{@tt{doc} 导出和 @tt{->html} 函数}

为了理解模板的必要部分，让我们看一个简单的——Pollen 用于 HTML 文件的备用模板，称为 @filepath{fallback.html}。

@fileblock["fallback.html"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊(->html (html (head (meta #:charset "UTF-8")) (body doc)))
}]

后备模板具有三个关键要素：

第一个成分是表示基本 HTML 页面的 X 表达式：

@codeblock[#:keep-lang-line? #f]{
#lang pollen
(html (head (meta #:charset "UTF-8")) (body))
}

该 X 表达式等效于以下 HTML 字符串：

@terminal{<html><head><meta charset="UTF-8"></head><body></body></html>}

但是我们如何从 X-expression 到 HTML 呢？方法如下：在模板中，我们将这个 X 表达式显式转换为 HTML。我们通过第二个关键成分函数@racket[->html 发送X 表达式来做到这一点]:

@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊(->html (html (head (meta #:charset "UTF-8")) (body)))
}

@racket[->html] 在@racketmodname[pollen/template] 模块中，但是这个模块会自动导入到每个模板中，所以我们不需要做任何额外的事情。

最后，我们需要包含源文件中的 X 表达式。按照惯例，每个 Pollen 源文件都通过名为 @code{doc} 的导出变量提供其输出。预处理器模式下的源文件将其文本结果放入@code{doc}。并且处于创作模式的源文件将其 X 表达式结果放在 @code{doc} 中。所以我们把第三个关键成分，变量@code{doc}，放在我们的@code{body}标签中。

@codeblock[#:keep-lang-line? #f]{
#lang pollen
◊(->html (html (head (meta #:charset "UTF-8")) (body doc)))
}

总结一下：这个模板包含一个骨架 HTML 页面（X 表达式格式）。我们将@code{doc} 放入模板中以指示应在何处插入源文件的 X 表达式。最后，我们使用 @racket[->html] 将整个 X 表达式转换为 HTML。

``所以我必须将我的 HTML 模板转换为 X 表达式？'' 不，这是可选的。您还可以将硬编码的 HTML 放入模板中。这是使用显式 HTML 编写 @filepath{fallback.html.p} 的等效方法：

@fileblock["fallback.html.p" @codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head><meta charset="UTF-8"></head>
<body>◊(->html doc)</body>
</html>
}]

请注意，我们仍然需要使用@racket[->html] 函数，但是这一次，它不是围绕一个更大的X 表达式，而是围绕@code{doc}。模板代码的其余部分以纯文本形式传递。

最重要的是以 HTML 结尾。否则，这两种方法之间没有区别。首先，您使用 X 表达式描述整个模板，并使用 @racket[->html] 转换整个模板。在第二个中，您对样板 HTML 进行硬编码，因此唯一需要使用 @racket[->html] 转换的部分是 @code{doc}。

使用最适合您的方法。我通常更喜欢第二种方法，因为我喜欢使用占位符内容手动构建 HTML 布局，以确保所有繁琐的部分都能正常工作。然后很容易将占位符内容替换为@racket[(->html doc)]，它就变成了一个模板。

另一方面，当您使用 X 表达式生成 HTML 时，结果总是完美的。您永远不必梳理模板来寻找缺少的关闭 @code{</div>} 标记。

@subsection{制作自定义模板}

在了解了三个关键模板成分之后，我们现在将为 @filepath{article.html.pmd} 创建一个自定义模板。 

通常，模板文件可以具有您想要的任何名称。但默认情况下，Pollen 将首先在您的项目目录中查找名为 @filepath{template.ext} 的文件，其中 @filepath{ext} 匹配源文件的输出文件扩展名。所以如果你的源文件是@filepath{database.xml.pmd}，Pollen 会寻找@filepath{template.xml}。对于@filepath{article.html.pmd}，Pollen 将寻找@filepath{template.html}。

@margin-note{如果您想使用具有不同名称的模板，或将特殊模板应用于特定源文件，您可以通过插入行 @code{◊(define-meta template "my-template) 从任何源文件中指定模板-name.html")}。有关更多信息，请参阅@racket[get-template-for]。}

此外，使用将产生@filepath{template.html} 的 Pollen 源文件也很好。按照上面的 fallback-template 示例，我们将使用空扩展名并将我们的模板命名为 @filepath{template.html.p} —— Pollen 会将其转换为它需要的 @filepath{template.html}。

除此之外，我们需要做的就是确保我们的模板具有我们在备用模板中看到的三个关键要素。当我们在项目服务器中查看时，Pollen 会自动将其应用到@filepath{article.html.pmd}。

在您的项目目录中，创建一个名为 @filepath{template.html.p} 的新文件：

@margin-note{如果您在 Mac OS 上使用 DrRacket 来保存此文件，它可能会坚持在文件名中添加 @filepath{rkt} 扩展名。如果是这样，您可以在保存文件后更正文件名，或者使用不同的文本编辑器创建@filepath{template.html.p}。} 


@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>Custom template</title>
</head>
<body>◊(->html doc)</body>
</html>
}]

@filepath{p} 扩展名是 Pollen 的 @seclink["Null___p_extension_"]{@defterm{空扩展}}，它执行您可能猜到的 —— 没有。如果模板被称为@filepath{template.html}，它仍然可以工作。但是使用空扩展名是一个良好的习惯，因为它使您的模板与输出文件不同（这是为了您和您的操作系统，这可能会被@filepath{template.pdf}之类的东西弄糊涂，这将是一个文本基于二进制扩展名的文件。）

否则，这与我们在上一节中看到的备用模板相同，但用 HTML 编写，并添加了 @code{title} 元素。在项目服务器中刷新@filepath{article.html}。看起来不一样吗？不——不会，因为模板基本相同。但是由于新的@code{title} 字段，您应该注意到浏览器窗口中显示的标题是@onscreen{Custom template}。这表明 Pollen 正确地依赖于我们的新模板文件，而不是备用模板。

让我们继续，在我们的自定义模板中添加一个 @code{style} 块：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>Custom template</title>
  <style type="text/css">
    body {padding: 3em; font-size: 20px;}
    h1 {background: gray; color: white;}
    strong {color: red;}
  </style>
</head>
<body>◊(->html doc)</body>
</html>}]

再次刷新项目服务器中的@filepath{article.html}。现在您会看到标题的背景是灰色的，文本中的一个单词是红色的。

随意将其他设置添加到@filepath{template.html.p}，或更新@filepath{article.html.pmd} 中的文本，并查看页面在浏览器中的变化。如您所料，项目服务器会同时关注您的源文件和模板文件，如果其中一个文件发生更改，它会重新生成输出文件。

@subsection{将特定源数据插入模板}

在前面的示例中，我们使用 @code{doc} 将源文件的全部内容（作为 X 表达式）插入到模板中。

但是，如果您只想将部分源文件插入到模板中怎么办？例如，如果每个页面上的标题是@onscreen{自定义模板}，你会看起来像个傻瓜。所以让我们解决这个问题。

当您在模板中工作时，Pollen 提供了一个 @racket[select] 函数，可以让您提取特定标签的内容，例如：@code{◊(select tag-name doc)}，这意味着 ``从@code{doc} 中获取 @racketvarfont{tag-name} 的内容并放在这里。''

假设我们宁愿使用文章的名称——@italic{Deep Thought}——作为页面标题。为此，我们将在@code{<title>} 标记内放置一个@racket[select] 命令。

为了使@racket[select] 工作，我们还需要知道包含标题的标签名称。如果我们有一点 Markdown 专业知识，我们可能已经知道这部分 Markdown 源代码：


@codeblock[#:keep-lang-line? #f]{
#lang pollen
Deep Thought
============
}

将生成一个名为 @code{h1} 的标签。

如果我们没有记住所有的 Markdown 转换怎么办？没问题。我们仍然可以通过在 DrRacket 中运行 @filepath{article.html.pmd} 源文件并查看生成的 X 表达式来找出标签名称：

@repl-output{'(root (h1 ((id "my-article")) "Deep Thought") (p "I am " 
(strong "so") " happy to be writing this."))}

无论哪种方式，现在我们都知道文本 @italic{Deep Thought} 存在于 @code{h1} 标记中。所以我们相应地更新我们的模板（为简洁起见，我将在这些示例中省略 @code{style} 标记，但可以保留它）：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc)</title>
</head>
<body>◊(->html doc)</body>
</html>
}]

当您在项目服务器中刷新页面时，页面标题现在将显示为 @onscreen{Deep Thought}。当然，您也可以在模板中组合静态和动态元素，如下所示：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
</head>
<body>◊(->html doc)</body>
</html>
}]

页面标题现在将是 @onscreen{Deep Thought, by MB}。

关于命令语法的一些注释。我们以 Racket 风格编写了 @racket[select] 和 @racket[->html] 命令。我们也可以用 Pollen 风格编写命令，如下所示：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊select['h1 doc], by MB</title>
</head>
<body>◊->html[doc]</body>
</html>
}]

这与前面的示例完全相同。欢迎怀疑者通过在项目服务器中重新加载页面来确认这一点。

最后，请注意在@racket[select] 命令中，标签名称@code{h1} 用引号(@code{'h1}) 编写，而@code{doc} 不是。这是一个容易出错的地方，但规则很简单：当您引用 @defterm{identifier} 时，不要使用引号 - 即现有函数或变量的名称（如@racket[select] 或@code{doc}）。但是当您将事物用作文字值时，您确实需要一个引号。


@margin-note{Racket（以及 Pollen）对 @secref["符号" #:doc '(lib "scribblings/guide/guide.scrbl")] （例如 @racket['h1]）和 @secref["字符串" #:doc '(lib "scribblings/reference/reference.scrbl")] （例如 @racket["h1"] ）进行了区别。在不深入研究的情况下，现在只要知道 X 表达式的标签总是一个符号，而不是一个字符串。但是如果你输入 @racketfont*{◊(@racket[select] "h1" doc)}，这个命令仍然可以工作，因为Pollen会把它当作 @racketfont*{◊(@racket[select] 'h1 doc)}，符合在意图明确的情况下对输入类型不挑剔的一般原则。}


@subsection{链接到外部 CSS 文件}

如果您是超级网络能手，您可能不会将 CSS 选择器放在 @code{<head>} 标记中。相反，您链接到外部 CSS 文件。因此，在 Pollen 中，您可以通过将常用的 @code{<link>} 标记添加到 HTML 模板（在本例中为名为 @filepath{styles.css} 的文件）来做到这一点，这不会让您感到惊讶：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)</body>
</html>
}]

手工编码 CSS 的粉丝，我相信你可以从这里开始：在你的项目目录中放置一个 @filepath{styles.css} 文件，然后享受结果。

但是在@seclink["first-tutorial"]{first tutorial} 期间关注的人可能想知道 ``我们可以链接到动态生成的 @filepath{styles.css.pp} 文件吗？''

是的当然。这是经验法则：当您在文件之间建立链接时——无论是 CSS、HTML 还是其他任何东西——Pollen 并不关心文件是静态的还是动态的。您只需通过其最终名称来引用它，在本例中为 @filepath{styles.css}。如果存在静态 @filepath{styles.css} 文件，Pollen 将使用该文件。如果没有，Pollen 将寻找可用于制作 @filepath{styles.css} 的源文件，并在现场生成它。 （您也可以从静态文件开始，稍后将其更改为动态文件，Pollen 会做正确的事情。）

因此，要使用动态 CSS 文件，我们不需要对 @filepath{template.html.p} 进行任何更改。我们只需要将@filepath{styles.css.pp} 添加到项目目录：

@fileblock["styles.css.pp"
@codeblock{
#lang pollen

◊(define h1-color "blue")
◊(define strong-color "green")

body {padding: 3em; font-size: 20px;}
h1 {background: ◊|h1-color|; color: white;}
strong {color: ◊|strong-color|;}
}]

这一次，当你在项目服务器中刷新@filepath{article.html} 时，Pollen 会运行@filepath{styles.css.pp} 来获取它需要的@filepath{styles.css} 文件，你会看到页面中的新颜色。像往常一样，如果您更新@filepath{styles.css.pp}，Pollen 会在您刷新页面时注意到并重新生成 CSS 文件。

你可以添加多个动态样式表吗？是的。
@(linebreak)你能混合动态和静态样式表吗？是的。
@(linebreak)你能添加一个动态的JavaScript文件吗？是的。

你有大致的想法，对吧？所以让我们继续。


@section[#:tag-prefix "tutorial-2"]{名词: 幕间休息}

如果你的文章只需要一页，你可以在这里停下来。您现在了解了使用创作模式发布单页文章所需的一切。您知道如何创建必需的成分（源文件和模板），还知道如何链接到可以动态生成的可选 CSS 文件。

但是，如果您想创建一篇多页文章，您需要了解一个更大的想法。这可能是休息的好时机。

@section[#:tag-prefix "tutorial-2"]{页面树（pagetree）}

A @italic{pagetree} 是 Pollen 页面的分层列表。当您的项目中有多个页面时，页面树会在这些页面之间建立关系。在最基本的情况下，页面树为页面建立了线性序列。但是页面树也可以建立层次关系——例如，可以将一本书长度的项目组织成章节，将章节组织成章节，等等。页面树不会对项目的组织施加任何语义。它只是一棵树，由你决定要建立多少层，这些层的含义等等。


@subsection{页面树导航}

页面树的一个明显用途是将导航链接添加到您的页面。显然，在多页文章中，读者需要一种从一页到下一页的方式。在本教程的这一部分中，我们会将示例文章从一页扩展到三页，并了解如何在模板中创建相对于当前页面动态生成的“上一页”和“下一页”链接页。


@subsection{使用自动页面树}

您实际上已经接触过页面树（尽管当时我没有告诉您）。回想一下，项目服务器的仪表板位于 @link-tt{http://localhost:8080/index.ptree}。您在仪表板中看到的文件列表是 Pollen 通过获取当前目录中的文件列表，并按字母顺序排列它们而生成的页面树。

因此，如果您的项目中的多个页面已经按文件名的字母顺序排序，那么您可以依赖此自动页面树。 （更常见的是，您将为导航创建一个单独的页面树文件 - 但我们将在本教程的后面部分介绍。）

在本教程的前面，您有一个名为 @filepath{article.html.pmd} 的 Markdown 源文件，如下所示：

@fileblock["article.html.pmd"
@codeblock{
#lang pollen

Deep Thought
============

I am **so** happy to be writing this.
}]

让我们通过为项目创建另外两个来补充这个源文件：

@fileblock["barticle.html.pmd"
@codeblock{
#lang pollen

Barticle Title
==============

The wonderful second part of the article.
}]

@fileblock["carticle.html.pmd"
@codeblock{
#lang pollen

Carticle Title
==============

The terrific third part.
}]

和以前一样，您可以使用您喜欢的任何示例 Markdown 内容填充这些源文件。此外，您不必使用文件名 @filepath{barticle.html.pmd} 和 @filepath{carticle.html.pmd} — 关键是预期的顺序需要与文件名的字母排序相匹配。

我们将重用本教程前面的 @filepath{template.html.p} 和 @filepath{styles.css.pp} 文件。移动或删除其他教程文件，以便项目服务器中的仪表板仅显示以下五个文件：

@itemlist[

@item{@filepath{article.html.pmd}}
@item{@filepath{barticle.html.pmd}}
@item{@filepath{carticle.html.pmd}}
@item{@filepath{styles.css.pp}}
@item{@filepath{template.html.p}}
]

如果您单击三个 Markdown 源中的任何一个，您将看到它使用 @filepath{template.html.p} 转换为 HTML，其样式来自生成的 @filepath{styles.css}。

这个项目的自动页面树正是您在仪表板中看到的：三个文章文件的列表，后跟@filepath{styles.css} 和@filepath{template.html.p}。

@subsection{使用 @tt{here} 向模板添加导航链接}

回想一下本教程前面的内容，源文件的内容通过特殊变量 @code{doc} 在模板中可用。同样，当前源文件的输出名称可通过特殊变量@code{here} 获得。

要制作任何导航链接——向上、向下、横向——一般的想法是我们使用@code{here} 作为页面树导航函数的输入，然后在当前页面树中查找答案。

首先，让我们单独看看@code{here}。像下面一样更新您的模板：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
</body>
</html>
}]

如果您刷新@filepath{article.html}，您现在将看到“当前页面名为article.html”。切换到@filepath{barticle.html}，您将看到 ``当前页面名为 barticle.html。'' 有道理，对吧？

请注意，@code{here} 始终是 @italic{output} 文件名，因为导航自然会在输出文件之间建立连接，而不是在源文件之间建立连接。在这种情况下，转换为输出名称意味着我们丢失了 @filepath{pmd} 扩展名。 （正如我们将在下面看到的，当我们是 @secref["制作页面树文件"] 时，我们还将使用这些输出文件名。）

现在让我们使用 pagetree 函数来显示上一页和下一页的名称。与通常的 Pollen 显而易见性策略一致，这些函数称为 @racket[previous] 和 @racket[next]：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
The previous is ◊|(previous here)|. 
The next is ◊|(next here)|.
</body>
</html>
}]

刷新@filepath{barticle.html}。您现在将看到 ``当前页面名为 barticle.html。前面是article.html。下一个是 carticle.html。'' 到目前为止，一切都很好：我们正确地从自动页面树中派生了前一页和下一页。

剩下的就是添加超链接：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
The previous is <a href="◊|(previous here)|">◊|(previous here)|</a>. 
The next is <a href="◊|(next here)|">◊|(next here)|</a>.
</body>
</html>
}]

刷新@filepath{barticle.html}，您会看到上一页和下一页的名称现在是指向这些页面的超链接。单击并说服自己它有效。

@margin-note{pagetree @secref["Navigation"] 的文档将告诉您可用于生成导航链接的其他功能。}

@subsection{使用条件处理导航边界}

如果您点击进入@filepath{article.html} 或@filepath{carticle.html}，您可能已经注意到一些问题。因为@filepath{article.html} 是自动页面树中的第一页，所以它没有可以链接到的任何前一页。 @filepath{carticle.html} 的下一页链接是 @filepath{styles.css}，这是完全正确的——事实上，它是自动页面树中的下一个文件——但它不是我们文章的一部分，所以我们宁愿在那里停止导航。

解决此问题的一种方法是使用三个单独的模板文件——标准的一个包含上一页和下一页链接，一个只有一个下一页链接，一个只有一个上一页链接。

但由于我们在 Pollen 中提供了一整套编程语言，所以这是一个弱解决方案。更好的方法是在模板中添加@defterm{conditionals} 以选择性地更改导航。这让事情变得简单，因为我们仍然只有一个 @filepath{template.html.p} 需要处理。

为了处理@filepath{article.html}，我们希望在没有上一页时隐藏上一页导航链接。事实证明，如果 @racket[previous] 函数找不到前一页，它将返回 false。所以我们只需要在@racket[when/splice] 命令中包装上一页导航，如下所示：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊when/splice[(previous here)]{The previous is 
<a href="◊|(previous here)|">◊|(previous here)|</a>.} 
The next is <a href="◊|(next here)|">◊|(next here)|</a>.
</body>
</html>
}]

@racket[when/splice] 的基本形式是 

@racketfont{◊when/splice[@racketvarfont{test-condition}]{@racketvarfont{content-to-insert}}} 

我们用Pollen风格写这个命令--注意@racketvarfont{condition}周围的方括号，以及@racketvarfont{text}周围的大括号。使用@racket[(previous here)]作为条件是对 ``when @racket[(previous here)] 不返回 false ...''的简写。

听众中的程序员可能对重复使用 @racket[(previous here)] 感到焦虑——欢迎您将该值存储在一个变量中，一切都会以同样的方式工作：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊(define prev-page (previous here))
◊when/splice[prev-page]{The previous is 
<a href="◊|prev-page|">◊|prev-page|</a>.} 
The next is <a href="◊|(next here)|">◊|(next here)|</a>.
</body>
</html>
}]

我们需要一种不同的技术来处理下一页导航的结尾，因为我们没有到达页面树的实际结尾。我们只是到达了我们浏览的页面的末尾。

什么条件可以帮助我们检测到这一点？这一次，我们可能会注意到文章页面的名称都包含字符串@code{article}。在实际项目中，您可能需要更有意义的测试条件。但是在本教程中，如果下一页的名称不包含@filepath{article}，我们将做的是隐藏下一页导航。正如我们之前所做的，我们将导航线包裹在 @racket[when/splice] 函数中：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊(define prev-page (previous here))
◊when/splice[prev-page]{The previous is 
<a href="◊|prev-page|">◊|prev-page|</a>.} 
◊when/splice[(regexp-match "article" (symbol->string (next here)))]{
The next is <a href="◊|(next here)|">◊|(next here)|</a>.}
</body>
</html>
}]


这一次，条件是 @racket[(regexp-match "article" (symbol->string (next here)))]。你知道为什么会是这个呢？你不知道。这就是为什么这是个教程。在此不做赘述，@racket[regexp-match] 函数是 Racket 的正则表达式匹配器。如果第一个字符串（在本例中，@racket["article"]）在第二个字符串（在本例中，我们通过将 @racket[(next here)] 包裹在 @racket[symbol->string] 中将其转换为一个字符串）中找到，它将返回 true 。


即使刚才有些程序上的东西让你摸不着头脑，也要放松，把代码粘贴到你的模板上。当你刷新@filepath{carticle.html}时，你会看到下一页的链接已经消失。所以，现在我们的模板可以让我们在文章的各个页面之间进行导航，而且条件式可以正确处理末页。

@subsection{制作页面树文件}

我不想在上一节中纠缠于编程的复杂性。为什么呢？因为额外的编程是必要的，只是因为我们依赖自动的 pagetree 而使自己的生活有些困难。

解决这个问题的更好方法是通过制作一个页面树文件来完全避免它。

Pagetree 文件的语法和状态与其他 Pollen 源文件不同，因此它们是使用自己的 Pollen 方言编译的。要调用此方言，您只需在文件开头输入 @tt{#lang pollen} 并以@filepath{ptree} 扩展名命名文件，例如@filepath{my-project.ptree}。虽然您可以在项目中拥有任意数量的页面树，但 Pollen 将首先查找名为 @filepath{index.ptree} 的页面树。

@margin-note{pagetree 文件只有一个文件扩展名，因为与其他 Pollen 源文件不同，它永远不会转换为输出文件。}

所以让我们创建一个@filepath{index.ptree} 文件。在最简单的情况下，页面树文件可以只是按预期顺序排列的文件列表。在 DrRacket 中，在您的项目目录中创建一个新文件，如下所示：

@fileblock["index.ptree"
@codeblock{
#lang pollen

carticle.html
article.html
barticle.html
}]

现在运行该文件。结果将是：

@repl-output{'(pagetree-root carticle.html article.html barticle.html)}

很无聊，我知道。但在幕后，Pollen 的 pagetree 编译器正在确保您的树是有效的（例如，没有重复或格式错误的名称）。今天很无聊，但是当你有一个又长又复杂的页面树时，你会很感激的。

请注意，此页面树中的名称是 @italic{output} 文件的名称，而不是源文件的名称。这是故意的。您和 Pollen 都不必关心哪些文件是静态文件还是动态文件。就语法错误来说，下一个页面树不算错误的——页面树编译器不会抱怨——但在非你所愿的意义上来说，它是错误的，因为它指的是源名称而不是输出名称：

@fileblock["bad-index.ptree"
@codeblock{
#lang pollen

carticle.html.pmd
article.html.pmd
barticle.html.pmd
}]

您可能还注意到文件的顺序与它们在自动页面树中的顺序不同：首先是@filepath{carticle.html}，然后是@filepath{article.html}，然后是@filepath{barticle.html}。这也是故意的，所以我们可以看到不同顺序的页面树会发生什么。

pagetree 不像其他源文件那样经常变化，所以作为一种性能优化，项目服务器在决定是否刷新文件时忽略了它们。因此，在更新一个 pagetree 之后，你必须手动重置你的项目。转到你的终端窗口，用 @onscreen{ctrl+C} 停止项目服务器。

然后清除 Pollen 的渲染页面缓存：

@terminal{
> raco pollen reset}

并重启项目服务器：

@terminal{
> raco pollen start}

现在再次单击@filepath{carticle.html}。您会注意到导航链接是不同的。您不会看到上一页链接——因为@filepath{carticle.html} 现在是页面树中的第一页——而下一页将显示为@filepath{article.html}。点击@filepath{article.html}，您将看到导航同样更新。点击@filepath{barticle.html}，你会看到...

砰！错误页面显示 

@errorblock{symbol->string: contract violation
  expected: symbol?
  given: #f}

发生了什么？我们切换到使用我们自己的 pagetree 文件，但我们没有更新模板中的条件。一旦我们到达@filepath{barticle.html}，@racket[(next here)] 的值为 false (@racket[#f])。但是模板中的 @racket[(symbol->string (next here))] 命令需要一个符号作为输入。因此错误。

所以让我们回去解决这个问题。因为我们的页面树中不再有多余的文件，我们可以更改模板中的第二个条件，使其与第一个条件相同：

@fileblock["template.html.p"
@codeblock[#:keep-lang-line? #f]{
#lang pollen
<html>
<head>
  <meta charset="UTF-8">
  <title>◊(select 'h1 doc), by MB</title>
  <link rel="stylesheet" type="text/css" href="styles.css" />
</head>
<body>◊(->html doc)
The current page is called ◊|here|.
◊(define prev-page (previous here))
◊when/splice[prev-page]{The previous is <a href="◊|prev-page|">◊|prev-page|</a>.} 
◊(define next-page (next here))
◊when/splice[next-page]{The next is <a href="◊|next-page|">◊|next-page|</a>.}
</body>
</html>
}]

刷新 @filepath{barticle.html} - 因为您正在更新模板，所以您不需要重新启动项目服务器 - 您会看到正确的结果。上一页链接转到 @filepath{article.html}，而下一页链接被隐藏。

@subsection{@tt{index.ptree} & the project server}

在我们结束本教程之前还有一件事。还记得项目服务器的仪表板位于@link-tt{http://localhost:8080/index.ptree} 吗？默认情况下，项目服务器将根据字母目录列表生成页面树。 

但是，如果您将自己的 @filepath{index.ptree} 文件放在该目录中，项目服务器会将其用于仪表板。事实上，现在访问@link-tt{http://localhost:8080/index.ptree} 你就会明白我的意思了。与您创建的@filepath{index.ptree} 一致，您现在将看到@filepath{carticle.html}、@filepath{article.html} 和@filepath{barticle.html}，但看不到@filepath{template。 html.p} 或 @filepath{styles.css} （即使它们仍在项目目录中）。


@section{第二个教程完成}

那是一个很大的教程。我赞扬你的坚韧和耐心。但是你向前迈出了一大步。尽管有一些愚蠢的例子，但您现在知道在 Pollen 中使用 Markdown 创作模式制作多页文章（甚至书籍）所需的一切。如果这就是您使用 Pollen 的全部目的，那么它将比普通的 Markdown 有很大的改进。

但还有更多的东西。我们甚至还没有进入 Pollen 可以实现的更复杂的自动化，也没有涉及 Pollen 自己的标记语言。我们将在 @seclink["third-tutorial"]{third tutorial} 中介绍它。


