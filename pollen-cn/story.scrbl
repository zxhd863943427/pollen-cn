#lang scribble/manual

@title{背景故事}

我创建 pollen 是为了克服我在现有 web 发布工具中反复遇到的限制和挫折。

如果您同意我对这些问题的描述，那么您可能会喜欢 Pollen 提供的解决方案。如果没有，那么你可能不会。

@section{Web 开发及对其的不满}

1994 年，在网络发明后不久，我制作了我的第一个网页。我打开了我的文本编辑器 (当时是 @link["http://www.barebones.com/products/bbedit/"]{BBEdit}), 输入@code{<html><body>Hello world</body></html>}，然后将其加载到 @link["http://en.wikipedia.org/wiki/Mosaic_(web_browser)"]{Mosaic}。一百万其他书呆子也是这么做的。

如果你当时不在，你不会错过太多。网络的一切都很可怕：网络浏览器、运行浏览器的计算机、为浏览器提供数据的拨号连接，当然还有 HTML 本身。那时，桌面软件体验已经很流畅和精致了。相比之下，使用网络就像把石头撞在一起。

这不再是真的。网络现在已有 20 多年的历史。在此期间，网络的大部分内容都得到了显着改善——例如，连接速度更快，浏览器更复杂，屏幕像素更多。

但是有一部分没有太大改进：我们制作网页的方式。多年来，承诺简化 Web 开发的工具来了又去——从 @link["http://www.macobserver.com/reviews/pagemill2.shtml"]{PageMill} 到 @link["http:// www.adobe.com/products/dreamweaver.html"]{Dreamweaver} 到 @link["http://www.squarespace.com"]{Squarespace}。与此同时，严肃的网络达人仍然忠于最初编写 HTML 的强大工具：简陋的文本编辑器。

从某种意义上说，这是有道理的。网页主要由基于文本的数据（HTML、CSS、JavaScript 等）组成，处理这些数据的最简单方法是使用文本编辑器。虽然 HTML 和 CSS 不是编程语言——你甚至无法计算 1 + 1——但它们具有语义和逻辑结构，通过将它们编辑为文本最容易表达。此外，基于文本的编辑使调试和性能改进更容易。

但基于文本的编辑也受到限制。尽管网页的基本描述在理论上是人类可读的，但它已经过优化，可以被其他软件（即网络浏览器）读取。尤其是 HTML 很冗长且容易输入错误。管理所有样板文件，比如用@code{<p>...</p>} 包围每个段落，是不是很枯燥？是的。

由于这些原因，大部分 Web 开发都应该使用 @italic{abstraction} 和 @italic{automation}。抽象意味着将重复的、复杂的模式合并为更简单、更通用的形式。自动化意味着避免手工生成输出文件所固有的乏味和潜在的错误。换句话说，它开始看起来很像编程。

@section{更好的主意：编程模型}

在接受 HTML 教育的同时，我还尝试了各种编程语言——C、C++、Perl、Java、PHP、JavaScript、Python。与 HTML 不同，编程语言擅长抽象和自动化。这似乎是 Web 开发的明显方向。

文本编辑模型与编程模型有何区别？这是对输出的直接与间接操纵的问题。文本编辑模型将 HTML 视为可以直接使用文本编辑器编写的内容。而编程模型将 HTML（或任何输出）视为编译一组用编程语言编写的源文件的结果。通过编程语言间接工作的成本被抽象和自动化的好处所抵消。

在早期的网络上，文本编辑模型非常精确和快速。在小型项目中，它运行良好。但随着项目的增长，文本编辑模式将失去动力。我不是唯一一个注意到的人。在那几百万个书呆子手工制作了他们的第一个网页后不久，他们中的许多人开始着手设计将编程模型应用于 Web 开发的方法。

@section{``现在你有两个问题''}

随之而来的是源源不断的产品、框架、工具和内容管理系统，它们声称将编程模型带入 Web 开发。有些人比其他人更好。但它们都没有取代文本编辑器成为 Web 开发人员的首选工具。而且它们都无法与您从任何合理的编程语言中获得的功能和灵活性相提并论。

为什么不？这些工具总是承诺在解决 Web 开发问题方面取得巨大的飞跃。然而，在实践中，他们只是重新分配了痛苦。

例如，今天可以为 @link["http://haml.info"]{HTML}、@link["http://sass-lang.com"]{CSS} 和 @link["http://coffeescript.org"]{JavaScript} 找到多个预处理器链接。但它们都是独立的工具，具有不同的语法和功能。祝你好运，希望你能找到一个可以同时处理所有 Web 文件的预处理器。

这种思维——从边缘向内，而不是从中心向外——一直是网络发布工具的主题性失败。每个人都像谚语中的盲人，只摸到了@link["http://www.jainworld.com/literature/story25.htm"]{大象的一部分}——解决了一个特定的问题，却忽略了更广泛的背景。

同样，即使是表面上基于通用编程语言的网络发布系统，例如 @link["https://wordpress.org"]{WordPress} 或 @link["https://www.djangoproject.com"]{ Django} — 也有反复出现的缺陷：

@itemlist[

@item{@bold{没有 HTML 的原生数据结构。} 任何编程模型的核心都是数据结构。良好的数据结构使处理变得容易；坏的使事情变得困难。尽管 HTML 具有 @link["http://www.w3.org/TR/html401/struct/global.html"]{良好的文档} 格式，但很少在编程系统中用原生、直观的数据结构来处理它。相反，它要么被视为一个字符串（错误）、一棵树（也是错误的），要么被视为一些神奇的解析对象（完全是错误的）。这使得在编程环境中使用 HTML 变得不必要地困难。}

@item{@bold{强制分离代码、表示和内容。}这个原则经常被@link["http://alistapart.com/article/separationdilemma/"]{坚持}作为Web开发的理想。但这也违反直觉，因为一个 HTML 页面自然包含了这三者。如果你想将它们分开，你的工具应该允许你这样做。但如果你不这样做，你的工具也不应该强迫你。}

@item{@bold{受损的模板语言。} 似乎每一种编程语言都有至少 10 种用于 HTML 的模板系统，所有这些系统都要求你学习一种新的 "模板语言"，它提供了两种世界中最糟糕的情况：比底层语言更少的特性和不同的语法。}

@item{@bold{陡峭的学习曲线。} Web 程序员经常责备设计师不知道@link["http://elliotjaystocks.com/blog/web-designers-who-cant-code/"]{如何编码}。但是基于编程的网络开发工具往往有一个很高的初始学习曲线，要求你抛弃现有的工作流程。程序员建造了这些工具--程序员对它们更加得心应手并不奇怪。}

]

这些年来，我尝试了很多这样的工具。有些我喜欢。有些我不喜欢。然而，每当我还能用手工编辑HTML项目的时候，我总是会这样做。在试图说服当今的网络框架为我服务之后，用一些效率来换取控制权是一件很轻松的事情。


@section{重新思考数字图书的解决方案}

2008 年，我推出了一个名为 @link["http://typographyforlawyers.com"]{@italic{律师排版}} 的网站。起初，我把它设想成一本书。然后我想，“没有人会出版它”。因此，它变成了一个网站，我的目标是让它尽可能的像书一样。但是手工编辑是不够的。 

所以我使用了@link["http://wordpress.org"]{WordPress}。主要的工作变成了清除通常存在于博客模板中的所有废话。最后，它看起来比通常的 WordPress 网站更简单和干净。很大程度上是因为这个，人们@link["http://ma.tt/2010/04/typography-for-lawyers/"]{喜欢它}。

最终，一家出版商提出将其作为@link["http://typo.la/amzn"]{一本平装本} 发布，于 2010 年出版（第二版于 2015 年发布）。

后来不可避免地被要求将其制作成Kindle书。作为排版爱好者，我讨厌 Kindle。布局控制很粗糙，阅读体验也很粗糙。但我没有躲开。基本上，Kindle 书是一个使用 1995 年 HTML 制作的小网站。所以我在 Perl 中编写了一些工具来将我的书转换为 Kindle 格式，同时尽可能地保留格式和图像。

那时，我注意到我使用两组不同的工具将@italic{Typography for Lawyers} 两次转换为网络格式。在有人要求我第三次这样做之前，我开始思考如何为这本书创建源代码，以便将其呈现为不同的格式。

这就是 Pollen 项目的开始。

我用 Python 编写了 Pollen 的初始版本。我为源文件设计了一种简化的标记符号语言。该语言使用 @link["http://www.dabeaz.com/ply/"]{ply} (Python lex/yacc) 编译成 XML 风格的数据结构。使用@link["http://lxml.de/"]{LXML} 将这些结构解析为树。这些树与@link["http://chameleon.readthedocs.org/en/latest/"]{Chameleon} 中制作的模板相结合。这些模板通过@link["http://bottlepy.org/"]{Bottle} Web 服务器呈现和预览。

它能正常工作了吗？差不多吧。源代码进去了；网页出来了。但它也是复杂而脆弱的。此外，虽然有自动化，但源层还没有足够的抽象。我开始考虑如何添加源预处理器。

@section{进入 Racket}

我在研究适合 HTML/XML 处理的语言时遇到了 Racket。我意外地了解了 XML 和 Lisp 的 @link["http://www.defmacro.org/ramblings/lisp.html"]{secret kinship}：虽然 XML 不是功能齐全的编程语言，但它使用了Lisp 语法的变体。因此，Lisp 语言特别擅长处理 XMLish 结构。这很有趣。

在比较了一些 Lisp 和 Scheme 变体之后，@link["http://practicaltypography.com/why-racket-why-lisp.html"]{Racket 脱颖而出} 因为它有一个基于文本的方言，称为  @seclink["getting-started" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{Scribble}。 Scribble 可用于在文本内容中嵌入代码。那也很有趣。除此之外，这意味着 Scribble 可以用作 @seclink["text" #:doc '(lib "scribblings/scribble/scribble-pp.scrbl")]{通用预处理器}。所以我想我会看看我是否可以将它添加到 Pollen 中。

它成功了。事实上，我开始考虑是否可以重新实现 Racket 中 Pollen 的其他部分。然后我开始考虑在 Racket 中重新实现所有这些功能。

所以我做到了。

@section{什么是 Pollen ？}

Pollen 是一个建立在 Scribble 和 Racket 之上的发布系统。到目前为止，我已经为基于网络的书籍优化了 Pollen，因为这主要是我使用它的目的。但它也可以用于小型项目，以及像 @seclink["Adding_support_for_PDF_output"]{PDF} 这样的非 webby 项目。

作为一个出版系统，Pollen 包括：

@itemlist[

@item{@bold{一种编程语言。} Pollen 语言是 Scribble 的变体，具有针对不同类型的源文件量身定制的特定方言。您不需要使用编程功能来完成有用的工作，但它们在您需要时可用。}

@item{@bold{一组工具和库。} Pollen 可以生成任何格式的输出，但它对于 XML 和 HTML 等标记样式格式特别有用。}

@item{@bold{一个开发环境。} Pollen 可与 DrRacket IDE 配合使用。它还包括一个项目 Web 服务器，因此您可以动态预览和修改您的出版物。}


]

Pollen 解决了我在使用其他工具时遇到的不足：

@itemlist[

@item{@bold{是的，我们有一个用于HTML的本地数据结构。} Racket 将 HTML 结构表示为 @secref["X-expressions"]，它是标准 Racket 数据结构的变体，称为 @italic{S-expressions}。换句话说，不仅有 HTML 的本地数据结构表示，而且它的表示方式与该语言中的其他所有内容相同。}

@item{@bold{代码、演示和内容的灵活混合。} Pollen 是一种基于文本的语言。所以 Pollen 源文件可能根本没有代码。但作为 Scribble & Racket 的方言，如果你想将代码与内容混合，你当然可以。}

@item{@bold{没有模板语言。} 这不是必需的，因为在每个 Pollen 文件中，您都可以使用整个 Racket 语言和所有常用的 Racket 语法。}

@item{@bold{学习曲线浅。} 您无需进行大量设置和配置即可开始使用 Pollen 进行有用的工作。程序员和非程序员可以轻松协作。是的，我承认，如果你打算认真起来，你需要学习一些 Racket 。我想你不会后悔的。}


]

