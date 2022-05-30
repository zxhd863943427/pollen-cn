#lang scribble/manual

@title[#:style 'toc]{花粉:这本书是一个程序}

@author[(author+email "Matthew Butterick" "mb@mbtype.com")]

@defmodulelang[pollen]

Pollen 是一个出版系统,可帮助作者制作实用且美观的数字图书。

我创建 Pollen 是为了制作基于网络的书籍 @link["http://practicaltypography.com"]{@italic{Practical Typography}}, @link["http://typographyforlawyers.com"]{@italic{Typography for Lawyers}}, 和 @link["http://beautifulracket.com"]{@italic{Beautiful Racket}}。 来吧，去看看。它们是否比您遇到的上一本电子书更好？当然，他们肯定是。你希望你的下一本电子书也能这样工作吗？如果是这样，请继续阅读。

Pollen 的核心是一个论点：

@itemlist[#:style 'ordered

@item{数字图书应该是我们有过的最好的图书。到目前为止，他们还不太接近。}

@item{因为数字图书是软件，所以作者不应该将图书仅仅视为数据。 @bold{这本书就是一个程序。}}

@item{我们使数字图书比其前辈更好的方法是利用这种可编程性。}]

这就是花粉的目的。

并不是说您需要成为程序员才能开始使用 Pollen。相反，Pollen 语言是基于标记的，因此您可以自然地编写和编辑文本。但是，当您想要自动执行重复性任务、添加交叉引用或从其他来源提取数据时，您可以从文本中访问完整的编程语言。

这个编程语言就是 @link["http://racket-lang.org"]{Racket}。我之所以选择 Racket，是因为它具有一些使 Pollen 成为可能的独特功能。因此，如果您对它不熟悉，请不要惊慌。这对我来说很陌生。一旦你看到你可以用球拍和花粉做什么，你可能会被说服。我曾是。

或者，如果您能找到更好的数字出版工具，请使用它。但我永远不会回到以前的工作方式了。


@local-table-of-contents[]


@include-section["installation.scrbl"]
@include-section["quick.scrbl"]
@include-section["story.scrbl"]
@include-section["big-picture.scrbl"]
@include-section["tutorial-first.scrbl"]
@include-section["tutorial-second.scrbl"]
@include-section["tutorial-third.scrbl"]
@include-section["tutorial-fourth.scrbl"]
@include-section["tutorial-mini.scrbl"]
@include-section["raco.scrbl"]
@include-section["formats.scrbl"]
@include-section["command.scrbl"]
@include-section["programming-pollen.scrbl"]
@include-section["module-reference.scrbl"]
@include-section["unstable-module-reference.scrbl"]
@include-section["acknowledgments.scrbl"]
@include-section["license.scrbl"]
@include-section["version-history.scrbl"]

@index-section[]
